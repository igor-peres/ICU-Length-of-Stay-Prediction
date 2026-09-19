#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
SLOS — Reprodução do retreinamento no AmsterdamUMCdb (extração de 24h)
======================================================================
Pipeline completo, dos dados brutos aos números e figuras reportados no artigo
(Expert Systems with Applications). Reproduz a metodologia do pacote SLOS /
Peres et al. (2022) em Python, de forma determinística (semente fixa).

Entrada
-------
CSV da extração de 24h (nível-paciente), p.ex. `umcdb_24h_patient_level.csv`,
em que TODAS as variáveis já estão restritas às primeiras 24h de cada admissão
(cada tabela-fonte foi filtrada por timestamp até admissão+24h).

Saídas
------
- métricas nível-paciente (RMSE, MAE, R2) no conjunto de teste
- métricas nível-unidade (R2), SLOS agregado, mediana e IIQ por especialidade
- funnel (ems, SRU indireto) com unidades fora dos limites
- figuras: fig_importance_nl.png, fig_calibration_nl.png, fig_efficiency_panel_nl.png

Dependências: numpy, pandas, scikit-learn, matplotlib.
Uso: python slos_umcdb_reproduction.py --csv caminho/para/umcdb_24h_patient_level.csv
"""
import argparse, json
import numpy as np, pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.experimental import enable_iterative_imputer  # noqa
from sklearn.impute import IterativeImputer
from sklearn.linear_model import BayesianRidge, Ridge
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import make_pipeline
from sklearn.ensemble import RandomForestRegressor, StackingRegressor
from sklearn.metrics import r2_score, mean_squared_error, mean_absolute_error

# ------------------------------------------------------------------ parâmetros
SEED            = 42
LOS_MIN_H       = 6          # exclusão de Peres 2022: LoS na UTI < 6h
LOS_CAP_D       = 21         # desfecho truncado em 21 dias
TEST_SIZE       = 0.20       # split 80/20
MISSING_CUTOFF  = 0.30       # corte de dados faltantes (medido no treino)
NZV_FREQ        = 19         # near-zero variance: freqRatio > 19
NZV_PCTUNIQUE   = 10         # e  % único < 10
CORR_NUM        = 0.75       # redundância numérica (Pearson) — Peres 2022
CRAMER_CAT      = 0.50       # redundância categórica (Cramér's V) — Peres 2022
CORR_OUTCOME    = 0.75       # salvaguarda anti-vazamento: |r| com o desfecho
N_FEATURES      = 21         # nº de features do RFE (platô da curva de R2)
UNIT_MIN_N      = 20         # nº mínimo de admissões (teste) p/ benchmark no funnel
np.random.seed(SEED)

# ------------------------------------------------------------------ utilidades
def load_and_exclude(csv_path):
    """Carrega a base de 24h e aplica os critérios de exclusão de pacientes."""
    df = pd.read_csv(csv_path, low_memory=False)
    n0 = len(df)
    # idade > 16: UMCdb é adulto (todas as faixas >= 18) -> nenhum removido
    # LoS na UTI < 6h  (icu_los_h tem apenas cap superior, então <6h é preservado)
    df = df[df["icu_los_h"] >= LOS_MIN_H].copy()
    # LoS hospitalar prévio > 60 dias: não aplicável (UMCdb não registra)
    print(f"[coorte] bruto={n0}  apos <{LOS_MIN_H}h={len(df)}  pacientes={df['patientid'].nunique()}")
    return df.reset_index(drop=True)

def predictor_columns(cols):
    """Preditores = tudo menos ids, unidade, desfecho e variáveis pós-24h/desfecho."""
    ids     = {"admissionid", "patientid"}
    unit    = {"specialty"}
    outcome = {"icu_los_h", "icu_los_d"}
    def is_leak(c):
        lc = c.lower()
        return (c in {"destination", "dateofdeath"}
                or any(t in lc for t in
                       ["ontslag", "verkeerde_bed", "overled", "overlijd", "donatie"]))
    drop = ids | unit | outcome | {c for c in cols if is_leak(c)}
    return [c for c in cols if c not in drop]

def nzv_keep(Dtr, cols):
    """NZV estilo caret (freqRatio>19 E %único<10), vetorizado: só calcula
    value_counts para colunas de baixa cardinalidade (as demais nunca são NZV)."""
    n = len(Dtr); nun = Dtr[cols].nunique(); pu = 100 * nun / n
    keep = []
    for c in cols:
        if pu[c] >= NZV_PCTUNIQUE:
            keep.append(c); continue
        vc = Dtr[c].value_counts()
        fr = vc.iloc[0] / vc.iloc[1] if len(vc) >= 2 else np.inf
        if not (fr > NZV_FREQ and pu[c] < NZV_PCTUNIQUE):
            keep.append(c)
    return keep

def find_correlation(corr_abs, thr):
    """Remoção pareada (caret::findCorrelation): remove a variável de maior
    correlação média enquanto houver par acima do limiar."""
    M = corr_abs.copy(); np.fill_diagonal(M, 0.0)
    names = list(range(M.shape[0])); dropped = set()
    while True:
        m = M.copy()
        for i in dropped: m[i, :] = 0; m[:, i] = 0
        if m.max() <= thr: break
        i, j = np.unravel_index(m.argmax(), m.shape)
        keep = [k for k in names if k not in dropped]
        dropped.add(i if m[i, keep].mean() >= m[j, keep].mean() else j)
    return dropped

def cramers_v(a, b, na, nb):
    cont = np.bincount(a * nb + b, minlength=na * nb).reshape(na, nb).astype(float)
    n = cont.sum()
    if n == 0: return 0.0
    rs = cont.sum(1, keepdims=True); cs = cont.sum(0, keepdims=True)
    exp = rs @ cs / n; mask = exp > 0
    chi2 = ((cont[mask] - exp[mask]) ** 2 / exp[mask]).sum()
    phi2 = chi2 / n; r, k = cont.shape
    phi2c = max(0, phi2 - (k - 1) * (r - 1) / (n - 1))
    kc = k - (k - 1) ** 2 / (n - 1); rc = r - (r - 1) ** 2 / (n - 1)
    den = min(kc - 1, rc - 1)
    return np.sqrt(phi2c / den) if den > 0 else 0.0

VAR_LABELS = {
    "drug_Noradrenaline (Norepinefrine)": "Vasopressor (noradrenaline)",
    "porder_Opdr. Lijnen/Catheter/Drains": "Line/catheter/drain order",
    "location": "ICU care level", "num_Adem_Frequentie_Set": "Set respiratory rate",
    "num_PEEP_Set": "PEEP (set)", "num_PC_boven_PEEP_Set": "Pressure control above PEEP",
    "num_WOBv": "Work of breathing", "num_pH_bloed": "Arterial pH",
    "num_UrineCAD": "Urine output", "drug_Macrogol (Movicolon)X": "Laxative (macrogol)",
    "num_Piek_druk": "Peak airway pressure", "num_PS_boven_PEEP_Set": "Pressure support above PEEP",
    "num_Mean_luchtweg_druk": "Mean airway pressure", "num_Cdyn": "Dynamic compliance (Cdyn)",
    "num_Ademfrequentie_Monitor": "Respiratory rate (monitor)",
    "drug_Midazolam (Dormicum)": "Sedation (midazolam)",
    "drug_Nutrison Protein Plus": "Enteral nutrition", "num_Saturatie_Monitor": "Oxygen saturation",
}
def _label(c):
    for k, v in VAR_LABELS.items():
        if c.startswith(k):
            return v
    return c

def make_figures(model, X, top, tr, y, gb, theta, phi, over, outdir):
    import matplotlib; matplotlib.use("Agg"); import matplotlib.pyplot as plt
    from statistics import NormalDist
    plt.rcParams.update({"font.size": 11, "axes.spines.top": False,
                         "axes.spines.right": False, "figure.dpi": 160})
    ACC, GREY, RED = "#2C6E8F", "#9aa0a6", "#B4433A"
    # importância (RF nas 21 features, agregada a nível de variável)
    rf = RandomForestRegressor(n_estimators=200, max_depth=14, min_samples_leaf=5,
                               max_features=0.34, n_jobs=-1, random_state=SEED)
    rf.fit(X[top].values[tr], y[tr])
    agg = {}
    for c, im in zip(top, rf.feature_importances_):
        agg[_label(c)] = agg.get(_label(c), 0) + im
    s = pd.Series(agg).sort_values(); s = 100 * s / s.sum()
    fig, ax = plt.subplots(figsize=(7.2, 5.2)); ax.barh(range(len(s)), s.values, color=ACC)
    ax.set_yticks(range(len(s))); ax.set_yticklabels(s.index); ax.set_xlabel("Importance (% of total)")
    for i, v in enumerate(s.values): ax.text(v + 0.3, i, f"{v:.1f}", va="center", fontsize=9, color="#444")
    ax.set_xlim(0, max(s.values) * 1.15); plt.tight_layout()
    plt.savefig(f"{outdir}/fig_importance_nl.png", bbox_inches="tight"); plt.close()
    # calibração
    fig, ax = plt.subplots(figsize=(6, 5.6)); mx = max(gb["obs"].max(), gb["pred"].max()) * 1.05
    ax.plot([0, mx], [0, mx], "--", color=GREY, lw=1)
    ax.scatter(gb["pred"], gb["obs"], s=np.sqrt(gb["n"]) * 4, color=ACC, alpha=.8, edgecolor="white", linewidth=.6)
    ax.set_xlabel("Summed predicted LoS per unit (days)"); ax.set_ylabel("Summed observed LoS per unit (days)")
    from sklearn.metrics import r2_score as _r2
    ax.set_xlim(0, mx); ax.set_ylim(0, mx)
    ax.text(.05, .92, f"unit-level R$^2$ = {_r2(gb['obs'], gb['pred']):.2f}", transform=ax.transAxes)
    plt.tight_layout(); plt.savefig(f"{outdir}/fig_calibration_nl.png", bbox_inches="tight"); plt.close()
    # funnel
    U = len(gb); rho = np.linspace(gb["pred"].min() * .8, gb["pred"].max() * 1.05, 200)
    def lim(p):
        zp = NormalDist().inv_cdf(p); sd = np.sqrt(theta * (phi if over else 1) / rho)
        return theta - zp * sd, theta + zp * sd
    lo95, hi95 = lim(.975); lo99, hi99 = lim(.995)
    fig, ax = plt.subplots(figsize=(8, 5)); ax.axhline(theta, color=GREY, lw=1, label=f"pooled SLOS = {theta:.3f}")
    ax.plot(rho, hi95, color=ACC, lw=1, ls="--", label="95% limits"); ax.plot(rho, lo95, color=ACC, lw=1, ls="--")
    ax.plot(rho, hi99, color=ACC, lw=1, ls=":", label="99% limits"); ax.plot(rho, lo99, color=ACC, lw=1, ls=":")
    outm = (gb["slos"] > np.interp(gb["pred"], rho, hi95)) | (gb["slos"] < np.interp(gb["pred"], rho, lo95))
    ax.scatter(gb["pred"][~outm], gb["slos"][~outm], s=40, color="#555", alpha=.8, zorder=3)
    ax.scatter(gb["pred"][outm], gb["slos"][outm], s=70, color=RED, zorder=4, label="outside 95%")
    for _, r in gb[outm].iterrows():
        ax.annotate(r["u"], (r["pred"], r["slos"]), fontsize=8, color=RED, xytext=(5, 5), textcoords="offset points")
    ax.set_xlabel("Expected (predicted) total LoS per unit (days)"); ax.set_ylabel("SLOS")
    ax.legend(fontsize=8, frameon=False, loc="upper right"); plt.tight_layout()
    plt.savefig(f"{outdir}/fig_efficiency_panel_nl.png", bbox_inches="tight"); plt.close()

# ------------------------------------------------------------------ pipeline
def run(csv_path, outdir="."):
    df = load_and_exclude(csv_path)
    preds = predictor_columns(df.columns)
    y = df["icu_los_d"].astype(float).values
    unit = df["specialty"].fillna("Unknown").values

    # split 80/20 estratificado por unidade (especialidade)
    idx = np.arange(len(df))
    tr, te = train_test_split(idx, test_size=TEST_SIZE, random_state=SEED, stratify=unit)
    print(f"[split] treino={len(tr)}  teste={len(te)}")

    num0 = [c for c in preds if df[c].dtype.kind in "fi"]
    cat0 = [c for c in preds if df[c].dtype.kind not in "fi"]

    # (1) missing 30% (medido no treino)
    miss = df[preds].isna().iloc[tr].mean()
    kept = [c for c in preds if miss[c] <= MISSING_CUTOFF]
    print(f"[missing {int(MISSING_CUTOFF*100)}%] {len(preds)} -> {len(kept)}")

    # imputação: MICE (num contínuas) + moda (categóricas); passthru = binárias/num sem missing
    num_cont = [c for c in kept if c.startswith("num_")]
    passthru = [c for c in kept if c in num0 and c not in num_cont]
    cat      = [c for c in kept if c in cat0]
    mice = IterativeImputer(estimator=BayesianRidge(), max_iter=3, n_nearest_features=12,
                            sample_posterior=False, random_state=SEED)
    mice.fit(df[num_cont].iloc[tr].astype("float32"))
    Xi = pd.DataFrame(mice.transform(df[num_cont].astype("float32")), columns=num_cont, index=df.index)
    # medianas/modas do treino calculadas de forma vetorizada (sem copiar o frame por coluna)
    Pdf = df[passthru].fillna(df[passthru].iloc[tr].median()) if passthru else pd.DataFrame(index=df.index)
    if cat:
        modes = df[cat].iloc[tr].mode()
        modes = modes.iloc[0] if len(modes) else pd.Series({c: "NA" for c in cat})
        Cdf = df[cat].fillna(modes).astype(str)
    else:
        Cdf = pd.DataFrame(index=df.index)
    data = pd.concat([Xi, Pdf, Cdf], axis=1)
    data_tr = data.iloc[tr]                      # subconjunto de treino, calculado uma vez

    # (2) NZV (treino)
    kept2 = nzv_keep(data_tr, kept)
    print(f"[nzv] {len(kept)} -> {len(kept2)}")

    # (3) correlação: Pearson>0.75 (numéricas contínuas), Cramér's V>0.5 (categóricas E
    #     binárias de presença — presença/ausência é categórica), e anti-vazamento |r|>0.75
    cont = [c for c in kept2 if c.startswith("num_") or c == "admissioncount"]
    catg = [c for c in kept2 if c not in cont]
    Cn = data_tr[cont].corr().abs().values
    red_num = {cont[i] for i in find_correlation(Cn, CORR_NUM)}
    cont2 = [c for c in cont if c not in red_num]
    leak = [c for c in cont2 if abs(np.corrcoef(data_tr[c].values, y[tr])[0, 1]) > CORR_OUTCOME]
    cont3 = [c for c in cont2 if c not in leak]
    codes = {c: pd.factorize(data_tr[c])[0].astype(np.int64) for c in catg}
    nlev = {c: int(codes[c].max() + 1) for c in catg}
    V = np.zeros((len(catg), len(catg)))
    for i in range(len(catg)):
        for j in range(i + 1, len(catg)):
            v = cramers_v(codes[catg[i]], codes[catg[j]], nlev[catg[i]], nlev[catg[j]])
            V[i, j] = V[j, i] = v
    red_cat = {catg[i] for i in find_correlation(V, CRAMER_CAT)}
    catg2 = [c for c in catg if c not in red_cat]
    feats = cont3 + catg2
    print(f"[corr] Pearson>{CORR_NUM}: -{len(red_num)} | outcome>{CORR_OUTCOME}: -{len(leak)} | "
          f"Cramér>{CRAMER_CAT}: -{len(red_cat)} -> {len(feats)}")

    # one-hot p/ modelagem
    Xc = pd.get_dummies(data[[c for c in feats if c in catg2]].astype(str),
                        prefix=[c for c in feats if c in catg2])
    X = pd.concat([data[[c for c in feats if c in cont3]].astype("float32"),
                   Xc.astype("float32")], axis=1)

    # (4) RFE: ranking por importância RF, seleciona top-N (platô da curva)
    rf_rank = RandomForestRegressor(n_estimators=120, max_depth=14, min_samples_leaf=5,
                                    max_features=0.34, n_jobs=-1, random_state=SEED)
    rf_rank.fit(X.values[tr], y[tr])
    order = np.argsort(rf_rank.feature_importances_)[::-1]
    top = [X.columns[i] for i in order[:N_FEATURES]]
    print(f"[rfe] top-{N_FEATURES} features selecionadas")

    # (5) modelo: stacking Ridge + RF, meta-learner RF, CV 5-fold
    base = [("ridge", make_pipeline(StandardScaler(), Ridge(alpha=1.0))),
            ("rf", RandomForestRegressor(n_estimators=200, max_depth=14, min_samples_leaf=5,
                                         max_features=0.34, n_jobs=-1, random_state=SEED))]
    model = StackingRegressor(
        estimators=base,
        final_estimator=RandomForestRegressor(n_estimators=100, max_depth=6, min_samples_leaf=10,
                                              n_jobs=-1, random_state=SEED),
        cv=5, n_jobs=-1)
    Xtr, Xte = X[top].values[tr], X[top].values[te]
    model.fit(Xtr, y[tr])
    pred = np.clip(model.predict(Xte), 0, LOS_CAP_D)

    rmse = float(np.sqrt(mean_squared_error(y[te], pred)))
    mae  = float(mean_absolute_error(y[te], pred))
    r2   = float(r2_score(y[te], pred))
    print(f"\n[NÍVEL PACIENTE / teste] RMSE={rmse:.2f}  MAE={mae:.2f}  R2={r2:.3f}")

    # (6) SLOS por especialidade + funnel (ems, SRU indireto)
    g = (pd.DataFrame({"u": unit[te], "obs": y[te], "pred": pred})
         .groupby("u").agg(n=("obs", "size"), obs=("obs", "sum"), pred=("pred", "sum")).reset_index())
    g["slos"] = g["obs"] / g["pred"]
    theta = g["obs"].sum() / g["pred"].sum()
    gb = g[(g["n"] >= UNIT_MIN_N) & (g["u"] != "Unknown")].copy()
    r2u = float(r2_score(gb["obs"], gb["pred"]))
    print(f"[NÍVEL UNIDADE / teste] unidades={len(g)} benchmarkaveis(n>={UNIT_MIN_N})={len(gb)}  R2u={r2u:.3f}")
    print(f"[SLOS] pooled={theta:.3f}  mediana={gb['slos'].median():.3f}  "
          f"Q1={gb['slos'].quantile(.25):.3f}  Q3={gb['slos'].quantile(.75):.3f}")

    from statistics import NormalDist
    U = len(gb); z = (gb["slos"].values - theta) * np.sqrt(gb["pred"].values / theta)
    phi = np.clip(z ** 2, *np.percentile(z ** 2, [10, 90])).mean()
    over = phi > (1 + 2 * np.sqrt(2 / U))
    def limit(rho, p):
        zp = NormalDist().inv_cdf(p); s = np.sqrt(theta * (phi if over else 1.0) / rho)
        return theta - zp * s, theta + zp * s
    out = [r["u"] for _, r in gb.iterrows()
           if r["slos"] > limit(r["pred"], 0.975)[1] or r["slos"] < limit(r["pred"], 0.975)[0]]
    print(f"[FUNNEL] phi={phi:.2f} overdisp={over} | fora do 95%: {out}")

    # ---------------------------------------------------------- figuras
    try:
        make_figures(model, X, top, tr, y, gb, theta, phi, over, outdir)
        print("[figuras] fig_importance_nl.png, fig_calibration_nl.png, fig_efficiency_panel_nl.png")
    except Exception as e:
        print("[figuras] pulado:", e)

    json.dump({"rmse": rmse, "mae": mae, "r2": r2, "r2_unit": r2u, "slos_pooled": theta,
               "slos_median": float(gb["slos"].median()),
               "slos_q1": float(gb["slos"].quantile(.25)), "slos_q3": float(gb["slos"].quantile(.75)),
               "n_units": int(U), "units_outside_95": out, "features": top},
              open(f"{outdir}/slos_umcdb_results.json", "w"), indent=2, ensure_ascii=False)
    print(f"\n[ok] resultados salvos em {outdir}/slos_umcdb_results.json")

if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--csv", default="umcdb_24h_patient_level.csv")
    ap.add_argument("--outdir", default=".")
    a = ap.parse_args()
    run(a.csv, a.outdir)
