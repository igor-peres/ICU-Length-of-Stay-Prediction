#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Gera um SAMPLE SINTÉTICO no formato da extração 24h do AmsterdamUMCdb, para
replicação do pipeline SLOS por terceiros (revisores) sem acesso ao banco real
(restrito). NENHUMA linha real é copiada: cada coluna é reamostrada
independentemente da sua distribuição empírica (marginais + taxa de missing),
e o desfecho (LoS) é sintetizado por um modelo simples sobre variáveis de
intensidade de cuidado — sem identificadores, não reconstruível a pacientes.

Uso:
  python make_synthetic_umcdb.py --real umcdb_24h_patient_level.csv \
         --out umcdb_synthetic_sample.csv --n 1000 --seed 42
"""
import argparse, numpy as np, pandas as pd

# 15 variáveis clínicas selecionadas (colunas-fonte) — sempre incluídas
SELECTED = [
    "location", "drug_Noradrenaline (Norepinefrine)", "porder_Opdr. Lijnen/Catheter/Drains",
    "drug_Midazolam (Dormicum)", "drug_Nutrison Protein Plus", "drug_Macrogol (Movicolon)X",
    "num_Mean_luchtweg_druk_max", "num_Piek_druk_max", "num_PEEP_Set_max",
    "num_PC_boven_PEEP_Set_max", "num_PC_boven_PEEP_Set_n", "num_Adem_Frequentie_Set_max",
    "num_WOBv_mean", "num_pH_bloed_mean", "num_Cdyn_n", "num_UrineCAD_n",
]
ID_DROP = {"admissionid", "patientid", "admissioncount", "dateofdeath", "destination",
           "icu_los_h", "icu_los_d", "admittedat"}

def pick_columns(all_cols, n_extra, rng):
    pool = [c for c in all_cols if c not in ID_DROP and c not in SELECTED
            and c not in ("icu_los_d", "specialty")]
    extra = list(rng.choice(pool, size=min(n_extra, len(pool)), replace=False))
    cols = ["specialty"] + [c for c in SELECTED if c in all_cols] + extra
    return list(dict.fromkeys(cols))

def resample_col(series, n, rng):
    """Reamostra n valores da distribuição empírica (inclui NaN na taxa real)."""
    vals = series.to_numpy()
    idx = rng.integers(0, len(vals), size=n)
    return vals[idx]

def synth_los(df, rng):
    """LoS sintético (dias, truncado [0.25,21]) com sinal leve nas variáveis de suporte."""
    def z(col, default):
        if col not in df: return np.full(len(df), 0.0)
        x = pd.to_numeric(df[col], errors="coerce").to_numpy(dtype=float)
        m = np.nanmedian(x) if np.isfinite(np.nanmedian(x)) else default
        x = np.where(np.isfinite(x), x, m); s = np.nanstd(x) or 1.0
        return (x - np.nanmean(x)) / s
    def flag(col):
        if col not in df: return np.zeros(len(df))
        return (pd.to_numeric(df[col], errors="coerce").fillna(0).to_numpy() > 0).astype(float)
    base = (2.5
            + 3.2 * flag("drug_Noradrenaline (Norepinefrine)")
            + 1.8 * flag("porder_Opdr. Lijnen/Catheter/Drains")
            + 1.2 * flag("drug_Midazolam (Dormicum)")
            + 0.9 * z("num_Mean_luchtweg_druk_max", 13)
            + 0.7 * z("num_Piek_druk_max", 23)
            + 0.5 * z("num_PEEP_Set_max", 8)
            + 0.4 * z("num_Cdyn_n", 21)
            + rng.normal(0, 2.4, size=len(df)))
    los = np.clip(base, 0.25, 21.0)
    return np.round(los, 4)

def main(real, out, n, seed, n_extra):
    rng = np.random.default_rng(seed)
    header = pd.read_csv(real, nrows=0, low_memory=False)
    cols = pick_columns(list(header.columns), n_extra, rng)
    real_df = pd.read_csv(real, usecols=[c for c in cols if c in header.columns], low_memory=False)
    syn = pd.DataFrame({c: resample_col(real_df[c], n, rng) for c in real_df.columns})
    # Unidade (specialty): amostra das top-8 especialidades reais com pesos suavizados
    # (sqrt), para que várias unidades tenham suporte suficiente no split/funnel do
    # demo (a distribuição real é muito concentrada em uma especialidade).
    top = real_df["specialty"].dropna().value_counts().head(8)
    labels = top.index.tolist(); w = np.sqrt(top.values.astype(float)); w = w / w.sum()
    syn["specialty"] = rng.choice(labels, size=len(syn), p=w)
    syn["icu_los_d"] = synth_los(syn, rng)                       # desfecho sintético c/ sinal
    syn["icu_los_h"] = np.round(syn["icu_los_d"] * 24.0, 2)      # horas (usado no filtro <6h)
    tail = ["icu_los_h", "icu_los_d"]
    order = ["specialty"] + [c for c in syn.columns if c not in (["specialty"] + tail)] + tail
    syn = syn[order]
    # ids SINTÉTICOS novos (não os reais) — 1 admissão por paciente
    ids = [f"SYN{i+1:05d}" for i in range(len(syn))]
    syn.insert(0, "admissionid", ids); syn.insert(1, "patientid", ids)
    syn.to_csv(out, index=False)
    print(f"OK: {out}  shape={syn.shape}  (synthetic, no real rows/identifiers)")
    print(f"  unidades (specialty): {syn['specialty'].nunique()}  | LoS mediana={syn['icu_los_d'].median():.2f}d")

if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--real", default="umcdb_24h_patient_level.csv")
    ap.add_argument("--out", default="umcdb_synthetic_sample.csv")
    ap.add_argument("--n", type=int, default=1000)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--n_extra", type=int, default=60)
    a = ap.parse_args(); main(a.real, a.out, a.n, a.seed, a.n_extra)
