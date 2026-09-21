#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Treina uma versao COMPACTA e fiel do modelo de LoS (UMCdb 24h) para rodar
inteiramente no navegador (Clinical Predictor client-side), e compara as
metricas com o modelo cheio. Exporta JSON compacto (arredondado) + input-spec.
Reutiliza o pipeline de slos_umcdb_reproduction.py via build_clinical_model.prepare().
"""
import json, os, numpy as np
from build_clinical_model import prepare, build_input_spec
from slos_umcdb_reproduction import LOS_CAP_D, SEED
from sklearn.linear_model import Ridge
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import make_pipeline
from sklearn.ensemble import RandomForestRegressor, StackingRegressor
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score

CSV = "/mnt/user-data/uploads/SLOS/data/umcdb_24h_patient_level.csv"

def export_tree(t, ndig=4):
    tr = t.tree_
    r = lambda x: round(float(x), ndig)
    return {"f":[int(x) for x in tr.feature], "t":[r(x) for x in tr.threshold],
            "l":[int(x) for x in tr.children_left], "r":[int(x) for x in tr.children_right],
            "v":[r(tr.value[i][0][0]) for i in range(tr.node_count)]}

def evaluate(model, X, top, tr, te, y, tag):
    p = np.clip(model.predict(X[top].values[te]), 0, LOS_CAP_D)
    rmse = mean_squared_error(y[te], p) ** 0.5
    mae = mean_absolute_error(y[te], p); r2 = r2_score(y[te], p)
    print(f"[{tag}] RMSE={rmse:.3f} MAE={mae:.3f} R2={r2:.3f}")
    return p, rmse, mae, r2

def make_stack(n_base, d_base, leaf_base, n_meta, d_meta, leaf_meta):
    base = [("ridge", make_pipeline(StandardScaler(), Ridge(alpha=1.0))),
            ("rf", RandomForestRegressor(n_estimators=n_base, max_depth=d_base,
                    min_samples_leaf=leaf_base, max_features=0.34, n_jobs=-1, random_state=SEED))]
    return StackingRegressor(estimators=base,
        final_estimator=RandomForestRegressor(n_estimators=n_meta, max_depth=d_meta,
                    min_samples_leaf=leaf_meta, n_jobs=-1, random_state=SEED), cv=5, n_jobs=-1)

def main():
    df, data, X, top, tr, te, y, cont3, catg2, med, modes, num_cont, mice = prepare(CSV)
    print("features:", len(top))

    # ---- modelo CHEIO (referencia, igual ao build_clinical_model) ----
    full = make_stack(200, 14, 5, 100, 6, 10); full.fit(X[top].values[tr], y[tr])
    evaluate(full, X, top, tr, te, y, "FULL 200/100")

    # ---- modelo COMPACTO (navegador) ----
    comp = make_stack(60, 12, 8, 40, 6, 12); comp.fit(X[top].values[tr], y[tr])
    _,rmse,mae,r2 = evaluate(comp, X, top, tr, te, y, "COMPACT 60/40")

    # export JSON compacto
    ridge_pipe = comp.estimators_[0]; sc = ridge_pipe.named_steps["standardscaler"]; rg = ridge_pipe.named_steps["ridge"]
    rf_base = comp.estimators_[1]; meta = comp.final_estimator_
    spec = build_input_spec(top, catg2, X, data, tr, cont3)
    mj = {"features": top,
          "ridge": {"mean":[round(float(x),6) for x in sc.mean_], "scale":[round(float(x),6) for x in sc.scale_],
                    "coef":[round(float(x),6) for x in rg.coef_], "intercept":round(float(rg.intercept_),6)},
          "rf": [export_tree(t) for t in rf_base.estimators_],
          "meta": {"order":["ridge","rf"], "trees":[export_tree(t) for t in meta.estimators_]},
          "clip":[0, LOS_CAP_D], "input_spec": spec,
          "metrics": {"rmse":round(rmse,3), "mae":round(mae,3), "r2":round(r2,3)}}
    outp = "/mnt/user-data/outputs/slos_umcdb_model_compact.json"
    json.dump(mj, open(outp,"w"), separators=(",",":"))
    print("compact json %.2f MB" % (os.path.getsize(outp)/1e6))
    # 3 casos de teste p/ parity JS
    pe = np.clip(comp.predict(X[top].values[te][:5]),0,LOS_CAP_D)
    rows = X[top].values[te][:5]
    json.dump({"features":top, "cases":[[round(float(v),6) for v in row] for row in rows],
               "pred":[round(float(v),4) for v in pe]},
              open("/mnt/user-data/outputs/_parity_cases.json","w"))
    print("parity preds:", [round(float(v),3) for v in pe])

if __name__ == "__main__":
    main()
