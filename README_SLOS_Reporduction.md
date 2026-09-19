# SLOS — Reprodução (AmsterdamUMCdb)

Pipeline em duas etapas, do banco bruto aos números e figuras do artigo.

## Etapa 1 — Extração 24h
`umcdb_24h_extraction.py`
- Entrada: tabelas brutas do AmsterdamUMCdb (`admissions`, `numericitems`,
  `listitems`, `drugitems`, `processitems`, `freetextitems`,
  `procedureorderitems`) — ajuste os caminhos no topo do script.
- Faz o janelamento por timestamp até **admissão + 24h** em cada tabela e
  pivota para nível-paciente.
- Saída: `umcdb_24h_patient_level.csv` (+ CSVs de diagnóstico/cobertura).

## Etapa 2 — Retreinamento e resultados
`slos_umcdb_reproduction.py`
- Entrada: `umcdb_24h_patient_level.csv` (saída da Etapa 1).
- Exclusão de pacientes (Peres 2022): LoS UTI < 6h removido; idade > 16
  (auto); LoS hospitalar prévio N/A no UMCdb.
- Pipeline: split 80/20 estratificado por especialidade; missing 30%; NZV;
  correlação Pearson 0,75 (numéricas) + Cramér's V 0,5 (categóricas/binárias)
  + salvaguarda anti-vazamento |r|>0,75 com o desfecho; imputação MICE (PMM);
  RFE (21 features, platô da curva); stacking Ridge + Random Forest com
  meta-learner Random Forest (CV 5-fold); SLOS por especialidade + funnel (ems).
- Saída: métricas nível-paciente e nível-unidade, SLOS, funnel, o JSON
  `slos_umcdb_results.json` e as figuras `fig_importance_nl.png`,
  `fig_calibration_nl.png`, `fig_efficiency_panel_nl.png`.

Uso:
```
python slos_umcdb_reproduction.py --csv umcdb_24h_patient_level.csv --outdir .
```

Dependências: numpy, pandas, scikit-learn, matplotlib.
Semente fixa (SEED=42) — resultados determinísticos.

*Todos os parâmetros estão como constantes no topo de `slos_umcdb_reproduction.py`.*
