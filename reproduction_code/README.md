# SLOS — Reprodução (AmsterdamUMCdb)

Pipeline do banco bruto aos números, figuras e modelos do artigo
(*SLOS: A Reproducible Machine Learning Pipeline for ICU Efficiency Benchmarking
Across Health Systems*, Expert Systems with Applications).

## Etapa 1 — Extração 24h
`umcdb_24h_extraction.py`
- Entrada: tabelas brutas do AmsterdamUMCdb (`admissions`, `numericitems`,
  `listitems`, `drugitems`, `processitems`, `freetextitems`,
  `procedureorderitems`) — ajuste os caminhos no topo do script.
- Restringe a base às localizações de UTI (`IC`, `IC&MC`, `MC&IC`), faz o
  janelamento por timestamp até **admissão + 24h** em cada tabela e pivota
  para nível-paciente.
- Saída: `umcdb_24h_patient_level.csv` (+ CSVs de diagnóstico/cobertura).

## Etapa 2 — Retreinamento e resultados
`slos_umcdb_reproduction.py`
- Entrada: `umcdb_24h_patient_level.csv` (saída da Etapa 1).
- Exclusão de pacientes (Peres 2022): LoS UTI < 6h removido; idade > 16
  (auto); LoS hospitalar prévio N/A no UMCdb.
- Pipeline: split 80/20 estratificado por especialidade; missing 30%; NZV
  (freqRatio > 19, pctUnique < 10); correlação Pearson 0,75 (numéricas) +
  Cramér's V 0,5 (categóricas/binárias) + salvaguarda anti-vazamento |r| > 0,75
  com o desfecho; imputação MICE (PMM); RFE (21 features, platô da curva);
  stacking Ridge + Random Forest com meta-learner Random Forest (CV 5-fold);
  SLOS por especialidade + funnel (`ems`).
- Saída: métricas nível-paciente e nível-unidade, SLOS, funnel, o JSON
  `slos_umcdb_results.json` e as figuras `fig_importance_nl.png`,
  `fig_calibration_nl.png`, `fig_efficiency_panel_nl.png`.

```
python slos_umcdb_reproduction.py --csv umcdb_24h_patient_level.csv --outdir .
```

## Etapa 3 — Modelo do preditor clínico (client-side)
`build_clinical_model_compact.py`
- Reutiliza exatamente o pipeline da Etapa 2 (mesmas features e split).
- Treina o modelo cheio (referência) e uma versão **compacta** (menos árvores),
  reportando as métricas das duas no teste.
- Exporta:
  - `slos_umcdb_model.pkl` — modelo scikit-learn completo (uso em Python).
  - `slos_umcdb_model_compact.json` — modelo serializado (Ridge + florestas +
    input-spec) que roda a predição no navegador, embutido no
    `clinical-predictor.html` do [SLOS Hub](https://igor-peres.github.io/slos-hub/).

```
python build_clinical_model_compact.py --csv umcdb_24h_patient_level.csv --outdir .
```

`fig_panel_journey.html` reconstrói a figura ilustrativa do painel guiado.

## Modelo treinado (.pkl)
O modelo completo `slos_umcdb_model.pkl` (~24 MB) está anexado ao
[Release](../../releases) mais recente (não versionado na árvore do repositório).
Uso:
```python
import joblib
m = joblib.load("slos_umcdb_model.pkl")          # {"model", "features", "note"}
pred = m["model"].predict(X[m["features"]]).clip(0, 21)   # LoS em dias
```

## Dependências e reprodutibilidade
`numpy`, `pandas`, `scikit-learn`, `matplotlib`, `joblib`.
Semente fixa (`SEED = 42`) — resultados determinísticos. Todos os parâmetros
estão como constantes no topo de `slos_umcdb_reproduction.py`.

## Sample sintético para replicação (revisores)
`umcdb_synthetic_sample.csv` (~1.200 linhas) + `make_synthetic_umcdb.py`
- Amostra **totalmente sintética** no formato da extração 24h (mesmos nomes de
  coluna), para rodar o pipeline **sem** acesso ao AmsterdamUMCdb (restrito).
- Nenhuma linha real é copiada: cada coluna é reamostrada da sua distribuição
  empírica (marginais + missingness); a unidade (`specialty`) vem das 8 maiores
  especialidades com pesos suavizados; o desfecho (LoS) é sintetizado por um
  modelo simples sobre variáveis de intensidade de cuidado. Sem identificadores
  reais — não reconstruível a pacientes; seguro para redistribuir.
- Roda direto no pipeline:
  ```
  python slos_umcdb_reproduction.py --csv umcdb_synthetic_sample.csv --outdir out_synth
  ```
  (produz métricas paciente/unidade, SLOS, funnel e figuras — valores ilustrativos,
  não os do artigo).
- Regenerar: `python make_synthetic_umcdb.py --real <base_real>.csv --out umcdb_synthetic_sample.csv --n 1200 --seed 42`
