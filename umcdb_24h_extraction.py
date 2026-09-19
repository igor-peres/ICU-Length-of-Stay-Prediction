#!/usr/bin/env python3
"""
╔══════════════════════════════════════════════════════════════════╗
║         UMCDB — Extração de Features nas Primeiras 24h          ║
║         Impact Lab · Liga de IA PUC-Rio                         ║
╚══════════════════════════════════════════════════════════════════╝

COMO RODAR ESTE SCRIPT
──────────────────────
  OPÇÃO 1 — Terminal do VS Code (RECOMENDADO para arquivos grandes):
    1. Abra o VS Code
    2. Menu: Terminal → New Terminal
    3. Cole e execute:
         cd "C:\\Users\\Igor Peres\\Desktop\\Arquivos pesados"
         python umcdb_24h_extraction.py

    O script vai imprimir o progresso no terminal em tempo real.
    O arquivo de 80 GB (numericitems) pode levar 30–60 minutos.
    Não feche o terminal enquanto estiver rodando.

  OPÇÃO 2 — Jupyter / Interactive Window do VS Code:
    Selecione TODO o código (Ctrl+A) → Run Cell (Shift+Enter)
    Funciona, mas o progresso aparece mais lento na tela.

  NÃO rode célula por célula — o script precisa rodar de uma vez
  porque as variáveis de uma etapa são usadas nas seguintes.

COMO CHAMAR O CLAUDE CODE NO TERMINAL
──────────────────────────────────────
  Se você tiver o Claude Code instalado (npm install -g @anthropic/claude-code),
  pode abrir o terminal do VS Code e digitar:
    claude
  Isso abre uma sessão interativa onde o Claude pode ver e editar este script,
  rodar comandos e acompanhar erros em tempo real.

O QUE ESTE SCRIPT FAZ
──────────────────────
  Lê as 7 tabelas brutas do UMCDB, filtra apenas os dados das primeiras
  24 horas de cada internação na UTI, e gera um arquivo CSV com uma
  linha por internação e uma coluna por variável clínica.

  Tabelas processadas:
    admissions          → dados demográficos + desfecho (tempo de UTI)
    processitems        → procedimentos: linhas, ventilação, drenos...
    freetextitems       → resultados laboratoriais qualitativos
    procedureorderitems → ordens de procedimento (lab, raio-X, etc.)
    drugitems           → medicamentos administrados      (~818 MB)
    numericitems        → medidas numéricas: FC, PA, labs (~80 GB)
    listitems           → medidas categóricas             (~2.8 GB)

  Arquivos gerados na pasta "Arquivos pesados":
    umcdb_24h_patient_level.csv     ← dataset principal
    umcdb_24h_column_inventory.csv  ← lista de todas as colunas
    umcdb_24h_descriptive.csv       ← estatísticas descritivas
    numeric_item_coverage.csv       ← cobertura dos itens numéricos
    list_item_coverage.csv          ← cobertura dos itens categóricos

Autor : Igor Peres (igor.peres@puc-rio.br) — Impact Lab, Liga de IA PUC-Rio
Projeto: SLOS — Expert Systems and Applications
"""

import gc
import sys
import time
import warnings
from datetime import datetime
from functools import reduce
from pathlib import Path

import numpy as np
import pandas as pd

warnings.filterwarnings("ignore")

# Registra o horário de início para calcular tempo total ao final
_INICIO = time.time()

def tempo_decorrido():
    """Retorna quanto tempo passou desde o início do script."""
    seg = int(time.time() - _INICIO)
    h, resto = divmod(seg, 3600)
    m, s = divmod(resto, 60)
    if h > 0:
        return f"{h}h {m}min {s}s"
    elif m > 0:
        return f"{m}min {s}s"
    return f"{s}s"

print("=" * 62)
print("  UMCDB 24h Extraction — iniciando")
print(f"  {datetime.now().strftime('%d/%m/%Y %H:%M:%S')}")
print("=" * 62)
print()
print("  Verificando bibliotecas...")
try:
    import numpy, pandas
    print(f"  ✓ pandas {pandas.__version__}  |  numpy {numpy.__version__}")
except ImportError as e:
    print(f"  ✗ Biblioteca faltando: {e}")
    print("    Instale com:  pip install pandas numpy")
    sys.exit(1)
print()

# ═══════════════════════════════════════════════════════════════
# CONFIGURAÇÃO  —  ajuste os caminhos se necessário
# ═══════════════════════════════════════════════════════════════

BASE_DIR    = Path(r"C:\Users\Igor Peres\Desktop\Arquivos pesados")
UMCDB_DIR   = BASE_DIR / "UMCdb-20260912T010305Z-1-001" / "UMCdb"
NUMERIC_CSV = BASE_DIR / "numericitems-003" / "numericitems" / "numericitems.csv"
LIST_CSV    = BASE_DIR / "listitems-002.csv"
OUTPUT_DIR  = BASE_DIR  # onde salvar os CSVs de saída

# Constantes de tempo (milissegundos)
H24_MS   = 24 * 60 * 60 * 1000   # 86 400 000 ms = 24 h
DAYS21_H = 21 * 24                # 504 h = 21 dias (cap do desfecho)

# Localizações de UTI a incluir
ICU_LOCATIONS = ["IC", "IC&MC", "MC&IC"]

# OBS: não há filtro de cobertura mínima nesta extração.
# Todas as variáveis presentes nas primeiras 24h são mantidas.
# Near-zero variance e seleção serão feitos num passo separado de pré-processamento.
# Os arquivos *_coverage.csv gerados servem apenas como referência para o artigo.

# Linhas por chunk para leitura de arquivos grandes
CHUNK_SIZE = 500_000

# ═══════════════════════════════════════════════════════════════
# UTILITÁRIOS
# ═══════════════════════════════════════════════════════════════

def sep(title):
    print(f"\n{'═' * 62}")
    print(f"  {title}  [{tempo_decorrido()} desde o início]")
    print(f"{'═' * 62}")

def progress(chunk_n, chunk_size=CHUNK_SIZE):
    if chunk_n % 50 == 0 and chunk_n > 0:
        linhas = chunk_n * chunk_size
        print(f"    ... {linhas:,} linhas processadas  [{tempo_decorrido()}]",
              flush=True)


def filter_24h_df(df, time_col, adm_map, icu_ids,
                  allow_pre_admission=False):
    """
    Filtra um DataFrame para internações de UTI dentro das primeiras 24h.

    allow_pre_admission=True: inclui registros com início antes da admissão
      (útil p/ processitems e drugitems — procedimentos iniciados no pronto-socorro)
    """
    df = df[df["admissionid"].isin(icu_ids)].copy()
    if df.empty:
        return df
    df["_adm"] = df["admissionid"].map(adm_map)
    df["_t"]   = df[time_col] - df["_adm"]         # tempo desde admissão (ms)

    if allow_pre_admission:
        mask = df["_t"] <= H24_MS                   # iniciou antes da marca de 24h
    else:
        mask = (df["_t"] >= 0) & (df["_t"] <= H24_MS)

    return df[mask].drop(columns=["_adm", "_t"])


def binary_pivot(df, group_col, prefix):
    """Cria pivot binário (1/0) para todos os itens — sem filtro de cobertura."""
    df = df.copy()
    df["_val"] = 1
    pivot = (
        df.groupby(["admissionid", group_col])["_val"]
        .max()
        .unstack(fill_value=0)
        .reset_index()
    )
    pivot.columns = ["admissionid"] + [f"{prefix}{c}" for c in pivot.columns[1:]]
    return pivot


# ═══════════════════════════════════════════════════════════════
# PASSO 1 — ADMISSIONS
# ═══════════════════════════════════════════════════════════════

sep("PASSO 1 · Admissions")

admissions = pd.read_csv(UMCDB_DIR / "admissions.csv")
icu = admissions[admissions["location"].isin(ICU_LOCATIONS)].copy()

n_adm = len(icu)
print(f"  Internações UTI   : {n_adm:,}")
print(f"  Pacientes únicos  : {icu['patientid'].nunique():,}")

# Desfecho: LOS em horas com cap de 21 dias
icu["icu_los_h"] = icu["lengthofstay"].clip(upper=DAYS21_H)
icu["icu_los_d"] = icu["icu_los_h"] / 24

print(f"  LOS (h) mediana   : {icu['lengthofstay'].median():.1f}")
print(f"  LOS cap (h) mediana: {icu['icu_los_h'].median():.1f}")

# Dataset base
DEMO_COLS = [
    "admissionid", "patientid", "admissioncount", "location",
    "urgency", "origin", "admittedat", "admissionyeargroup",
    "gender", "agegroup", "weightgroup", "weightsource",
    "heightgroup", "heightsource", "specialty", "destination",
    "dateofdeath", "icu_los_h", "icu_los_d",
]
patient_df = icu[DEMO_COLS].copy()

# Lookups rápidos
adm_map = dict(zip(icu["admissionid"], icu["admittedat"]))
icu_ids = set(icu["admissionid"].tolist())

# ═══════════════════════════════════════════════════════════════
# PASSO 2 — PROCESSITEMS  (256 K linhas — cabe em RAM)
# ═══════════════════════════════════════════════════════════════

sep("PASSO 2 · processitems (linhas, cateteres, ventilação)")

proc = pd.read_csv(UMCDB_DIR / "processitems.csv", encoding="latin1")
print(f"  Registros totais  : {len(proc):,}")

proc_24h = filter_24h_df(proc, "start", adm_map, icu_ids,
                          allow_pre_admission=True)
print(f"  Após filtro 24h   : {len(proc_24h):,}")
print(f"  Itens únicos      : {proc_24h['item'].nunique()}")

proc_pivot = binary_pivot(proc_24h, "item", "proc_")
patient_df = patient_df.merge(proc_pivot, on="admissionid", how="left")

proc_cols = [c for c in patient_df.columns if c.startswith("proc_")]
patient_df[proc_cols] = patient_df[proc_cols].fillna(0).astype(np.int8)
print(f"  Colunas adicionadas: {len(proc_cols)}")

del proc, proc_24h, proc_pivot
gc.collect()

# ═══════════════════════════════════════════════════════════════
# PASSO 3 — FREETEXTITEMS  (52 MB — cabe em RAM)
# ═══════════════════════════════════════════════════════════════

sep("PASSO 3 · freetextitems (resultados laboratoriais qualitativos)")

ftext = pd.read_csv(UMCDB_DIR / "freetextitems.csv",
                    encoding="latin1")
ftext_24h = filter_24h_df(ftext, "measuredat", adm_map, icu_ids)
print(f"  Registros 24h     : {len(ftext_24h):,}")
print(f"  Itens únicos      : {ftext_24h['item'].nunique()}")

# Contagem de registros por item (quantas vezes foi medido nas 24h)
# Sem filtro de cobertura — todos os itens são mantidos
ftext_24h = ftext_24h.copy()
ftext_24h["_val"] = 1
ftext_cnt = (
    ftext_24h.groupby(["admissionid", "item"])["_val"]
    .count()
    .unstack(fill_value=0)
    .reset_index()
)
ftext_cnt.columns = ["admissionid"] + [f"freetext_{c}" for c in ftext_cnt.columns[1:]]

patient_df = patient_df.merge(ftext_cnt, on="admissionid", how="left")
ft_cols = [c for c in patient_df.columns if c.startswith("freetext_")]
patient_df[ft_cols] = patient_df[ft_cols].fillna(0)
print(f"  Colunas adicionadas: {len(ft_cols)}")

del ftext, ftext_24h, ftext_cnt
gc.collect()

# ═══════════════════════════════════════════════════════════════
# PASSO 4 — PROCEDUREORDERITEMS  (208 MB — cabe em RAM)
# ═══════════════════════════════════════════════════════════════

sep("PASSO 4 · procedureorderitems (ordens de procedimento)")

porder = pd.read_csv(UMCDB_DIR / "procedureorderitems.csv", encoding="latin1")
porder_24h = filter_24h_df(porder, "registeredat", adm_map, icu_ids)
print(f"  Registros 24h     : {len(porder_24h):,}")
print(f"  Categorias únicas : {porder_24h['ordercategoryname'].nunique()}")

porder_pivot = binary_pivot(porder_24h, "ordercategoryname", "porder_")
patient_df = patient_df.merge(porder_pivot, on="admissionid", how="left")
po_cols = [c for c in patient_df.columns if c.startswith("porder_")]
patient_df[po_cols] = patient_df[po_cols].fillna(0).astype(np.int8)
print(f"  Colunas adicionadas: {len(po_cols)}")

del porder, porder_24h, porder_pivot
gc.collect()

# ═══════════════════════════════════════════════════════════════
# PASSO 5 — DRUGITEMS  (818 MB — chunked)
# ═══════════════════════════════════════════════════════════════

sep("PASSO 5 · drugitems (medicamentos)")

drug_parts = []
n_drug = 0
for i, chunk in enumerate(
    pd.read_csv(UMCDB_DIR / "drugitems.csv", encoding="latin1",
                chunksize=CHUNK_SIZE)
):
    ch = filter_24h_df(chunk, "start", adm_map, icu_ids,
                        allow_pre_admission=True)
    if not ch.empty:
        n_drug += len(ch)
        drug_parts.append(ch[["admissionid", "item"]].copy())
    progress(i + 1)

print(f"  Registros 24h     : {n_drug:,}")

if drug_parts:
    drug_df = pd.concat(drug_parts, ignore_index=True)
    drug_pivot = binary_pivot(drug_df, "item", "drug_")
    patient_df = patient_df.merge(drug_pivot, on="admissionid", how="left")
    drug_cols = [c for c in patient_df.columns if c.startswith("drug_")]
    patient_df[drug_cols] = patient_df[drug_cols].fillna(0).astype(np.int8)
    print(f"  Colunas adicionadas: {len(drug_cols)}")
    del drug_parts, drug_df, drug_pivot
else:
    print("  Nenhum registro encontrado.")
gc.collect()

# ═══════════════════════════════════════════════════════════════
# PASSO 6 — NUMERICITEMS  (80 GB — 2 passagens em chunks)
# ═══════════════════════════════════════════════════════════════

sep("PASSO 6 · numericitems (medidas numéricas — ~80 GB, pode demorar 30–60 min)")

# ---------- Passagem 1: mapa de cobertura por item ----------
print("  Passagem 1/2: calculando cobertura de itens...")
item_adm_sets = {}

for i, chunk in enumerate(
    pd.read_csv(NUMERIC_CSV, encoding="latin1", chunksize=CHUNK_SIZE)
):
    ch = filter_24h_df(chunk, "measuredat", adm_map, icu_ids)
    if ch.empty:
        progress(i + 1)
        continue
    for item, grp in ch.groupby("item"):
        if item not in item_adm_sets:
            item_adm_sets[item] = set()
        item_adm_sets[item].update(grp["admissionid"].tolist())
    progress(i + 1)

cov_num = pd.Series({k: len(v) / n_adm for k, v in item_adm_sets.items()})
all_num_items = set(cov_num.index.tolist())
print(f"  Itens encontrados : {len(cov_num):,}  (todos serão mantidos)")

# Salva relatório de cobertura — apenas para diagnóstico/artigo
cov_num_df = cov_num.reset_index()
cov_num_df.columns = ["item", "coverage"]
cov_num_df.sort_values("coverage", ascending=False).to_csv(
    OUTPUT_DIR / "numeric_item_coverage.csv", index=False
)
print("  Salvo: numeric_item_coverage.csv")

del item_adm_sets
gc.collect()

# ---------- Passagem 2: agregação (mean, min, max) — todos os itens ----------
print("  Passagem 2/2: agregando mean / min / max por item × admissão...")
num_parts = {item: [] for item in all_num_items}

for i, chunk in enumerate(
    pd.read_csv(NUMERIC_CSV, encoding="latin1", chunksize=CHUNK_SIZE)
):
    ch = filter_24h_df(chunk, "measuredat", adm_map, icu_ids)
    if ch.empty:
        progress(i + 1)
        continue
    ch["value"] = pd.to_numeric(ch["value"], errors="coerce")
    for item, grp in ch.groupby("item"):
        num_parts[item].append(grp[["admissionid", "value"]].copy())
    progress(i + 1)

print("  Construindo tabela wide...")
num_pivots = []
for item, dfs in num_parts.items():
    if not dfs:
        continue
    df_i = pd.concat(dfs, ignore_index=True)
    agg = df_i.groupby("admissionid")["value"].agg(["mean", "min", "max", "count"])
    safe = item.replace("/", "_").replace(" ", "_").replace("(", "").replace(")", "")
    agg.columns = [f"num_{safe}_mean", f"num_{safe}_min",
                   f"num_{safe}_max",  f"num_{safe}_n"]
    num_pivots.append(agg.reset_index())

if num_pivots:
    num_wide = reduce(lambda a, b: a.merge(b, on="admissionid", how="outer"), num_pivots)
    patient_df = patient_df.merge(num_wide, on="admissionid", how="left")
    num_cols = [c for c in patient_df.columns if c.startswith("num_")]
    print(f"  Colunas adicionadas: {len(num_cols)}")

del num_parts, num_pivots
gc.collect()

# ═══════════════════════════════════════════════════════════════
# PASSO 7 — LISTITEMS  (2.8 GB — 2 passagens em chunks)
# ═══════════════════════════════════════════════════════════════

sep("PASSO 7 · listitems (medidas categóricas — ~2.8 GB)")

# ---------- Passagem 1 ----------
print("  Passagem 1/2: calculando cobertura de itens...")
list_adm_sets = {}

for i, chunk in enumerate(
    pd.read_csv(LIST_CSV, encoding="latin1", chunksize=CHUNK_SIZE)
):
    ch = filter_24h_df(chunk, "measuredat", adm_map, icu_ids)
    if ch.empty:
        progress(i + 1)
        continue
    for item, grp in ch.groupby("item"):
        if item not in list_adm_sets:
            list_adm_sets[item] = set()
        list_adm_sets[item].update(grp["admissionid"].tolist())
    progress(i + 1)

cov_list = pd.Series({k: len(v) / n_adm for k, v in list_adm_sets.items()})
all_list_items = set(cov_list.index.tolist())
print(f"  Itens encontrados : {len(cov_list):,}  (todos serão mantidos)")

# Salva relatório de cobertura — apenas para diagnóstico/artigo
cov_list_df = cov_list.reset_index()
cov_list_df.columns = ["item", "coverage"]
cov_list_df.sort_values("coverage", ascending=False).to_csv(
    OUTPUT_DIR / "list_item_coverage.csv", index=False
)
print("  Salvo: list_item_coverage.csv")

del list_adm_sets
gc.collect()

# ---------- Passagem 2 — todos os itens ----------
print("  Passagem 2/2: coletando moda por item × admissão...")
list_parts = {item: [] for item in all_list_items}

for i, chunk in enumerate(
    pd.read_csv(LIST_CSV, encoding="latin1", chunksize=CHUNK_SIZE)
):
    ch = filter_24h_df(chunk, "measuredat", adm_map, icu_ids)
    if ch.empty:
        progress(i + 1)
        continue
    for item, grp in ch.groupby("item"):
        list_parts[item].append(grp[["admissionid", "value"]].copy())
    progress(i + 1)

print("  Calculando moda...")
list_pivots = []
for item, dfs in list_parts.items():
    if not dfs:
        continue
    df_i = pd.concat(dfs, ignore_index=True)
    mode_s = df_i.groupby("admissionid")["value"].agg(
        lambda x: x.mode().iloc[0] if len(x) > 0 else np.nan
    )
    safe = item.replace("/", "_").replace(" ", "_").replace("(", "").replace(")", "")
    mode_s.name = f"list_{safe}_mode"
    list_pivots.append(mode_s.reset_index())

if list_pivots:
    list_wide = reduce(lambda a, b: a.merge(b, on="admissionid", how="outer"), list_pivots)
    patient_df = patient_df.merge(list_wide, on="admissionid", how="left")
    list_cols = [c for c in patient_df.columns if c.startswith("list_")]
    print(f"  Colunas adicionadas: {len(list_cols)}")

del list_parts, list_pivots
gc.collect()

# ═══════════════════════════════════════════════════════════════
# PASSO 8 — SALVAR SAÍDAS
# ═══════════════════════════════════════════════════════════════

sep("PASSO 8 · Salvando saídas")

# Remove coluna interna de referência de timestamp
patient_df = patient_df.drop(columns=["admittedat"], errors="ignore")

# ── Dataset principal ──
out_main = OUTPUT_DIR / "umcdb_24h_patient_level.csv"
patient_df.to_csv(out_main, index=False)
print(f"  Dataset principal : {out_main}")
print(f"  Shape             : {patient_df.shape[0]:,} linhas × {patient_df.shape[1]:,} colunas")

# ── Inventário de colunas ──
def col_source(c):
    if c.startswith("proc_"):    return "processitems"
    if c.startswith("freetext_"): return "freetextitems"
    if c.startswith("porder_"):  return "procedureorderitems"
    if c.startswith("drug_"):    return "drugitems"
    if c.startswith("num_"):     return "numericitems"
    if c.startswith("list_"):    return "listitems"
    if c in ("icu_los_h", "icu_los_d"): return "outcome"
    return "admissions"

inventory = pd.DataFrame({
    "column"      : patient_df.columns,
    "source"      : [col_source(c) for c in patient_df.columns],
    "dtype"       : patient_df.dtypes.values,
    "missing_pct" : (patient_df.isnull().mean() * 100).values,
    "n_unique"    : [patient_df[c].nunique() for c in patient_df.columns],
})
inventory.to_csv(OUTPUT_DIR / "umcdb_24h_column_inventory.csv", index=False)
print("  Inventário        : umcdb_24h_column_inventory.csv")

# ── Estatísticas descritivas ──
desc = patient_df.describe(include="all").T
desc["missing_pct"] = (patient_df.isnull().mean() * 100)
desc["dtype"] = patient_df.dtypes
desc.to_csv(OUTPUT_DIR / "umcdb_24h_descriptive.csv")
print("  Descritiva        : umcdb_24h_descriptive.csv")

sep("CONCLUÍDO")
print(f"  {patient_df.shape[0]:,} internações × {patient_df.shape[1]:,} variáveis")
print(f"  Desfecho: icu_los_h (horas, cap 21 dias) | icu_los_d (dias)")
print(f"  Tempo total: {tempo_decorrido()}")
print()
print("  Arquivos gerados em:")
print(f"  {OUTPUT_DIR}")
print()
for f in ["umcdb_24h_patient_level.csv", "umcdb_24h_column_inventory.csv",
          "umcdb_24h_descriptive.csv", "numeric_item_coverage.csv",
          "list_item_coverage.csv"]:
    p = OUTPUT_DIR / f
    if p.exists():
        size_mb = p.stat().st_size / 1e6
        print(f"    ✓ {f:45s} ({size_mb:.1f} MB)")
    else:
        print(f"    ✗ {f} — não gerado ainda")
print()
print("  Próximo passo: compartilhe os arquivos gerados com o Claude")
print("  para análise descritiva e comparação com a base anterior.")
