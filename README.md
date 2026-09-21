# ICU Length of Stay Prediction

> **Status:** the associated paper *SLOS: A Reproducible Machine Learning Pipeline for ICU Efficiency Benchmarking Across Health Systems* (Expert Systems with Applications) is currently **under review**. Contents of the [`reproduction_code/`](reproduction_code/) folder are provided for peer review and may change before publication.

## Introduction

This project develops a machine learning model to predict the length of stay (LOS) in the Intensive Care Unit (ICU). The model leverages patient data to estimate how long an individual will remain in the ICU, aiding healthcare providers in resource management and patient care planning. The model can be tested with synthetic data made available in this repository or your own patient data.

**Notice**: This README is in regards to the second version of the model, available in the Stacking_NumericLOS_V2.0 folder. For information about the SLOS package, please refer to the documentation available in the package's description.
```R
> install.packages("SLOS")
> library(SLOS)
> ?SLOS
```

**Reproducing the ESWA paper (AmsterdamUMCdb):** the end-to-end code, synthetic sample and models for the paper *SLOS: A Reproducible Machine Learning Pipeline for ICU Efficiency Benchmarking Across Health Systems* are in the [`reproduction_code/`](reproduction_code/) folder — see [Reproduction](#reproduction--eswa-paper-amsterdamumcdb) below.

## Table of Contents

- [Introduction](#introduction)
- [Reproduction — ESWA paper (AmsterdamUMCdb)](#reproduction--eswa-paper-amsterdamumcdb)
- [Installation](#installation)
- [Usage](#usage)
- [Dependencies](#dependencies)
- [Configuration](#configuration)
- [Documentation](#documentation)
- [Examples](#examples)
- [Troubleshooting](#troubleshooting)
- [Contributors](#contributors)
- [License](#license)

## Reproduction — ESWA paper (AmsterdamUMCdb)

The [`reproduction_code/`](reproduction_code/) folder reproduces the results, figures and models of the paper *SLOS: A Reproducible Machine Learning Pipeline for ICU Efficiency Benchmarking Across Health Systems* (Expert Systems with Applications), which validates the SLOS pipeline on the Dutch **AmsterdamUMCdb** database.

| File | What it does |
|---|---|
| `umcdb_24h_extraction.py` | Builds the first-24&nbsp;h patient-level table from the raw AmsterdamUMCdb tables (intensive-care locations only; each source table time-windowed to admission&nbsp;+&nbsp;24&nbsp;h). |
| `slos_umcdb_reproduction.py` | Runs the full pipeline — missingness screen (30%), near-zero-variance, Pearson (0.75) / Cramér's V (0.5) redundancy, outcome-correlation safeguard, MICE imputation, RFE, stacked Ridge&nbsp;+&nbsp;Random&nbsp;Forest with a Random-Forest meta-learner, SLOS per unit and funnel plot — and regenerates every reported metric and figure. |
| `build_clinical_model_compact.py` | Trains the final model and exports the full `slos_umcdb_model.pkl` and the compact browser model behind the [SLOS Hub Clinical Predictor](https://igor-peres.github.io/slos-hub/clinical-predictor.html). |
| `make_synthetic_umcdb.py` | Generates the synthetic sample below. |
| `umcdb_synthetic_sample.csv` | A fully **synthetic** (~1,200-row) sample in the 24&nbsp;h extraction format, so the pipeline can be run **without** access to the restricted AmsterdamUMCdb. No real rows or identifiers — not reconstructable to patients; safe to redistribute. |
| `fig_panel_journey.html` | Rebuilds the illustrative guided-panel figure. |
| `README.md` | Full details and the exact parameters. |

**Quick start** (no data access needed — uses the synthetic sample):

```bash
cd reproduction_code
pip install numpy pandas scikit-learn matplotlib joblib
python slos_umcdb_reproduction.py --csv umcdb_synthetic_sample.csv --outdir out_synth
```

This prints patient- and unit-level metrics and the SLOS distribution, and writes a funnel plot and figures to `out_synth/`. Values obtained on the synthetic sample are **illustrative** — the paper's exact numbers require the real AmsterdamUMCdb extraction. Runs are deterministic (`SEED = 42`).

The full trained scikit-learn model `slos_umcdb_model.pkl` (~24&nbsp;MB) is attached to the latest [Release](../../releases). Load it in Python with `joblib.load(...)`, predict on the one-hot feature matrix `X[features]`, and clip to `[0, 21]` days.

## Installation

1. Clone the repository to your local machine.
2. Install the required R packages (listed in the Dependencies section).
3. Download the necessary model file (`SLOS_model.RData`) and place it in the working directory.

```bash
git clone https://github.com/igor-peres/ICU-Length-of-Stay-Prediction
```

## Usage

1. Ensure that `SLOS_model.RData` is downloaded and available in your working directory.
2. Run the `Testing.R` script to evaluate the performance of the ICU Length of Stay model.

```bash
Rscript Testing.R
```

### Input
- **predictors.csv**: The file containing the model predictors.
- **Synthetic_TestingData.csv**: Synthetic patient data used for testing. You can change this input to your patient data.

### Output
- Performance evaluation results, including metrics and graphs, will be displayed.

## Dependencies

The project requires the following R libraries:

- `caret`
- `tidyverse`
- `caretEnsemble`
- `MLmetrics`

You can install these dependencies using the following command:

```r
install.packages(c("caret", "tidyverse", "caretEnsemble", "MLmetrics"))
```

The Python reproduction code (`reproduction_code/`) requires: `numpy`, `pandas`, `scikit-learn`, `matplotlib`, `joblib`.

## Configuration

- The scripts (`Training.R` and `Testing.R`) are preconfigured to work with the provided `predictors.csv` dataset and the SLOS model file.
- Adjustments to the data format or model configurations may require modifying the R scripts.

## Documentation

- **Training.R**: This script is used to train the ICU Length of Stay prediction model. Modify it if you need to retrain the model with new data.
- **Testing.R**: This script loads the pretrained model (`SLOS_model.RData`) and runs it on test data to produce evaluation results.
- **DataDictionary.pdf**: Documentation for each column in the input data
- **reproduction_code/**: Python code, synthetic sample and models reproducing the ESWA paper (AmsterdamUMCdb) — see the [Reproduction](#reproduction--eswa-paper-amsterdamumcdb) section.

## Examples

To run the model and view its performance, execute the following command:

```bash
Rscript Testing.R
```

Sample output includes accuracy scores and performance plots that show how well the model predicts ICU stay duration.

## Troubleshooting

- **Model not found error**: Ensure that `SLOS_model.RData` is located in your working directory before running the `Testing.R` script.
- **Missing libraries**: Make sure all required R libraries are installed before running the scripts.

## Contributors

- **Author**: Professor Igor Peres
- **Maintainer**: Joana da Matta

## License
This project is licensed under the terms of the [MIT license](https://github.com/igor-peres/ICU-Length-of-Stay-Prediction/blob/update2024/SLOS_package/LICENSE.md).
