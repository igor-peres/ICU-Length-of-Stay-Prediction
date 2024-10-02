# ICU Length of Stay Prediction

## Introduction

This project develops a machine learning model to predict the length of stay (LOS) in the Intensive Care Unit (ICU). The model leverages patient data to estimate how long an individual will remain in the ICU, aiding healthcare providers in resource management and patient care planning. The model can be tested with synthetic data made available in this repository or your own patient data.
**Notice**: This README is in regards to the second version of the model, contained in the Stacking_NumericLOS_V2.0 folder.

## Table of Contents

- [Introduction](#introduction)
- [Installation](#installation)
- [Usage](#usage)
- [Dependencies](#dependencies)
- [Configuration](#configuration)
- [Documentation](#documentation)
- [Examples](#examples)
- [Troubleshooting](#troubleshooting)
- [Contributors](#contributors)
- [License](#license)

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

## Configuration

- The scripts (`Training.R` and `Testing.R`) are preconfigured to work with the provided `predictors.csv` dataset and the SLOS model file.
- Adjustments to the data format or model configurations may require modifying the R scripts.

## Documentation

- **Training.R**: This script is used to train the ICU Length of Stay prediction model. Modify it if you need to retrain the model with new data.
- **Testing.R**: This script loads the pretrained model (`SLOS_model.RData`) and runs it on test data to produce evaluation results.
- **DataDictionary.pdf**: Documentation for each column in the input data

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

- **Project Lead**: Professor Igor Peres
- **Undergradute Researchers**: Guilherme Ferrari and Joana da Matta

## License

