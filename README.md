---
title: "Replication Package for: Estimating the Extent and Sources of Model Uncertainty in Political Science"
output: html_document
---

# Abstract

Model uncertainty is widely acknowledged as a key challenge to scientific inference. But how large is the problem in practice, and what can be done to address it? We assess the extent and sources of model uncertainty by evaluating more than 3.6 billion model specifications across four prominent research areas in political science. We replicate three existing studies and examine one original case on the determinants of welfare generosity. For each case, we estimate the full model space of theoretically defensible specifications and assess the share of estimates that are statistically significant and in the expected direction. We then apply machine learning methods to predict significance patterns and identify which modeling decisions drive differences in findings. Our results reveal substantial model uncertainty and show that significance is often predictable, suggesting specification-driven rather than data-driven results. We conclude with practical recommendations for assessing robustness in empirical research.

# 1 Overview

This replication package reproduces all results from:

**Title:** Estimating the Extent and Sources of Model Uncertainty in Political Science  
**Authors:** Michael Ganslmeier^, Tim Vlandas^^
**Institutions:** ^University of Exeter / ^^University of Oxford
**Journal:** *Proceedings of the National Academy of Sciences* (PNAS)  
**Manuscript ID:** 2024-14926RR  
**Version:** 15 May 2025  

The analysis explores the extent and sources of model uncertainty in political science using four substantive domains:

- **CASE1**: Welfare State Generosity (OECD+)
- **CASE2**: Democratization (Global)
- **CASE3**: Public Goods Provision (China)
- **CASE4**: Trust in Institutions (Europe+)

The pipeline includes generating random model specifications, estimating them, evaluating significance patterns, and using machine learning to analyze predictive features across >3.6 billion model estimates.

# 2 Folder Structure

```text
1code/           # R scripts used in the replication
2data/           # Raw data (1raw), specifications (2spec), estimations (3est), aggregations (4aggr), predictions (5pred), neighbors (6nneigh)
3res/            # Output figures (Figure 1 to 4)
```

# 3 Setup Instructions

## 3.1 Requirements

Install R >= 4.2 and the following packages:

```r
install.packages("pacman")
pacman::p_load(tidyverse, data.table, pbmcapply, ggplot2, caret, keras, tensorflow,
               tidyr, dplyr, haven, patchwork, scales, gridExtra, stargazer, reticulate,
               nnet, e1071, mlr3, DALEX, iml, mlbench, fastDummies, yardstick)
```

Ensure Python environment is set:
```r
reticulate::use_condaenv("tf_env")
```

## 3.2 Data Access

The final datasets used for analysis are stored in `2data/1raw/`.

- **CASE1** (Welfare State): Available in the GitHub repository.
- **CASE2–CASE4**:
  1. European Social Survey (ESS): https://www.europeansocialsurvey.org/data-portal
  2. Claassen (2020): https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/HWLW0J
  3. Hong (2018): https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/IZC5P1

# 4 Scripts Overview

## Preprocessing Scripts (per case)
Each case has its own preprocessing script (not shown here) which:
- Loads and cleans raw data
- Renames variables
- Defines country and time samples
- Creates dependent and control variables
- Merges metadata (e.g., unit and time fixed effects)
- Saves a clean dataset to `2data/1raw/`

Variable names and labels are defined in:
`1data/1raw/admin/variable_labels.xlsx`

## `2_1_spec.R`
Creates a dataset of model specifications by combining all combinations of:
- Dependent variables
- Control sets
- Fixed effects (unit/time/none)
- Country and time samples

Generates up to 50,000 specifications by default (`space_n`), though this can be modified based on available computational resources.

## `3_1_est.R`
Estimates each model using standard OLS, with three standard error treatments:
- Classical
- Robust (HC1)
- Clustered (unit-level)

Each result includes coefficients, SEs, p-values, R-squared, AIC, log-likelihood, and sample size.

## `4_1_aggr.R`
Aggregates estimation output and computes significance shares (positive/negative/insignificant).
- Aggregates by specification dimensions (control set, sample, SE, FE, etc.)
- Constructs machine learning dataset for predicting significance classification

## `5_1_pred.R`
Runs machine learning models to understand which specification features predict significance.
- Multinomial logit (ML)
- Neural networks (NN)

Outputs prediction accuracy and feature importance metrics.

## `5_1a_dl_helper.R` and `5_1b_dl_helper.R`
Helper scripts for tuning and training NN models with grid search and SHAP value feature attribution.

## `6_1_nneigh.R`
Randomly selects specifications and makes one modeling decision change at a time (e.g., swapping a control or SE type).
- Evaluates the probability that significance classification changes due to single changes

## `helper.R`
Contains all core utility functions:
- Model estimation wrappers
- Aggregation and plotting functions
- SHAP and prediction probability comparisons

## `wrapper.R`
Runs the full replication pipeline:
1. Preprocessing (external to wrapper)
2. Specification generation
3. Model estimation
4. Aggregation
5. Prediction
6. Visualizations (Figures 1–4)

# 5 Replication Instructions

1. Set working directory at the top of `wrapper.R`
2. Modify `projects <- c('CASE1', 'CASE2', 'CASE3', 'CASE4')` to select specific cases
3. Run the full script:

```r
source('1code/wrapper.R')
```

Outputs:
- Figures to `3res/`
- Estimates and ML data to `2data/`

# 6 Code and Data Availability

All replication code will be published on GitHub without restriction. The processed data for CASE1 (Welfare State) will also be provided. Due to licensing restrictions, the raw datasets for CASE2–CASE4 are not hosted, but can be accessed freely from:

1. [European Social Survey](https://www.europeansocialsurvey.org/data-portal)  
2. [Claassen 2020, Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/HWLW0J)  
3. [Hong 2018, Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/IZC5P1)

# 7 Contact

Michael Ganslmeier  
University of Exeter  
📧 m.ganslmeier@exeter.ac.uk
