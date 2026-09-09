# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this repository is

A research analysis project (not a software package) modeling coldwater fish habitat in five North Temperate Lakes (NTL-LTER, Wisconsin): Allequash (`AL`), Big Muskellunge (`BM`), Crystal (`CR`), Sparkling (`SP`), and Trout (`TR`). It is a flat directory of standalone R scripts, CSV data/results, and PNG figures — there is no package structure, build system, test suite, or dependency lockfile (no `renv`/`packrat`, no `.Rproj`, no `DESCRIPTION`).

## Running scripts

There is no runner or Makefile; scripts are run individually, typically from RStudio or via:

```bash
Rscript "01b_Predictor_Prep.R"
```

Most scripts follow a self-installing pattern at the top — they check `installed.packages()` and `install.packages()` anything missing from a `need <- c(...)` vector, then `library()` them. There's no CRAN mirror pinning beyond `options(repos = c(CRAN = "https://cloud.r-project.org"))`, and no CI, linter, or tests to run.

**Paths are hardcoded**, not relative. Nearly every script defines its own `base_dir` (either `"/Users/alexisjohnson/Downloads/LakeData"` or `"~/Downloads/LakeData"`) and reads/writes CSVs and PNGs directly into that flat directory via `file.path(base_dir, ...)`. Scripts assume this exact folder — moving the repo requires editing `base_dir` in each script.

## Core domain concept: VHT / CVHT

The central response variable is **VHT** (habitat thickness): the vertical depth range in a lake's water column where temperature ≤ 17°C *and* dissolved oxygen ≥ 3 mg/L — i.e. the coldwater refuge zone fish can occupy, computed per lake per sample date from thermal/DO profiles ("Emma's" original method/definition, referenced throughout).

**CVHT** (cumulative/standardized VHT) aggregates this into a seasonal metric: VHT is averaged per month, then summed across the July–August–September (JAS) months for a given lake-year. This `CVHT_standardized` value (by `lakeid` and `Year`) is the outcome variable for all downstream modeling.

## Pipeline structure

Scripts are numbered as a loose pipeline (numbering reflects intended order, not guaranteed reproducibility — several later stages assume objects left in the R environment from earlier ones rather than reading them back from disk):

1. **`01-ALL_Lake_VHT.R`** — pulls raw NTL-LTER physical limnology data (temperature/DO profiles, via EDI/PASTA download URLs), computes per-lake VHT and standardized `CVHT_standardized` by year (see above).
2. **`01b_Predictor_Prep.R`** — builds the predictor set: merges Emma's original predictors (`CVHT_Predictors.csv`) with NTL-LTER chemistry, chlorophyll, lake-level, and ice-duration data, filtered to the 5 study lakes.
3. **`01c_Master_Merge.R.R`** — joins the CVHT results (`CVHT_Results_All_Lakes.csv`) with the predictor set into the master modeling table.
4. **`02-Deterministic_Models.R`** — trains point-prediction models (Random Forest via `ranger`, XGBoost) on `Final_Analysis_Ready_Data.csv` using the `tidymodels` stack (`recipes`/`workflows`/`rsample`/`yardstick`/`parsnip`), exporting observed vs. predicted values per lake for the next stage.
5. **`02b_Model_Visualizations.R`** — plots observed vs. predicted CVHT by lake/model from stage 2's export.
6. **`03-ProbabilisticModel (XG_RF).r`** — takes deterministic observed/predicted pairs and fits a probabilistic layer (normal distribution over residuals, via `caret`) to express prediction uncertainty.
7. **`04_Poster_Figures.R`** — generates the full set of poster figures (time series, R²/RPSS bar charts, spider/radar plots, heatmaps) from the accumulated result CSVs.
8. **`05_Validation_Comparison.R.R`**, **`# 06_Validation_Comparison_Extended.R`**, **`07_PCA_Validation_Comparison.R`**, **`08_Forward_Chaining_AllLakes.R`** — compare validation schemes (LOOCV, Blocked K-Fold, Forward Chaining) and PCA vs. no-PCA preprocessing, across models and all 5 lakes.
9. **`09_Lake Classification Heatmap.R`** — final multi-panel classification heatmap figure (`ggplot2` + `patchwork`).

`HabitatMap.R` is a standalone conceptual/illustrative figure (simulated data, not fit to the real dataset).

## Messy/exploratory files — check before reusing

This directory retains dead ends and iteration artifacts; don't assume every `.R`/`.r` file is canonical:
- `Attempt2-02_Deterministic_Models.R`, `Attempt2-03-ProbabilisticModel.R` — earlier attempts superseded by `02-Deterministic_Models.R` / `03-ProbabilisticModel (XG_RF).r`.
- `WrongAccuracyRF&XGB.R`, `UntitledD3.R` — abandoned/experimental, per filenames.
- `CVHT_multi_lakes.r`, `knb-lter-ntl.32.31.r`, `Random Forest and XGBoost Modeling for CVHT: Observed vs. Predicted Data Export.R` — earlier or one-off variants of the numbered pipeline scripts above.
- Several filenames contain literal typos/artifacts (e.g. `01c_Master_Merge.R.R`, `05_Validation_Comparison.R.R`, and `# 06_Validation_Comparison_Extended.R` — the leading `#` and space are part of the actual filename). When referencing these files in shell commands, quote them exactly.

When asked to modify "the deterministic model" or "the probabilistic model" etc., prefer the non-`Attempt2` numbered script unless the user says otherwise.
