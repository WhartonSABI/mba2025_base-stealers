## Project Description

This project models optimal primary lead distance from first base using a situational run-expectancy objective and nested mixed-effects logistic models.

The active manuscript workflow is `paper/`.

## Canonical Pipeline

Run all stages from the project root:

```bash
Rscript code/run_all.R
```

Stages:

1. `code/01_fit_models_situational.R`
2. `code/02_optimize_leads_parallel.R`
3. `code/03_make_paper_figures.R`
4. `code/04_pca_case_study_figures.R`

## Parallelism

- Long compute stages use `n_cores` auto-detected from system cores.
- Override with `N_CORES`, for example:

```bash
N_CORES=4 Rscript code/run_all.R
```

- Fallback default is `12` when cores cannot be detected.

## Dependencies

If local packages are installed into `r_lib/`, scripts automatically add that library path.

Required packages include:

- `dplyr`
- `ggplot2`
- `tidyr`
- `lme4`

## Archive

Legacy scripts are stored under `archive/code/`.
