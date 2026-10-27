# Balancing Cardiometabolic Health and Food Security in the Context of Market Integration in Northeast Madagascar

This repository contains analysis code for a study examining how market integration — measured by commercial goods ownership (CGSOL) and housing quality (HSOL) — relates to cardiometabolic health outcomes and food security in rural northeast Madagascar, with a secondary analysis of physical activity (PA).

## Study overview

Market integration may improve food security via access to purchased foods but could simultaneously worsen cardiometabolic health through dietary shifts and reduced physical activity. We use Bayesian multilevel path models to estimate direct and indirect (diet-mediated) effects of market integration on a panel of cardiometabolic biomarkers and food insecurity, with village-level random intercepts to account for community clustering.

**Primary exposure:** Commercial goods ownership score (CGSOL), a count of durable consumer goods owned (mobile phone, radio, TV, computer, refrigerator, generator, bicycle, motorcycle, car/truck, boat).

**Outcomes:** BMI, mean arterial pressure (MAP), triglycerides, glucose, LDL cholesterol, HDL cholesterol, C-reactive protein (CRP), body fat %, waist circumference, food insecurity (binary), and metabolic syndrome (binary).

**Mediator:** Market food consumption index (log-transformed average frequency of purchased/processed foods from the FFQ).

**Physical activity measures (actigraphy subsample, n ≈ 100):** Light, moderate, and vigorous activity minutes; inactivity minutes; ENMO; intensity gradient — derived from wrist-worn Axivity AX3 accelerometers processed with GGIR.

## Repository contents

| File | Description |
|------|-------------|
| `data_prep.R` | Data wrangling: merges long/short survey, Cardiochek, FFQ, CRP, and actigraphy data; computes CGSOL, HSOL, MAP, BMI, and market food index; writes `cardio_food_sec_dataset.csv` |
| `GGIR Redux.R` | GGIR actigraphy processing pipeline for raw Axivity AX3 files; outputs day-level PA summaries used by `data_prep.R` |
| `Cardiometabolic Health and Food Security CENS.Rmd` | **Primary analysis.** Interval-censored Bayesian path model (`brms`) for below-LLOQ biomarkers (triglycerides < 50 mg/dL, LDL not calculable, HDL < 20 mg/dL); produces main effect plots and indirect-effect summaries |
| `Cardiometabolic Health and Food Security NO CENS.Rmd` | **Sensitivity analysis.** Identical model structure but drops below-LLOQ observations (complete-case) instead of using interval censoring |
| `Cardiometabolic Health and PA.Rmd` | **PA sub-analysis.** Bayesian models of predictors of PA, and effects of PA and CGSOL (jointly and separately) on cardiometabolic outcomes in the actigraphy subsample |
| `metabolic_syndrome_analysis.R` | Separate path model for metabolic syndrome (binary) and food insecurity as joint outcomes |
| `actigraphy_subsample_biomarker_plots.R` | Descriptive histograms of cardiometabolic biomarkers for the actigraphy subsample |
| `writing_model_xlsxs.R` | Exports `fixef()` summaries from all models to `.xlsx` supplementary tables |
| `cardio_food_sec notes.R` | Scratch calculations: back-transforming log-scale estimates to percent change; posterior probability summaries for secondary predictors |

## Data

Raw data are stored in Box and are not included in this repository. The processed analytical dataset (`cardio_food_sec_dataset.csv`) is produced by `data_prep.R` and read by the analysis Rmds. Fitted model objects (`.rds` files) are saved to a local `fits/` directory and read back to avoid re-running long chains.

## Models

All models are fit with [`brms`](https://paul-buerkner.github.io/brms/) (Stan back-end). The primary path model is a multivariate (`mvbf`) joint model with:

- **a-path:** CGSOL → market food index
- **b/c-paths:** market food index + CGSOL → each health/food security outcome
- Village random intercepts on all equations
- Interval censoring (`| cens(cens_var, y2 = ub_var)`) for below-LLOQ lipids in the primary analysis
- 4 chains × 3000 post-warmup draws; `adapt_delta = 0.9995`

Effect plots use full posterior draws visualized as half-eye density plots (`ggdist::stat_halfeye`, 89% credible intervals).

## Dependencies

```r
tidyverse, brms, tidybayes, bayesplot, ggdist, patchwork, gt, corrplot, readxl, writexl, GGIR, GGIRread
```

## Citation

Kolinski, L. et al. (in preparation). *Balancing cardiometabolic health and food security in the context of market integration in northeast Madagascar.*
