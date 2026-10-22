library(writexl)
library(tidyverse)
library(brms)
# Main analysis path model  ----
# All responses in order
response_map <- c(
  "marketfoodindex" = "Market food index (log + 1)",
  "logBMI"          = "BMI (log)",
  "logMAP"          = "MAP (log)",
  "logtrilb"        = "Triglycerides (log, interval-censored)",
  "logglucose"      = "Glucose (log)",
  "logldllb"        = "LDL (log, interval-censored)",
  "loghdllb"        = "HDL (log, interval-censored)",
  "foodinsecurity"  = "Food insecurity (log-odds)"
)

predictor_map <- c(
  "cgsol_z"           = "Goods-based MI (z)",
  "hsol_z"            = "Housing-based MI (z)",
  "market_food_index" = "Market food index (log + 1, z)",
  "age"               = "Age (z)",
  "gender"            = "Sex (male = 1)",
  "Intercept"         = "Intercept"
)

main_supp <- fixef(fit_path, probs = c(0.055, 0.945)) |>
  as.data.frame() |>
  tibble::rownames_to_column("parameter") |>
  # Match response prefix and predictor suffix
  mutate(
    response_key  = purrr::map_chr(parameter, function(p) {
      names(response_map)[purrr::detect_index(names(response_map), ~ startsWith(p, .))]
    }),
    predictor_key = purrr::map_chr(parameter, function(p) {
      names(predictor_map)[purrr::detect_index(names(predictor_map), ~ endsWith(p, .))]
    }),
    Response  = response_map[response_key],
    Predictor = predictor_map[predictor_key]
    ) |>
  filter(predictor_key != "Intercept") |>   # drop intercepts for cleaner table; remove if you want them
  select(
    Response, Predictor,
    Median   = Estimate,
    Lower_89 = Q5.5,
    Upper_89 = Q94.5
  ) |>
  arrange(Response, Predictor)

writexl::write_xlsx(
  list(`Main model` = main_supp),
  path = "/Users/levkolinski/Library/CloudStorage/Box-Box/EEID_Data_public/manuscripts/cardiometabolic_health_food_security/supp_table_main_path_model.xlsx"
)

# Metabolic Syndrome Path Model -----
fit_path_met_s<-readRDS("/Users/levkolinski/Desktop/LK_Cardiometabolic_Health/Analyses/cardio_food_sec/fits/fit_path_met_s_2026.08.10_cens.rds")

met_s_response_map <- c(
  "marketfoodindex"          = "Market food index",
  "metabolicsyndromebinary"  = "Metabolic syndrome (log-odds)",
  "foodinsecurity"           = "Food insecurity (log-odds)"
)

met_s_predictor_map <- c(
  "cgsol_z"           = "Goods-based MI (z)",
  "hsol_z"            = "Housing-based MI (z)",
  "market_food_index" = "Market food index (mediator)",
  "age"               = "Age (z)",
  "gender"            = "Sex (male = 1)",
  "Intercept"         = "Intercept"
)

met_s_supp <- fixef(fit_path_met_s, probs = c(0.055, 0.945)) |>
  as.data.frame() |>
  tibble::rownames_to_column("parameter") |>
  mutate(
    response_key = purrr::map_chr(parameter, function(p) {
      hit <- names(met_s_response_map)[purrr::detect_index(names(met_s_response_map), ~ startsWith(p, .))]
      if (length(hit) == 0 || hit == "") NA_character_ else hit
    }),
    predictor_key = purrr::map_chr(parameter, function(p) {
      hit <- names(met_s_predictor_map)[purrr::detect_index(names(met_s_predictor_map), ~ endsWith(p, .))]
      if (length(hit) == 0 || hit == "") NA_character_ else hit
    }),
    Response  = met_s_response_map[response_key],
    Predictor = met_s_predictor_map[predictor_key]
  ) |>
  filter(predictor_key != "Intercept") |>
  select(
    Response, Predictor,
    Median   = Estimate,
    Lower_89 = Q5.5,
    Upper_89 = Q94.5
  ) |>
  arrange(Response, Predictor)

writexl::write_xlsx(
    list(`Metabolic syndrome Path`     = met_s_supp),
    path = "/Users/levkolinski/Library/CloudStorage/Box-Box/EEID_Data_public/manuscripts/cardiometabolic_health_food_security/supp_table_met_syndrome_path_model.xlsx")


# Predictors of PA ----
fits_pa_outcomes<- readRDS(paste0(fits_dir, "predictors_PA_2026.08.17.rds"))

predictor_map_pa <- c(
  "cgsol_z"           = "Goods-based MI (z)",
  "hsol_z"            = "Housing-based MI (z)",
  "market_food_index" = "Market food index (log + 1, z)",
  "age"               = "Age (z)",
  "gender"            = "Sex (male = 1)",
  "villageMandena" = "Village (Mandena vs Antafiamora)",
  "Intercept"         = "Intercept"
)
pa_outcome_labels <- c(
  "mean_light_min_z"           = "Light PA (min/day, z-scored)",
  "mean_moderate_min_z"        = "Moderate PA (min/day, z-scored)",
  "mean_vigorous_min_sqrt_z"   = "Vigorous PA (√min/day, z-scored)",
  "mean_inactive_min_z"        = "Inactivity (min/day, z-scored)",
  "mean_ENMO_z"                = "ENMO (mg, z-scored)",
  "mean_IG_z"                  = "Intensity Gradient (z-scored)"
)


pa_outcomes_supp <- purrr::imap_dfr(fits_pa_outcomes, function(fit, outcome_key) {
  fixef(fit, probs = c(0.055, 0.945)) |>
    as.data.frame() |>
    tibble::rownames_to_column("predictor_key") |>
    mutate(
      Outcome   = pa_outcome_labels[outcome_key],
      Predictor = coalesce(predictor_map_pa[predictor_key], predictor_key)
    )
}) |>
  filter(predictor_key != "Intercept") |>
  select(
    Outcome, Predictor,
    Median   = Estimate,
    Lower_89 = Q5.5,
    Upper_89 = Q94.5
  ) |>
  arrange(Outcome, Predictor)

writexl::write_xlsx(
  list(`PA Predictors` = pa_outcomes_supp),
  path = "/Users/levkolinski/Library/CloudStorage/Box-Box/EEID_Data_public/manuscripts/cardiometabolic_health_food_security/supp_table_pa_predictors.xlsx"
)

# How PA predicts health (not adjusted for CGSOL) ----
pa_health_fixefs_summary <- readRDS("~/Desktop/LK_Cardiometabolic_Health/Analyses/cardio_food_sec/fits/pa_health_fixefs_summary.rds")

outcome_labels_full <- c(
  "MAP"           = "Mean arterial pressure (mmHg)",
  "CRP"           = "CRP (log-scaled)",
  "body_fat"      = "Body fat % (log-scaled)",
  "waist_circ"    = "Waist circumference (cm, z-scored)",
  "bmi"           = "BMI (kg/m², z-scored)",
  "glucose"       = "Fasting glucose (z-scored)",
  "triglycerides" = "Triglycerides (log-scaled)",
  "ldl"           = "LDL cholesterol (log-scaled)",
  "hdl"           = "HDL cholesterol (log-scaled)",
  "met_s"         = "Metabolic syndrome (binary, log-odds)"
)

exposure_labels_full <- c(
  "mean_light_min_z"           = "Light PA (min/day, z-scored)",
  "mean_moderate_min_z"        = "Moderate PA (min/day, z-scored)",
  "mean_vigorous_min_sqrt_z"   = "Vigorous PA (√min/day, z-scored)",
  "mean_inactive_min_z"        = "Inactivity (min/day, z-scored)",
  "mean_ENMO_z"                = "ENMO (mg, z-scored)",
  "mean_IG_z"                  = "Intensity Gradient (z-scored)"
)

supp_xlsx <- purrr::imap_dfr(pa_health_fixefs_summary, function(exposure_list, exposure_key) {
  purrr::imap_dfr(exposure_list, function(fe_mat, outcome_key) {
    row <- fe_mat[exposure_key, ]
    tibble::tibble(
      exposure = exposure_key,
      outcome  = outcome_key,
      estimate = row[["Estimate"]],
      lower    = row[["Q5.5"]],
      upper    = row[["Q94.5"]]
    )
  })
}) |>
  mutate(
    Exposure = coalesce(exposure_labels_full[exposure], exposure),
    Outcome  = coalesce(outcome_labels_full[outcome],  outcome)
    )|>
  select(
    Exposure, Outcome,
    Median   = estimate,
    Lower_89 = lower,
    Upper_89 = upper,
  ) |>
  arrange(Exposure, Outcome)

writexl::write_xlsx(
  list(`PA Health Outcomes` = supp_xlsx),
  path = "/Users/levkolinski/Library/CloudStorage/Box-Box/EEID_Data_public/manuscripts/cardiometabolic_health_food_security/supp_table_pa_health_60models.xlsx"
)

# How CGSOL predicts Health (PA Sub Analysis)----

fits_cgsol_no_PA<- readRDS("/Users/levkolinski/Desktop/LK_Cardiometabolic_Health/Analyses/cardio_food_sec/fits/fits_cgsol_health_no_mvpa_2026.08.25.rds")

cgsol_no_pa_supp <- purrr::imap_dfr(fits_cgsol_no_PA, function(fit, outcome_key) {
  fixef(fit, probs = c(0.055, 0.945)) |>
    as.data.frame() |>
    tibble::rownames_to_column("predictor_key") |>
    mutate(
      Outcome   = coalesce(outcome_labels_full[outcome_key], outcome_key),
      Predictor = coalesce(predictor_map_pa[predictor_key], predictor_key)
    )
}) |>
  filter(predictor_key != "Intercept") |>
  select(
    Outcome, Predictor,
    Median   = Estimate,
    Lower_89 = Q5.5,
    Upper_89 = Q94.5
  ) |>
  arrange(Outcome, Predictor)


writexl::write_xlsx(
  list(`CGSOL Health Outcomes PA Sub Analysis` = cgsol_no_pa_supp),
  path = "/Users/levkolinski/Library/CloudStorage/Box-Box/EEID_Data_public/manuscripts/cardiometabolic_health_food_security/supp_table_cgsol_health_pa_sub.xlsx"
)

