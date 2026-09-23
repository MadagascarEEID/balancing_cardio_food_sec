## 1.1 Packages and data loading
library(tidyverse)
library(readxl)

# ── Data loading ──────────────────────────────────────────────────────────────
long_survey_data <- read.csv("/Users/levkolinski/Library/CloudStorage/Box-Box/EEID_Data_public/clean_data_tables/NSF Survey Data/long_survey.csv")

short_survey_data <- read.csv("/Users/levkolinski/Library/CloudStorage/Box-Box/EEID_Data_public/clean_data_tables/NSF Survey Data/short_survey.csv")


cardiochek_data <- read.csv("/Users/levkolinski/Library/CloudStorage/Box-Box/NSF Data/Survey Data/Lev Kolinski Data/Cardiochek/cardiochek_data.csv")
colnames(cardiochek_data) <- tolower(colnames(cardiochek_data))

ffq_data <- read.csv("/Users/levkolinski/Library/CloudStorage/Box-Box/NSF Data/Survey Data/Lev Kolinski Data/ffq.csv")
colnames(ffq_data) <- tolower(colnames(ffq_data))

ax3_data_fp <- "/Users/levkolinski/Library/CloudStorage/Box-Box/NSF Data/Survey Data/Lev Kolinski Data/Axivity/GGIR Outputs/"

actigraphy_data <- rbind(
  read.csv(paste0(ax3_data_fp, "GGIR Outputs Aug 2026/output_August 9-16/results/part5_daysummary_MM_L30M100V400_T5A5.csv")),
  read.csv(paste0(ax3_data_fp, "GGIR Outputs Aug 2026/output_August 16-23/results/part5_daysummary_MM_L30M100V400_T5A5.csv")),
  read.csv(paste0(ax3_data_fp, "GGIR Outputs Aug 2026/output_August 23-30/results/part5_daysummary_MM_L30M100V400_T5A5.csv")),
  read.csv(paste0(ax3_data_fp, "GGIR Outputs Aug 2026/output_August 30 - September 6/results/part5_daysummary_MM_L30M100V400_T5A5.csv")))

actigraphy_data$participant_id <- strtrim(actigraphy_data$ID, 4)

crp_data <- readxl::read_excel("/Users/levkolinski/Library/CloudStorage/Box-Box/NSF Data/Survey Data/Lev Kolinski Data/CRP ELISA RESULTS/CRP ELISA Results Master Sheet.xlsx",
                               sheet = "Assay Results")

# ── Data wrangling ──────────────────────────────────────────────────────────────

## Food Data (n=424) ----
food_data <- ffq_data |>
  dplyr::select(contains(c("participant_id", "food_insecurity", "total"))) |>
  dplyr::mutate(across(-c(participant_id, food_insecurity), as.numeric)) |> 
  ## dropping partiicpants who weren't asked about food insecurity (preliminary FFQ)
  filter(!is.na(food_insecurity)) |> 
  mutate(participant_id = ifelse(participant_id == "NDF7038", "NSF7038", participant_id),
         participant_id = ifelse(participant_id == "NSF705", "NSF7025", participant_id))

study_participant_ids<- food_data |> pull(participant_id)

## Long Survey Health Data (n=273)----
long_survey_health_data <- long_survey_data |>
  filter(participant_id %in% study_participant_ids) |> 
  dplyr::select(c(participant_id, gender, age, systolic_1, diastolic_1, systolic_2, diastolic_2,
                  systolic_3, diastolic_3, height, weight, body_fat)) |>
  mutate(across(contains("systolic"), as.numeric)) |>
  mutate(across(contains("diastolic"), as.numeric)) |>
  mutate(across(contains("height"), as.numeric)) |>
  mutate(across(contains("weight"), as.numeric)) |>
  mutate(age = as.numeric(age)) |>
  mutate(body_fat = as.numeric(body_fat)) |> 
  mutate(average_systolic  = (systolic_1 + systolic_2 + systolic_3) / 3) |>
  mutate(average_diastolic = (diastolic_1 + diastolic_2 + diastolic_3) / 3) |>
  mutate(MAP = average_diastolic + (average_systolic - average_diastolic) / 3) |>
  mutate(BMI = weight / (height)^2) |> 
  mutate(BMI = ifelse(BMI < 10|BMI > 60, NA, BMI))

## Long Survey SOL Data (confirmed no NAs) (n = 273)----
long_survey_sol_data <- long_survey_data |>
  filter(participant_id %in% study_participant_ids) |> 
  mutate(material_wall_index = 0) |>  # bamboo, rafia, travelers_palm default to 0
  mutate(material_wall_index = if_else(material_wall == "wood",          1, material_wall_index)) %>%
  mutate(material_wall_index = if_else(material_wall == "burnt_brick",   2, material_wall_index)) %>%
  mutate(material_wall_index = if_else(material_wall == "metal_sheets",  3, material_wall_index)) %>%
  mutate(material_wall_index = if_else(material_wall == "cement",        4, material_wall_index)) %>%
  mutate(material_roof_index = 0) %>%  # bamboo, thatch default to 0
  mutate(material_roof_index = if_else(material_roof == "metal_sheets",  1, material_roof_index)) %>%
  mutate(material_roof_index = if_else(material_roof == "cement",        2, material_roof_index)) %>%
  mutate(material_floor_index = 0) %>%  # bamboo, dirt, travelers_palm default to 0
  mutate(material_floor_index = if_else(material_floor == "wood",   1, material_floor_index)) %>%
  mutate(material_floor_index = if_else(material_floor == "cement", 2, material_floor_index)) %>%
  mutate(cgsol = rowSums(across(c(durable_goods_owned_mobile_phone,
                                  durable_goods_owned_radio,
                                  durable_goods_owned_television,
                                  durable_goods_owned_computer,
                                  durable_goods_owned_refrigerator,
                                  durable_goods_owned_generator,
                                  durable_goods_owned_bicycle,
                                  durable_goods_owned_motorcycle,
                                  durable_goods_owned_car_truck,
                                  durable_goods_owned_boat),
                                as.numeric),
                         na.rm = TRUE)) |>
  select(participant_id, village, cgsol, material_floor_index, material_wall_index,
         material_roof_index, waist_circumference)


## Short Survey Health Data (n = 151)----
short_survey_health_data <- short_survey_data |>
  filter(participant_id %in% study_participant_ids) |> 
  select(c(participant_id, gender, age, systolic_1, diastolic_1, systolic_2, diastolic_2,
           systolic_3, diastolic_3, height, weight)) |>
  mutate(across(contains("systolic"), as.numeric)) |>
  mutate(across(contains("diastolic"), as.numeric)) |>
  mutate(across(contains("height"), as.numeric)) |>
  mutate(across(contains("weight"), as.numeric)) |>
  mutate(age = as.numeric(age)) |>
  mutate(average_systolic  = (systolic_1 + systolic_2 + systolic_3) / 3) |>
  mutate(average_diastolic = (diastolic_1 + diastolic_2 + diastolic_3) / 3) |>
  mutate(MAP = average_diastolic + (average_systolic - average_diastolic) / 3) |>
  mutate(BMI = weight / (height)^2) |> 
  mutate(BMI = ifelse(BMI < 10|BMI > 60, NA, BMI)) |> 
  mutate(waist_circumference = NA)

## Short Survey SOL Data (n= 151)----
### confirmed no NAs in house mats
short_survey_sol_data <- short_survey_data |>
  filter(participant_id %in% study_participant_ids) |> 
  mutate(material_wall_index = 0) |>
  mutate(material_wall_index = if_else(material_wall == "Wood – Bamboo",                  0, material_wall_index)) %>%
  mutate(material_wall_index = if_else(material_wall == "Rafia",                          0, material_wall_index)) %>%
  mutate(material_wall_index = if_else(material_wall == "Traveler's palm",                0, material_wall_index)) %>%
  mutate(material_wall_index = if_else(material_wall == "Compacted earth",                0, material_wall_index)) %>%
  mutate(material_wall_index = if_else(material_wall == "Mud",                            0, material_wall_index)) %>%
  mutate(material_wall_index = if_else(material_wall == "Mud brick (unfired)",            0, material_wall_index)) %>%
  mutate(material_wall_index = if_else(material_wall == "Wood – Planks",                 1, material_wall_index)) %>%
  mutate(material_wall_index = if_else(material_wall == "Burnt brick",                      2, material_wall_index)) %>%
  mutate(material_wall_index = if_else(trimws(material_wall) == "Metal sheets (Iron, aluminum)", 3, material_wall_index)) %>%
  mutate(material_wall_index = if_else(material_wall == "Concrete/cement",                  4, material_wall_index)) %>%
  mutate(material_roof_index = 0) %>%
  mutate(material_roof_index = if_else(material_roof == "Thatch",                           0, material_roof_index)) %>%
  mutate(material_roof_index = if_else(trimws(material_roof) == "Metal sheets (Iron, aluminum)", 1, material_roof_index)) %>%
  mutate(material_roof_index = if_else(material_roof == "Concrete/Cement",                  2, material_roof_index)) %>%
  mutate(material_floor_index = 0) %>%
  mutate(material_floor_index = if_else(material_floor == "Sand/Dirt",          0, material_floor_index)) %>%
  mutate(material_floor_index = if_else(material_floor == "Wood – Bamboo", 0, material_floor_index)) %>%
  mutate(material_floor_index = if_else(material_floor == "Traveler's palm",    0, material_floor_index)) %>%
  mutate(material_floor_index = if_else(material_floor == "Wood – Planks", 1, material_floor_index)) %>%
  mutate(material_floor_index = if_else(material_floor == "Smooth cement",      2, material_floor_index)) %>%
  mutate(cgsol = rowSums(across(c(durable_goods_owned_mobile_phone,
                                  durable_goods_owned_radio,
                                  durable_goods_owned_television,
                                  durable_goods_owned_computer,
                                  durable_goods_owned_refrigerator,
                                  durable_goods_owned_generator,
                                  durable_goods_owned_bicycle,
                                  durable_goods_owned_motorcycle,
                                  durable_goods_owned_car_truck,
                                  durable_goods_owned_boat),
                                as.numeric),
                         na.rm = TRUE)) |>
  select(participant_id, village, cgsol, material_floor_index, material_wall_index,
         material_roof_index)


## Cardiochek Data (n=393)----
cardiochek_data <- cardiochek_data |>
  filter(participant_id %in% study_participant_ids) |> 
  ## dropping duplicate NSF7022 response....age doesn't match for R_mRIUzcXckCQEhdG
  filter(responseid!="R_mRIUzcXckCQEhdG") |> 
  select(-c(survey_date, responseid, age, gender, consent)) |>
  select(-contains("category"))   |> 
  ## removing non-fasting samples
  filter(!grepl("eat", notes)) |> 
  select(participant_id, total_cholesterol, hdl_cholesterol, triglycerides, glucose, ldl_cholesterol)

## CRP Data (n = 133) -----
crp_data <- crp_data |> 
  mutate(cv = as.numeric(cv),
         concentration_pg_ml_raw = as.numeric(concentration_pg_ml_raw),
         additional_dilution_factor = as.numeric(dilution),
         eluate_dilution = 167) |> 
  filter(cv<12,concentration_pg_ml_raw > 18.75,concentration_pg_ml_raw < 1200) |> 
  filter(!grepl("Control|Standard|Blank|Pool", sample_id),
         !(grepl("saturated", notes)),
         assay_number != 2) |> 
  group_by(sample_id) %>%
  slice_tail(n = 1) %>%
  ungroup() |> 
  rename(participant_id = sample_id) |> 
  filter(participant_id%in%study_participant_ids) |> 
  select(participant_id, additional_dilution_factor, concentration_pg_ml_raw) |> 
  filter(!is.na(concentration_pg_ml_raw)) |> 
  mutate(concentration_pg_ml_adjusted_for_dilutions = concentration_pg_ml_raw*additional_dilution_factor) |> 
  mutate(serum_equivalent_mg_L = 1E-6*((355.67*concentration_pg_ml_adjusted_for_dilutions)-59884.10)) |> 
  select(participant_id, serum_equivalent_mg_L)

## Actigraphy Summary (person-level) (n=103) ----
actigraphy_summary <- actigraphy_data |>
  mutate(
    light_min      = dur_day_total_LIG_min,
    moderate_min   = dur_day_total_MOD_min,
    vigorous_min   = dur_day_total_VIG_min,
    inactivity_min = dur_day_total_IN_min,
    ENMO           = ACC_day_mg,
    intensity_gradient = ig_day_gradient
  ) |>
  group_by(participant_id) |>
  summarise(
    mean_light_min    = mean(light_min,      na.rm = TRUE),
    mean_moderate_min = mean(moderate_min,   na.rm = TRUE),
    mean_vigorous_min = mean(vigorous_min,   na.rm = TRUE),
    mean_inactive_min = mean(inactivity_min, na.rm = TRUE),
    mean_ENMO         = mean(ENMO,           na.rm = TRUE),
    mean_IG           = mean(intensity_gradient, na.rm = TRUE),
    n_valid_days      = n(),
    .groups = "drop"
  )

## Merging ----
### n = 267
full_dataset_long_survey <- cardiochek_data |>
  inner_join(long_survey_health_data,  by = "participant_id") |>
  inner_join(long_survey_sol_data,     by = "participant_id") |>
  left_join(actigraphy_summary,        by = "participant_id") |> 
  left_join(crp_data,        by = "participant_id")

### n = 126
full_dataset_short_survey <- cardiochek_data |>
  inner_join(short_survey_health_data, by = "participant_id") |>
  inner_join(short_survey_sol_data,    by = "participant_id") |>
  left_join(actigraphy_summary,        by = "participant_id") |>
  left_join(crp_data,        by = "participant_id") |>
  mutate("body_fat" = NA, .after = "BMI")


### n = 393
full_dataset <- rbind(full_dataset_long_survey, full_dataset_short_survey) |>
  # hsol: sum raw ordinal scores across wall, roof, floor then scale once
  mutate(hsol = material_wall_index + material_floor_index + material_roof_index) |>
  mutate(
    cgsol = as.numeric(cgsol),   # substantive MI predictor: consumer goods
    hsol  = as.numeric(hsol)   # control variable: housing quality
  ) |>
  rename(crp = serum_equivalent_mg_L) |> 
  filter(!is.na(MAP)) |> 
  filter(!is.na(BMI))


# ── Market food index ─────────────────────────────────────────────────────────
# Market/processed items
market_food_cols <- c(
  "total_bread", "total_pasta", "total_chips", "total_cookies_sweets",
  "total_soda", "total_beer", "total_butter_oil",
  "total_dairy"
)

ffq_market <- ffq_data |>
  mutate(across(all_of(market_food_cols), as.numeric)) |>
  mutate(market_food_index = rowMeans(pick(all_of(market_food_cols)), na.rm = TRUE)) |>
  select(participant_id, market_food_index)

# Final sample size of N = 381
full_dataset <- full_dataset |>
  left_join(ffq_market, by = "participant_id") |> 
  filter(!is.na(market_food_index)) 



# ── writing CSV ─────────────────────────────────────────────────────────
colnames(full_dataset)

full_dataset_for_csv <- full_dataset |> 
  select(participant_id, village, gender, age, cgsol, hsol, market_food_index,
         glucose, total_cholesterol, hdl_cholesterol, ldl_cholesterol, triglycerides,
         MAP, BMI, body_fat, waist_circumference, crp, n_valid_days, mean_inactive_min,
         mean_light_min, mean_moderate_min, mean_vigorous_min, mean_ENMO, mean_IG)

write.csv(full_dataset_for_csv, row.names = FALSE,
          file = "/Users/levkolinski/Library/CloudStorage/Box-Box/NSF Data/Survey Data/Lev Kolinski Data/cardio_food_sec_dataset.csv")
