sd_log_bmi_prescale <- full_dataset |>
       filter(participant_id %in% study_participant_ids) |>   # match model_data's filter
       mutate(log_BMI = log(BMI)) |>
       pull(log_BMI) |>
       sd(na.rm = TRUE)

sd_log_bmi_prescale


pct_change <- (exp(0.2110377989 * sd_log_bmi_prescale) - 1) * 100
pct_change


sd_log_ldl_prescale <- full_dataset |>
  filter(participant_id %in% study_participant_ids) |>   # match model_data's filter
  mutate(log_ldl  = log(ldl_cholesterol)) |>
  pull(log_ldl) |>
  sd(na.rm = TRUE)

sd_log_ldl_prescale


pct_change <- (exp(0.138937281 * sd_log_ldl_prescale) - 1) * 100
pct_change

sd_log_glucose_prescale <- full_dataset |>
  filter(participant_id %in% study_participant_ids) |>   # match model_data's filter
  mutate(log_glucose  = log(glucose)) |>
  pull(log_glucose) |>
  sd(na.rm = TRUE)

sd_log_glucose_prescale


pct_change <- (exp(0.127891594 * sd_log_glucose_prescale) - 1) * 100
pct_change


sd_log_market_food <- model_data |>
  mutate(log_market_food_index = log(market_food_index_raw + 1)) |>
  pull(log_market_food_index) |>
  sd(na.rm = TRUE)

sd_log_market_food

pct_change <- (exp(0.246129450 * sd_log_market_food) - 1) * 100
pct_change


direct_effects_market_foods <- tibble(
  Age        =  draws$b_marketfoodindex_age,
  Male             =  draws$b_marketfoodindex_gender,
  HSOL   = draws$b_marketfoodindex_hsol_z,
  CGSOL         =  draws$b_marketfoodindex_cgsol_z)

# Summarise directional posterior probability
direct_summary_marketfood <- direct_effects_market_foods |>
  pivot_longer(everything(), names_to = "outcome", values_to = "direct") |>
  group_by(outcome) |>
  summarise(
    p_pos  = mean(direct > 0),
    p_neg  = mean(direct < 0),
    .groups = "drop"
  )



# direct effects gender
direct_effects_gender <- tibble(
  MAP        =  draws$b_logMAP_gender,
  Glucose             =  draws$b_logglucose_gender)

# Summarise directional posterior probability
direct_effects_gender <- direct_effects_gender |>
  pivot_longer(everything(), names_to = "outcome", values_to = "direct") |>
  group_by(outcome) |>
  summarise(
    p_pos  = mean(direct > 0),
    p_neg  = mean(direct < 0),
    .groups = "drop"
  )

direct_effects_hsol <- tibble(
  MAP        =  draws$b_logMAP_hsol_z,
  Glucose             =  draws$b_logglucose_hsol_z,
  HDL             =  draws$b_loghdllb_hsol_z,
  BMI =  draws$b_logBMI_hsol_z,
  triglycerides = draws$b_logtrilb_hsol_z)
  
  

# Summarise directional posterior probability
direct_effects_hsol <- direct_effects_hsol |>
  pivot_longer(everything(), names_to = "outcome", values_to = "direct") |>
  group_by(outcome) |>
  summarise(
    p_pos  = mean(direct > 0),
    p_neg  = mean(direct < 0),
    .groups = "drop"
  )

direct_effects_fi <- tibble(
  gender        =  draws$b_foodinsecurity_gender,
  age             =  draws$b_foodinsecurity_age,
  hsol             =  draws$b_foodinsecurity_hsol_z,
  cgsol             =  draws$b_foodinsecurity_cgsol_z,
  marketfood        =  draws$b_foodinsecurity_market_food_index)

# Summarise directional posterior probability
direct_effects_fi <- direct_effects_fi |>
  pivot_longer(everything(), names_to = "outcome", values_to = "direct") |>
  group_by(outcome) |>
  summarise(
    p_pos  = mean(direct > 0),
    p_neg  = mean(direct < 0),
    .groups = "drop"
  )
