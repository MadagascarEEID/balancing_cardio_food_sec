## RUN RMD THROUHG LINE 527
metabolic_syndrome_dataset <- model_data |>
  mutate(
    high_total_cholesterol = ifelse(tot_chol_lb >= 200, 1L, 0L),
    high_ldl_cholesterol   = ifelse(ldl_lb >= 100, 1L, 0L),
    high_triglycerides     = ifelse(tri_lb >= 150, 1L, 0L),
    high_glucose           = ifelse(glucose >= 100, 1L, 0L),
    low_hdl_cholesterol    = case_when(
      gender == 1 & hdl_lb < 40 ~ 1L,
      gender == 0 & hdl_lb < 50 ~ 1L,
      TRUE ~ 0L
    ),
    high_waste_circumference    = case_when(
      gender == 1 & waist_circumference >= 102 ~ 1L,
      gender == 0 & waist_circumference >= 88 ~ 1L,
      TRUE ~ 0L
    )
  ) |>
  mutate(
    high_cardiochek_binary = case_when(
      high_total_cholesterol == 1 | high_ldl_cholesterol == 1 |
        high_triglycerides == 1 | high_glucose == 1 ~ 1L,
      TRUE ~ 0L
    ),
    high_bp_for_mets = ifelse(average_systolic >= 130 | average_diastolic >= 85, 1L, 0L)
  ) |>
  mutate(
    metabolic_syndrome_score = rowSums(
      cbind(high_glucose, high_bp_for_mets, high_triglycerides, low_hdl_cholesterol, high_waste_circumference)
    ),
    metabolic_syndrome_binary = ifelse(metabolic_syndrome_score>=3, 1, 0)
  )



# Village random intercepts on all equations.
priors_path_met_s <- c(
  ## -------------------------------------------------
  ## market_food_index (Gaussian)
  ## -------------------------------------------------
  prior(normal(0, 1),   class = b,         resp = marketfoodindex),
  prior(normal(0, 2),   class = Intercept, resp = marketfoodindex),
  prior(normal(0, 0.5), class = sd,        resp = marketfoodindex, group = village),
  prior(exponential(1), class = sigma,     resp = marketfoodindex),
  
  
  ## -------------------------------------------------
  ## Metabolic syndrome (Bernoulli)
  ## -------------------------------------------------
  prior(normal(0, 1.5), class = b,         resp = metabolicsyndromebinary),
  prior(normal(0, 2),   class = Intercept, resp = metabolicsyndromebinary),
  prior(normal(0, 0.5), class = sd,        resp = metabolicsyndromebinary, group = village),
  
  ## -------------------------------------------------
  ## Food insecurity (Bernoulli)
  ## -------------------------------------------------
  prior(normal(0, 1.5), class = b,         resp = foodinsecurity),
  prior(normal(0, 2),   class = Intercept, resp = foodinsecurity),
  prior(normal(0, 0.5), class = sd,        resp = foodinsecurity, group = village)
)

set.seed(24)
#
fit_path_met_s <- brm(
  formula =
    # a-path: MI → diet mediator
    bf(market_food_index ~ cgsol_z + hsol_z + age + gender + (1 | village), family = gaussian()) +
    
    # predictors of binary metabolic syndrome
    bf(metabolic_syndrome_binary ~ cgsol_z + hsol_z + market_food_index + age + gender + (1 | village))+
    
    # H1.2: food insecurity — bernoulli family
    bf(food_insecurity ~ cgsol_z + hsol_z + market_food_index + age + gender + (1 | village)) +
    set_rescor(FALSE),

  data    = metabolic_syndrome_dataset,
  family  = bernoulli(link = "logit"),           # default family for all equations without explicit family
  prior   = priors_path_met_s,
  chains  = 4, iter = 4000, warmup = 1000, cores = 4, seed = 24,
  control = list(adapt_delta = 0.9995, max_treedepth = 12))

summary(fit_path_met_s)

fixef(fit_path_met_s, probs = c(0.055, 0.945))


draws <- as_draws_df(fit_path_met_s)

# a-path: cgsol_z → market_food_index (substantive MI predictor)
a <- draws$b_marketfoodindex_cgsol_z

indirect_effects <- tibble(
  metabolic_syndrome = a*draws$b_metabolicsyndromebinary_market_food_index,
  food_insecurity = a * draws$b_foodinsecurity_market_food_index)

# Summarise: median + 89% CI + directional posterior probability
indirect_summary <- indirect_effects |>
  pivot_longer(everything(), names_to = "outcome", values_to = "indirect") |>
  group_by(outcome) |>
  summarise(
    median = median(indirect),
    q5.5   = quantile(indirect, 0.055),
    q94.5  = quantile(indirect, 0.945),
    # direction depends on outcome: positive = worse for biomarkers, negative = better for FI
    p_pos  = mean(indirect > 0),
    p_neg  = mean(indirect < 0),
    .groups = "drop"
  )

print(indirect_summary)



direct_effects <- tibble(
  metabolic_syndrome = draws$b_metabolicsyndromebinary_market_food_index,
  food_insecurity = draws$b_foodinsecurity_market_food_index)

# Summarise directional posterior probability
direct_summary <- direct_effects |>
  pivot_longer(everything(), names_to = "outcome", values_to = "direct") |>
  group_by(outcome) |>
  summarise(
    p_pos  = mean(direct > 0),
    p_neg  = mean(direct < 0),
    .groups = "drop"
  )


print(direct_summary)

# saveRDS(fit_path_met_s, file = paste0(fits_dir, "fit_path_met_s_2026.08.10_cens.rds"))

pp_check(fit_path_met_s, resp = "metabolicsyndromebinary")

