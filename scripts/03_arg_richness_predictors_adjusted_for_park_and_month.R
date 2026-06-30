# 1 Load libraries and helper functions
source("./required_packages/setup_packages_and_dependencies_R.R")
source("./scripts/helper_functions.R")

# 2 Set input and output file paths

sample_richness_file_path <- "./data/arg_richness/sample_level_arg_richness.csv"
unadjusted_coefficient_file_path <- "./data/predictor_rankings/arg_richness_predictor_coefficients.csv"
output_folder <- "./data/predictor_rankings"

<<<<<<< HEAD
=======
required_input_files <- c(
  sample_richness_file_path,
  unadjusted_coefficient_file_path
)

if (any(!file.exists(required_input_files))) {
  stop(
    paste(
      "Required inputs are missing.",
      "Files scripts/01_calculate_arg_richness.R and",
      "02-rank_predictors_arg_richness.R should have been ran first." ))
}

# Create this script's output folder if needed

dir.create(
  output_folder,
  recursive = T,
  showWarnings = F
)

adjusted_coefficient_output_file <- file.path(
  output_folder,
  "arg_richness_predictors_adjusted_for_park_month.csv"
)
comparison_output_file <- file.path(
  output_folder,
  "arg_richness_predictors_unadjusted_vs_adjusted.csv"
)

adjusted_model_ranking_output_file <- file.path(
  output_folder,
  "arg_richness_predictor_adjusted_aicc_model_ranking.csv"
)

# 3 Read input files
sample_richness_df <- read_csv(
  sample_richness_file_path,
  show_col_types = F
)
unadjusted_coefficient_table <- read_csv(
  unadjusted_coefficient_file_path,
  show_col_types = F
)

# 4 Get AMR gene panel size
n_genes_tested <- unique(sample_richness_df$n_unique_genes_reported)
n_genes_tested <- as.numeric(n_genes_tested)


# 5 Fit global adjusted null model with park and month only

# adjusted_null_model_data <- sample_richness_df %>%
#   filter(
#     !is.na(arg_richness),
#     !is.na(park_name),
#     !is.na(sample_month)
#   ) %>%
#   mutate(
#     arg_detection_proportion = arg_richness / n_genes_tested,
#     n_genes_tested_per_sample = n_genes_tested,
#     park_name = as.factor(park_name),
#     sample_month = as.factor(sample_month)
#   )
# 
# adjusted_null_model <- glm(
#   arg_detection_proportion ~ park_name + sample_month,
#   family = binomial,
#   weights = n_genes_tested_per_sample,
#   data = adjusted_null_model_data
# )
# 
# adjusted_null_aic <- AIC(adjusted_null_model)
# 
# adjusted_null_aicc <- calculate_aicc(adjusted_null_model)


# 5Unadjusted analysis predictors
predictor_list_df <- unadjusted_coefficient_table %>%
  dplyr::select(
    predictor,
    predictor_type) %>%
  distinct()

# 6 check sample structure by park and month
park_sample_counts <- sample_richness_df %>%
  count(park_name) %>%
  arrange(park_name)

print(park_sample_counts)

month_sample_counts <- sample_richness_df %>%
  count(sample_month) %>%
  arrange(sample_month)

print(month_sample_counts)

park_month_sample_counts <- sample_richness_df %>%
  count(park_name, sample_month) %>%
  arrange(park_name, sample_month)

print(park_month_sample_counts)

write_csv(
  park_sample_counts,
  file.path(
    output_folder,
    "arg_richness_adjusted_model_samples_by_park.csv"
  )
)

write_csv(
  month_sample_counts,
  file.path(
    output_folder,
    "arg_richness_adjusted_model_samples_by_month.csv"
  )
)

write_csv(
  park_month_sample_counts,
  file.path(
    output_folder,
    "arg_richness_adjusted_model_samples_by_park_month.csv"
  )
)

# 7 Define function for fitting one adjusted model

fit_one_adjusted_arg_richness_model <- function(
    predictor_name,
    predictor_type,
    data,
    n_genes_tested) {
  
  model_data <- data %>%
    dplyr::select(
      arg_richness,
      park_name,
      sample_month,
      all_of(predictor_name)
    ) %>%
    filter(
      !is.na(arg_richness),
      !is.na(park_name),
      !is.na(sample_month),
      !is.na(.data[[predictor_name]])
    ) %>%
    mutate(
      arg_detection_proportion = arg_richness / n_genes_tested,
      n_genes_tested_per_sample = n_genes_tested,
      park_name = as.factor(park_name),
      sample_month = as.factor(sample_month)
    )
  
  if (predictor_type == "categorical") {
    model_data <- model_data %>%
      mutate(
        across(
          all_of(predictor_name),
          as.factor
        )
      )
  }
  
  model_formula <- as.formula(
    paste(
      "arg_detection_proportion ~",
      predictor_name,
      "+ park_name + sample_month"
    )
  )
  
  model <- glm(
    formula = model_formula,
    family = binomial,
    weights = n_genes_tested_per_sample,
    data = model_data
  )
  
  adjusted_null_model <- glm(
    arg_detection_proportion ~ park_name + sample_month,
    family = binomial,
    weights = n_genes_tested_per_sample,
    data = model_data
  )
  
  adjusted_null_aic <- AIC(adjusted_null_model)
  
  adjusted_null_aicc <- calculate_aicc(adjusted_null_model)
  
  adjusted_model_summary <- tibble(
    predictor = predictor_name,
    predictor_type = predictor_type,
    n_samples_adjusted = nrow(model_data),
    n_genes_tested = n_genes_tested,
    model_family = "binomial",
    adjusted_for_park = TRUE,
    adjusted_for_month = TRUE,
    aic = AIC(model),
    aicc = calculate_aicc(model),
    adjusted_null_aic = adjusted_null_aic,
    adjusted_null_aicc = adjusted_null_aicc
  )
  
  adjusted_coefficients <- broom::tidy(
    model,
    conf.int = TRUE,
    exponentiate = TRUE
  ) %>%
    filter(term != "(Intercept)") %>%
    filter(!str_detect(term, "^park_name")) %>%
    filter(!str_detect(term, "^sample_month")) %>%
    mutate(
      predictor = predictor_name,
      predictor_type = predictor_type,
      n_samples_adjusted = nrow(model_data),
      n_genes_tested = n_genes_tested,
      model_family = "binomial",
      adjusted_for_park = TRUE,
      adjusted_for_month = TRUE
    ) %>%
    rename(
      adjusted_odds_ratio = estimate,
      adjusted_conf_low = conf.low,
      adjusted_conf_high = conf.high
    ) %>%
    dplyr::select(
      predictor,
      predictor_type,
      term,
      n_samples_adjusted,
      n_genes_tested,
      model_family,
      adjusted_for_park,
      adjusted_for_month,
      adjusted_odds_ratio,
      adjusted_conf_low,
      adjusted_conf_high
    )
  
  list(
    adjusted_model_summary = adjusted_model_summary,
    adjusted_coefficients = adjusted_coefficients
  )
}
# 1 Load libraries and helper functions
source("./required_packages/setup_packages_and_dependencies_R.R")
source("./scripts/helper_functions.R")

# 2 Set input and output file paths

sample_richness_file_path <- "./data/arg_richness/sample_level_arg_richness.csv"
unadjusted_coefficient_file_path <- "./data/predictor_rankings/arg_richness_predictor_coefficients.csv"
output_folder <- "./data/predictor_rankings"

>>>>>>> WIP-predictor-ranking-analysis
adjusted_coefficient_output_file <- file.path(
  output_folder,
  "arg_richness_predictors_adjusted_for_park_month.csv"
)
comparison_output_file <- file.path(
  output_folder,
  "arg_richness_predictors_unadjusted_vs_adjusted.csv"
)

<<<<<<< HEAD
=======
adjusted_model_ranking_output_file <- file.path(
  output_folder,
  "arg_richness_predictor_adjusted_aicc_model_ranking.csv"
)

>>>>>>> WIP-predictor-ranking-analysis
# 3 Read input files
sample_richness_df <- read_csv(
  sample_richness_file_path,
  show_col_types = F
)
unadjusted_coefficient_table <- read_csv(
  unadjusted_coefficient_file_path,
  show_col_types = F
)

# 4 Get AMR gene panel size
<<<<<<< HEAD
n_genes_tested <- length(unique(amr_with_env_vars$amr_gene))
genes_tested <- unique(sample_richness_df$n_unique_genes_reported)
=======
n_genes_tested <- unique(sample_richness_df$n_unique_genes_reported)
n_genes_tested <- as.numeric(n_genes_tested)


# 5 Fit global adjusted null model with park and month only

# adjusted_null_model_data <- sample_richness_df %>%
#   filter(
#     !is.na(arg_richness),
#     !is.na(park_name),
#     !is.na(sample_month)
#   ) %>%
#   mutate(
#     arg_detection_proportion = arg_richness / n_genes_tested,
#     n_genes_tested_per_sample = n_genes_tested,
#     park_name = as.factor(park_name),
#     sample_month = as.factor(sample_month)
#   )
# 
# adjusted_null_model <- glm(
#   arg_detection_proportion ~ park_name + sample_month,
#   family = binomial,
#   weights = n_genes_tested_per_sample,
#   data = adjusted_null_model_data
# )
# 
# adjusted_null_aic <- AIC(adjusted_null_model)
# 
# adjusted_null_aicc <- calculate_aicc(adjusted_null_model)


# 5Unadjusted analysis predictors
predictor_list_df <- unadjusted_coefficient_table %>%
  dplyr::select(
    predictor,
    predictor_type) %>%
  distinct()

# 6 check sample structure by park and month
park_sample_counts <- sample_richness_df %>%
  count(park_name) %>%
  arrange(park_name)

print(park_sample_counts)

month_sample_counts <- sample_richness_df %>%
  count(sample_month) %>%
  arrange(sample_month)

print(month_sample_counts)

park_month_sample_counts <- sample_richness_df %>%
  count(park_name, sample_month) %>%
  arrange(park_name, sample_month)

print(park_month_sample_counts)

write_csv(
  park_sample_counts,
  file.path(
    output_folder,
    "arg_richness_adjusted_model_samples_by_park.csv"
  )
)

write_csv(
  month_sample_counts,
  file.path(
    output_folder,
    "arg_richness_adjusted_model_samples_by_month.csv"
  )
)

write_csv(
  park_month_sample_counts,
  file.path(
    output_folder,
    "arg_richness_adjusted_model_samples_by_park_month.csv"
  )
)

# 7 Define function for fitting one adjusted model

fit_one_adjusted_arg_richness_model <- function(
    predictor_name,
    predictor_type,
    data,
    n_genes_tested) {
  
  model_data <- data %>%
    dplyr::select(
      arg_richness,
      park_name,
      sample_month,
      all_of(predictor_name)
    ) %>%
    filter(
      !is.na(arg_richness),
      !is.na(park_name),
      !is.na(sample_month),
      !is.na(.data[[predictor_name]])
    ) %>%
    mutate(
      arg_detection_proportion = arg_richness / n_genes_tested,
      n_genes_tested_per_sample = n_genes_tested,
      park_name = as.factor(park_name),
      sample_month = as.factor(sample_month)
    )
  
  if (predictor_type == "categorical") {
    model_data <- model_data %>%
      mutate(
        across(
          all_of(predictor_name),
          as.factor
        )
      )
  }
  
  model_formula <- as.formula(
    paste(
      "arg_detection_proportion ~",
      predictor_name,
      "+ park_name + sample_month"
    )
  )
  
  model <- glm(
    formula = model_formula,
    family = binomial,
    weights = n_genes_tested_per_sample,
    data = model_data
  )
  
  adjusted_null_model <- glm(
    arg_detection_proportion ~ park_name + sample_month,
    family = binomial,
    weights = n_genes_tested_per_sample,
    data = model_data
  )
  
  adjusted_null_aic <- AIC(adjusted_null_model)
  
  adjusted_null_aicc <- calculate_aicc(adjusted_null_model)
  
  adjusted_model_summary <- tibble(
    predictor = predictor_name,
    predictor_type = predictor_type,
    n_samples_adjusted = nrow(model_data),
    n_genes_tested = n_genes_tested,
    model_family = "binomial",
    adjusted_for_park = TRUE,
    adjusted_for_month = TRUE,
    aic = AIC(model),
    aicc = calculate_aicc(model),
    adjusted_null_aic = adjusted_null_aic,
    adjusted_null_aicc = adjusted_null_aicc
  )
  
  adjusted_coefficients <- broom::tidy(
    model,
    conf.int = TRUE,
    exponentiate = TRUE
  ) %>%
    filter(term != "(Intercept)") %>%
    filter(!str_detect(term, "^park_name")) %>%
    filter(!str_detect(term, "^sample_month")) %>%
    mutate(
      predictor = predictor_name,
      predictor_type = predictor_type,
      n_samples_adjusted = nrow(model_data),
      n_genes_tested = n_genes_tested,
      model_family = "binomial",
      adjusted_for_park = TRUE,
      adjusted_for_month = TRUE
    ) %>%
    rename(
      adjusted_odds_ratio = estimate,
      adjusted_conf_low = conf.low,
      adjusted_conf_high = conf.high
    ) %>%
    dplyr::select(
      predictor,
      predictor_type,
      term,
      n_samples_adjusted,
      n_genes_tested,
      model_family,
      adjusted_for_park,
      adjusted_for_month,
      adjusted_odds_ratio,
      adjusted_conf_low,
      adjusted_conf_high
    )
  
  list(
    adjusted_model_summary = adjusted_model_summary,
    adjusted_coefficients = adjusted_coefficients
  )
}
# 8 Run one adjusted model for every predictor

adjusted_model_results <- list()

for (i in seq_len(nrow(predictor_list_df))) {
  
  current_predictor <- predictor_list_df$predictor[i]
  current_predictor_type <- predictor_list_df$predictor_type[i]
  
  adjusted_model_results[[current_predictor]] <- fit_one_adjusted_arg_richness_model(
    predictor_name = current_predictor,
    predictor_type = current_predictor_type,
    data = sample_richness_df,
    n_genes_tested = n_genes_tested
  )
}


# 9 Rank adjusted models with AICc

adjusted_model_ranking <- map_dfr(
  adjusted_model_results,
  "adjusted_model_summary"
) %>%
  mutate(
    adjusted_delta_aicc = aicc - min(aicc, na.rm = TRUE),
    delta_aicc_from_adjusted_null = aicc - adjusted_null_aicc,
    improves_over_adjusted_null = delta_aicc_from_adjusted_null < -2,
    adjusted_relative_likelihood = exp(-0.5 * adjusted_delta_aicc),
    adjusted_akaike_weight = adjusted_relative_likelihood / sum(adjusted_relative_likelihood, na.rm = TRUE),
    adjusted_aicc_support_category = case_when(
      adjusted_delta_aicc >= 0 & adjusted_delta_aicc <= 2 ~ "strong relative aicc support",
      adjusted_delta_aicc > 2 & adjusted_delta_aicc <= 7 ~ "moderate relative aicc support",
      adjusted_delta_aicc > 7 ~ "weak relative aicc support"
    )
  ) %>%
  arrange(adjusted_delta_aicc) %>%
  dplyr::select(
    predictor,
    predictor_type,
    adjusted_aicc_support_category,
    n_samples_adjusted,
    n_genes_tested,
    model_family,
    adjusted_for_park,
    adjusted_for_month,
    adjusted_aic = aic,
    adjusted_aicc = aicc,
    adjusted_delta_aicc,
    adjusted_null_aic,
    adjusted_null_aicc,
    delta_aicc_from_adjusted_null,
    improves_over_adjusted_null,
    adjusted_akaike_weight
  )

print(adjusted_model_ranking)

write_csv(
  adjusted_model_ranking,
  adjusted_model_ranking_output_file
)


# 10 Save adjusted coefficient table

adjusted_coefficient_table <- map_dfr(
  adjusted_model_results,
  "adjusted_coefficients"
) %>%
  left_join(
    adjusted_model_ranking %>%
      dplyr::select(
        predictor,
        predictor_type,
        n_genes_tested,
        model_family,
        adjusted_aicc_support_category,
        adjusted_aic,
        adjusted_aicc,
        adjusted_delta_aicc,
        adjusted_null_aic,
        adjusted_null_aicc,
        delta_aicc_from_adjusted_null,
        improves_over_adjusted_null,
        adjusted_akaike_weight
      ),
    by = c(
      "predictor",
      "predictor_type",
      "n_genes_tested",
      "model_family"
    )
  )

print(adjusted_coefficient_table)

write_csv(
  adjusted_coefficient_table,
  adjusted_coefficient_output_file
)


# 11 Compare unadjusted and adjusted estimates

comparison_table <- unadjusted_coefficient_table %>%
  rename(
    unadjusted_odds_ratio = odds_ratio,
    unadjusted_conf_low = conf_low,
    unadjusted_conf_high = conf_high,
    unadjusted_delta_aicc = delta_aicc,
    unadjusted_aicc_support_category = aicc_support_category,
    unadjusted_akaike_weight = akaike_weight
  ) %>%
  left_join(
    adjusted_coefficient_table,
    by = c(
      "predictor",
      "predictor_type",
      "term",
      "n_genes_tested",
      "model_family"
    )
  ) %>%
  mutate(
    unadjusted_direction = case_when(
      unadjusted_odds_ratio > 1 ~ "positive",
      unadjusted_odds_ratio < 1 ~ "negative",
      TRUE ~ "no clear direction"
    ),
    
    adjusted_direction = case_when(
      adjusted_odds_ratio > 1 ~ "positive",
      adjusted_odds_ratio < 1 ~ "negative",
      TRUE ~ "no clear direction"
    ),
    
    same_direction_after_adjustment = unadjusted_direction == adjusted_direction,
    
    adjusted_confidence_interval_includes_1 = adjusted_conf_low <= 1 &
      adjusted_conf_high >= 1,
    
    adjusted_association_summary = case_when(
      same_direction_after_adjustment & !adjusted_confidence_interval_includes_1 ~
        "same direction after adjustment and CI does not include 1",
      
      same_direction_after_adjustment & adjusted_confidence_interval_includes_1 ~
        "same direction after adjustment but CI includes 1",
      
      !same_direction_after_adjustment ~
        "direction changed after adjustment",
      
      TRUE ~
        "could not interpret adjusted association"
    ),
    
    aicc_support_changed_after_adjustment = unadjusted_aicc_support_category !=
      adjusted_aicc_support_category
  ) %>%
  arrange(adjusted_delta_aicc) %>%
  dplyr::select(
    predictor,
    predictor_type,
    term,
    n_samples,
    n_samples_adjusted,
    n_genes_tested,
    model_family,
    unadjusted_aicc_support_category,
    adjusted_aicc_support_category,
    aicc_support_changed_after_adjustment,
    unadjusted_delta_aicc,
    adjusted_delta_aicc,
    adjusted_null_aicc,
    delta_aicc_from_adjusted_null,
    improves_over_adjusted_null,
    unadjusted_akaike_weight,
    adjusted_akaike_weight,
    unadjusted_odds_ratio,
    unadjusted_conf_low,
    unadjusted_conf_high,
    adjusted_odds_ratio,
    adjusted_conf_low,
    adjusted_conf_high,
    unadjusted_direction,
    adjusted_direction,
    same_direction_after_adjustment,
    adjusted_confidence_interval_includes_1,
    adjusted_association_summary
  )

print(comparison_table)

write_csv(
  comparison_table,
  comparison_output_file
)

message(
  "Finished checking ARG richness predictors adjusted for park and month: ",
  comparison_output_file
)
>>>>>>> WIP-predictor-ranking-analysis
