# 1 Load libraries and helper functions
source("./required_packages/setup_packages_and_dependencies_R.R")
source("./scripts/helper_functions.R")

# 2 Set input and output file paths
input_file_path <- "./data/arg_richness/sample_level_arg_richness.csv"

predictor_list_file_path <- "./data/arg_richness/arg_richness_predictor_list.csv"
predictor_missingness_file_path <- "./data/arg_richness/sample_level_arg_richness_predictor_missingness.csv"
amr_with_env_data_file_path <- "./data/amr_with_environmental_data_cleaned.csv"

output_folder <- "./data/predictor_rankings"

dir.create(
  output_folder,
  recursive = T
)

model_ranking_output_file <- file.path(
  output_folder,
  "arg_richness_predictor_aicc_model_ranking.csv"
)

coefficient_output_file <- file.path(
  output_folder,
  "arg_richness_predictor_coefficients.csv"
)

interpretation_output_file <- file.path(
  output_folder,
  "arg_richness_predictor_interpretation_table.csv"
)

# 3. Read input files
sample_richness_df <- read_csv(
  input_file_path,
  show_col_types = F
)

predictor_list_df <- read_csv(
  predictor_list_file_path,
  show_col_types = F
)

predictor_missingness <- read_csv(
  predictor_missingness_file_path,
  show_col_types = F
)

amr_with_env_vars <- read_csv(
  amr_with_env_data.file_path,
  show_col_types = F
)

# 4. Set amr gene testing panel size? and check that richness does not exceed the number of tested genes
n_genes_tested <- length(unique(amr_with_env_vars$amr_gene))

if (any(sample_richness_df$arg_richness > n_genes_tested, na.rm = T)) {
  stop("Some samples have a richness greater than n_genes_tested.")
}

# 5 Sumarize predictors missing and calculate cutoff for inclusion

predictors_missingn_summary <- predictor_missingness %>%
  summarize(
    n_predictors = n(),
    min_proportion_missing = min(proportion_missing, na.rm = T),
    median_proportion_missing = median(proportion_missing, na.rm = T),
    mean_proportion_missing = mean(proportion_missing, na.rm = T),
    max_proportion_missing = max(proportion_missing, na.rm = T),
    min_non_missing = min(n_non_missing, na.rm = T),
    median_non_missing = median(n_non_missing, na.rm = T),
    max_non_missing = max(n_non_missing, na.rm = T),
    min_distinct_non_missing = min(n_distinct_non_missing, na.rm = T),
    median_distinct_non_missing = median(n_distinct_non_missing, na.rm = T),
    max_distinct_non_missing = max(n_distinct_non_missing, na.rm = T)
  )

print(predictors_missingn_summary)

write_csv(
  predictors_missingn_summary,
  file.path(
    output_folder,
    "arg_richness_predictors_missing_summary.csv")
)

total_samples <- nrow(sample_richness_df)

maximum_allowed_proportion_missing <- 0.25

minimum_non_missing_samples <- ceiling(
  total_samples * (1 - maximum_allowed_proportion_missing)
)

minimum_distinct_values <- 2

predictor_screening_thresholds <- tibble(
  threshold = c(
    "minimum_non_missing_samples",
    "minimum_distinct_values",
    "maximum_allowed_proportion_missing"),
  value = c(
    minimum_non_missing_samples,
    minimum_distinct_values,
    maximum_allowed_proportion_missing)
)

print(predictor_screening_thresholds)

write_csv(
  predictor_screening_thresholds,
  file.path(
    output_folder,
    "arg_richness_predictor_thresholds.csv")
)


# 6 Choosing predictors to model
predictor_screening_table <- predictor_missingness %>%
  mutate(
    included_in_modeling = n_distinct_non_missing >= minimum_distinct_values &
      proportion_missing <= maximum_allowed_proportion_missing,
    
    exclusion_reason = case_when(
      n_distinct_non_missing < minimum_distinct_values ~ "insufficient predictor variation",
      proportion_missing > maximum_allowed_proportion_missing ~ "exceeds maximum allowed missing data threshold",
      TRUE ~ "included" )
  )
usable_predictors <- predictor_screening_table$predictor[
  predictor_screening_table$included_in_modeling]

predictor_list_df <- predictor_list_df %>%
  filter(predictor %in% usable_predictors)

write_csv(
  predictor_screening_table,
  file.path(
    output_folder,
    "arg_richness_predictor_screening_table.csv")
)

# 7Summarize ARG richness response variable
arg_richness_summary <- sample_richness_df %>%
  summarize(
    n_samples = n(),
    n_genes_tested = n_genes_tested,
    min_arg_richness = min(arg_richness, na.rm = T),
    mean_arg_richness = mean(arg_richness, na.rm = T),
    median_arg_richness = median(arg_richness, na.rm = T),
    max_arg_richness = max(arg_richness, na.rm = T)
  )

print(arg_richness_summary)

write_csv(
  arg_richness_summary,
  file.path(
    output_folder,
    "arg_richness_response_variable_summary.csv")
)

# 8 Check binomial overdispersion
overdispersion_df <- sample_richness_df %>%
  mutate(
    arg_detection_proportion = arg_richness / n_genes_tested,
    n_genes_tested_per_sample = n_genes_tested
  )

baseline_binomial_model <- glm(
  arg_detection_proportion ~ 1,
  family = binomial,
  weights = n_genes_tested_per_sample,
  data = overdispersion_df
)

pearson_residuals <- residuals(
  baseline_binomial_model,
  type = "pearson"
)

pearson_chi_square <- sum(
  pearson_residuals^2
)

residual_degrees_of_freedom <- df.residual(
  baseline_binomial_model
)

binomial_overdispersion_ratio <- pearson_chi_square / residual_degrees_of_freedom

binomial_overdispersion_check <- tibble(
  model = "baseline_binomial_model",
  model_family = "binomial",
  n_genes_tested = n_genes_tested,
  pearson_chi_square = pearson_chi_square,
  residual_degrees_of_freedom = residual_degrees_of_freedom,
  overdispersion_ratio = binomial_overdispersion_ratio
)

print(binomial_overdispersion_check)
#Current
# # A tibble: 1 × 4 # We keep the binomial as a model family
# model                   model_family n_genes_tested overdispersion_ratio
# <chr>                   <chr>                 <int>                <dbl>
# baseline_binomial_model binomial                  8                 1.41


write_csv(
  binomial_overdispersion_check,
  file.path(
    output_folder,
    "arg_richness_binomial_overdispersion_check.csv"
  )
)

# 9 Fit model function

fit_one_arg_richness_model <- function(
    predictor_name,
    predictor_type,
    data,
    n_genes_tested) {
  
  model_data <- data %>%
    dplyr::select(
      arg_richness,
      all_of(predictor_name)) %>%
    filter(
      !is.na(arg_richness),
      !is.na(.data[[predictor_name]])) %>%
    mutate(
      arg_detection_proportion = arg_richness / n_genes_tested,
      n_genes_tested_per_sample = n_genes_tested)

  
  if (predictor_type == "categorical") {
    model_data <- model_data %>%
      mutate(
        across(
          all_of(predictor_name),
          as.factor))
  }
  
model_formula <- as.formula( paste("arg_detection_proportion ~", predictor_name))
  
model <- glm(
    formula = model_formula,
    family = binomial,
    weights = n_genes_tested_per_sample,
    data = model_data
)

model_pearson_residuals <- residuals(model, type = "pearson")

model_pearson_chi_square <- sum(model_pearson_residuals^2 )

model_residual_degrees_of_freedom <- df.residual(model)
  
model_overdispersion_ratio <- model_pearson_chi_square /model_residual_degrees_of_freedom
  
  model_summary <- tibble(
    predictor = predictor_name,
    predictor_type = predictor_type,
    n_samples = nrow(model_data),
    n_genes_tested = n_genes_tested,
    model_family = "binomial",
    aic = AIC(model),
    aicc = calculate_aicc(model),
    overdispersion_ratio = model_overdispersion_ratio
  )
  
  model_coefficients <- broom::tidy(
    model,
    conf.int = T,
    exponentiate = T) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      predictor = predictor_name,
      predictor_type = predictor_type,
      n_samples = nrow(model_data),
      n_genes_tested = n_genes_tested,
      model_family = "binomial") %>%
    dplyr::select(
      predictor,
      predictor_type,
      term,
      n_samples,
      n_genes_tested,
      model_family,
      estimate,
      conf.low,
      conf.high,
      p.value
    )
  
  list(
    model_summary = model_summary,
    model_coefficients = model_coefficients
  )
}


# 10 Runnning one model for each predictor
model_results <- list()

for (i in seq_len(nrow(predictor_list_df))) {
  current_predictor <- predictor_list_df$predictor[i]
  current_predictor_type <-predictor_list_df$predictor_type[ i]
  
  model_results[[current_predictor]] <- fit_one_arg_richness_model(
    predictor_name =current_predictor,
    predictor_type =current_predictor_type,
    data = sample_richness_df ,
    n_genes_tested = n_genes_tested )
}
