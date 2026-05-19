# 1 Load libraries and helper functions
source("./required_packages/setup_packages_and_dependencies_R.R")
source("./scripts/helper_functions.R")

# 2 Set input and output file paths

sample_richness_file_path <- "./data/arg_richness/sample_level_arg_richness.csv"
unadjusted_coefficient_file_path <- "./data/predictor_rankings/arg_richness_predictor_coefficients.csv"
output_folder <- "./data/predictor_rankings"

adjusted_coefficient_output_file <- file.path(
  output_folder,
  "arg_richness_predictors_adjusted_for_park_month.csv"
)
comparison_output_file <- file.path(
  output_folder,
  "arg_richness_predictors_unadjusted_vs_adjusted.csv"
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
n_genes_tested <- length(unique(amr_with_env_vars$amr_gene))
genes_tested <- unique(sample_richness_df$n_unique_genes_reported)