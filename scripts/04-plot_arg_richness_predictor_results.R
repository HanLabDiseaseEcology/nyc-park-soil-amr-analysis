# 1 Load libraries and helper functions
source("./required_packages/setup_packages_and_dependencies_R.R")
source("./scripts/helper_functions.R")

# 2 Set input and output file paths

sample_richness_file_path <- "./data/arg_richness/sample_level_arg_richness.csv"
model_ranking_file_path <- "./data/predictor_rankings/arg_richness_predictor_aicc_model_ranking.csv"
output_folder <- "./outputs/figures"

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

# 3 Read input files
sample_richness_df <- read_csv(
  sample_richness_file_path,
  show_col_types = FALSE
)
model_ranking_df <- read_csv(
  model_ranking_file_path,
  show_col_types = FALSE
)

#4 Filter for predictors with strong aicc scores
strong_predictors_df <- model_ranking_df %>%
  filter(
    aicc_support_category == "strongly relative aicc support") %>%
  dplyr::select(
    predictor,
    predictor_type,
    aicc_support_category,
    delta_aicc,
    akaike_weight) %>%
  distinct()

if (nrow(strong_predictors_df) == 0) {
  stop("No predictors with strong relative aicc support were found.")
}


# 5 Make plot for each strong aicc predictor
for (i in 1:(nrow(strong_predictors_df ))) {
  current_predictor <- strong_predictors_df$predictor[i]
  current_predictor_type <- strong_predictors_df$predictor_type[i]
  plot_data <- sample_richness_df %>%
    dplyr::select(
      arg_richness,
      park_name,
      sample_month,
      all_of(current_predictor)) %>%
    filter(
      !is.na(arg_richness),
      !is.na(park_name),
      !is.na(sample_month),
      !is.na(.data[[current_predictor]])
    )
  
  if (current_predictor_type == "numeric") {
    current_plot <- ggplot(
      plot_data,
      aes(
        x = .data[[current_predictor]],
        y = arg_richness,
        color = park_name)) +
      geom_point(
        size = 2,
        alpha = 0.8) +
      facet_wrap(
        ~ sample_month) +
      labs(
        title = paste("AMR richness vs", current_predictor),
        x = current_predictor,
        y = "AMR richness",
        color = "Park") +
      theme_bw()
  }
  
  if (current_predictor_type == "categorical") {
    
    current_plot <- ggplot(
      plot_data,
      aes(
        x = .data[[current_predictor]],
        y = arg_richness,
        color = park_name)) +
      geom_jitter(
        width = 0.15,
        height = 0,
        size = 2,
        alpha = 0.8) +
      facet_wrap(
        ~ sample_month ) +
      labs(
        title = paste("AMR richness vs", current_predictor),
        x = current_predictor,
        y = "AMR richness",
        color = "Park") +
      theme_bw()
  }
  print(current_plot)
  
  ggsave(
    filename = file.path(
      output_folder,
      paste0("amr_richness_vs_", current_predictor, ".png")),
    plot = current_plot,
    width = 8,
    height = 6)
}


message(
  "Finished plotting strong aicc predictors against AMR richness: ",
  output_folder
)