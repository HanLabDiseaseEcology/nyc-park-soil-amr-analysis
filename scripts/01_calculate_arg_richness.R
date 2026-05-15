# 1. Load libraries
library(readr)
library(dplyr)
library(stringr)

# 2. Set input and output file paths

input_file <- "./data/amr_with_environmental_data_cleaned.csv"
output_folder <- "./data/arg_richness"

richness_output_file <- file.path(output_folder, "sample_level_arg_richness.csv")

# 3. Import main df

main_df <- read_csv( input_file, show_col_types = FALSE)

# 4. Check required columns
required_columns <- c(
  "sample_date",
  "park_code",
  "tube_number",
  "amr_gene",
  "copies_per_microliter")

# 5. Clean key columns

main_df <- main_df %>%
  mutate(
    sample_date = as.Date(sample_date),
    park_code = str_to_upper(str_trim(as.character(park_code))),
    tube_number = str_to_upper(str_trim(as.character(tube_number))),
    amr_gene = str_to_upper(str_trim(as.character(amr_gene))),
    copies_per_microliter = as.numeric(copies_per_microliter),
    text_on_bag = str_to_upper(str_trim(as.character(text_on_bag)))
  )
# 6. Identify controls and non regular soil samples
# Control names used in the data:
#   PC1000
#   PC100
#   NC

# RAT, TRASH and LANDFILL are removed because they are not regular soil
# sampling locations for this analysis.

main_df <- main_df %>%
  mutate(
    is_control = if_any(
      c(tube_number),
      ~ .x %in% c("PC1000", "PC100", "NC")
    ),
    
    is_rat_or_trash = if_any(
      c(park_code, text_on_bag),
      ~ str_detect(.x, "RAT|TRASH|LANDFILL")
    )
  )

# 7. Keep only soil_chem samples that have all required data

soil_sample_df <- main_df %>%
  filter(!is_control) %>%
  filter(!is_rat_or_trash) %>%
  filter(
    !is.na(park_code),
    park_code != "",
    !is.na(sample_date)
  )


# 8. Candidate environmental predictor columns

candidate_predictors <- c(
  "prism_precipitation_mm",
  "prism_tmin_c",
  "prism_tmax_c",
  
  "sand_pct",
  "silt_pct",
  "clay_pct",
  
  "boron_ppm",
  "buffer_ph",
  "calcium_ppm",
  "cation_exchange_capacity_meq_100g",
  "copper_ppm",
  "estimated_nitrogen_release_lb_acre",
  "iron_ppm",
  "magnesium_ppm",
  "manganese_ppm",
  "organic_matter_pct",
  "ph",
  "phosphorus_ppm",
  "potassium_ppm",
  "sodium_ppm",
  "sulfur_ppm",
  "zinc_ppm",
  
  "calcium_saturation_pct",
  "hydrogen_saturation_pct",
  "magnesium_saturation_pct",
  "potassium_saturation_pct",
  "sodium_saturation_pct"
)

candidate_predictors <- candidate_predictors[ 
  candidate_predictors %in% names(soil_sample_df)]

message(
  "Candidate predictors found: ",
  paste(candidate_predictors, collapse = ", ")
)

# 9. Categorical predictors
categorical_predictors <- c("soil_classification")

categorical_predictors <- categorical_predictors[
  categorical_predictors %in% names(soil_sample_df)]
