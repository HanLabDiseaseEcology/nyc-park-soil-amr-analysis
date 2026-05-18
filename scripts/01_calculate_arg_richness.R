# 1. Load libraries
library(readr)
library(dplyr)
library(stringr)
library(tibble)
source("./scripts/helper_functions.R")

# 2. Set input and output file paths

input_file <- "./data/amr_with_environmental_data_cleaned.csv"
output_folder <- "./data/arg_richness"

richness_output_file <- file.path(
  output_folder, 
  "sample_level_arg_richness.csv"
)

dir.create( 
  output_folder, 
  recursive = TRUE, 
  showWarnings = FALSE
)

# 3. Import main df

main_df <- read_csv( 
  input_file, 
  show_col_types = FALSE)

# 4. Check required columns
required_columns <- c(
  "sample_date",
  "park_code",
  "tube_number",
  "amr_gene",
  "copies_per_microliter",
  "park_name")

# 5. Clean key columns

main_df <- main_df %>%
  mutate(
    sample_date = as.Date(sample_date),
    park_code = str_to_upper(str_trim(as.character(park_code))),
    tube_number = str_to_upper(str_trim(as.character(tube_number))),
    amr_gene = str_to_upper(str_trim(as.character(amr_gene))),
    copies_per_microliter = as.numeric(copies_per_microliter),
    park_name = str_to_upper(str_trim(as.character(park_name))),
    sample_month = format(sample_date, "%Y-%m")
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
      ~ .x %in% c("PC1000", "PC100", "NC")),
    
    is_rat_or_trash = if_any(
      c(park_code),
      ~ str_detect(.x, "RAT|TRASH|LANDFILL"))
  )

# 7. Keep only regular soil_chem samples that have all required data

soil_sample_df <- main_df %>%
  filter(!is_control) %>%
  filter(!is_rat_or_trash) %>%
  filter(
    !is.na(park_code),
    park_code != "",
    !is.na(sample_date),
    !is.na(park_name)
  )

if (all (c("prism_tmin_c" , "prism_tmax_c") %in% names(soil_sample_df ))) {
  soil_sample_df <- soil_sample_df %>%
    mutate(prism_tmean_c = (prism_tmin_c + prism_tmax_c) / 2)
}

# 8. Candidate environmental predictor columns

candidate_predictors <- c(
  "prism_precipitation_mm",
  "prism_tmin_c",
  "prism_tmax_c",
  "prism_tmean_c",
  
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
  "sodium_saturation_pct")

candidate_predictors <- candidate_predictors[ 
  candidate_predictors %in% names(soil_sample_df)]

# 9. Categorical predictors
categorical_predictors <- c("soil_classification")

categorical_predictors <- categorical_predictors[
  categorical_predictors %in% names(soil_sample_df)]


# 10. Create sample level ARG richness data frame
sample_richness_df <-soil_sample_df %>%
  filter( !is.na(amr_gene),amr_gene != "")%>%
  mutate(gene_detected = !is.na(copies_per_microliter) & copies_per_microliter > 0) %>%
  group_by(sample_date, park_code, tube_number, park_name, sample_month)%>%
  summarize(
    arg_richness = n_distinct(amr_gene[ gene_detected ]) ,
    total_arg_copies_per_microliter = sum(copies_per_microliter , na.rm = TRUE),
    n_positive_gene_rows = sum(gene_detected),
    n_unique_genes_reported = n_distinct( amr_gene),
    
    across( all_of(candidate_predictors),first_non_missing),
    across(all_of(categorical_predictors),first_non_missing ),
  
    .groups = "drop"
  )
write_csv(
  sample_richness_df, 
  richness_output_file
  )
#11. Save sample level ARG richness and summary file


richness_summary <-sample_richness_df %>%
  summarize(
    n_samples = n(),
    min_arg_richness = min(arg_richness, na.rm = TRUE),
    median_arg_richness = median (arg_richness, na.rm = TRUE),
    mean_arg_richness = mean(arg_richness , na.rm = TRUE),
    max_arg_richness = max(arg_richness, na.rm = TRUE),
    n_parks = n_distinct(park_name ),
    n_sample_dates = n_distinct( sample_date)
    )
print(richness_summary)
write_csv(richness_summary,
  file.path(output_folder, 
  "sample_level_arg_richness_summary.csv")
  )


#13 Check richness by park and montth

richness_by_park <- sample_richness_df %>%
  group_by( park_name) %>% summarise( n_samples = n(),
    
    mean_arg_richness = mean(
      arg_richness,
      na.rm = TRUE),
    
    median_arg_richness = median(
      arg_richness,
      na.rm = TRUE),
    
    min_arg_richness = min(
      arg_richness,
      na.rm = TRUE),
    
    max_arg_richness = max(
      arg_richness, na.rm = TRUE),.groups = "drop") %>% arrange(desc(median_arg_richness))

print(richness_by_park)

richness_by_month <- sample_richness_df %>%
  group_by(sample_month) %>%
  
  summarise(n_samples = n(),
    mean_arg_richness = mean(arg_richness,na.rm = TRUE),median_arg_richness = median( arg_richness,na.rm = TRUE),
     min_arg_richness = min(arg_richness, na.rm =TRUE),max_arg_richness = max( arg_richness,na.rm = TRUE),
    .groups ="drop") %>% arrange(sample_month 
  )

print(richness_by_month)

write_csv(
  richness_by_month,
  file.path(
    output_folder,
    "sample_level_arg_richness_by_month.csv" )
  )

write_csv(
  richness_by_park,
  file.path(
    output_folder,
    "sample_level_arg_richness_by_park.csv"
  )
)

#12 

