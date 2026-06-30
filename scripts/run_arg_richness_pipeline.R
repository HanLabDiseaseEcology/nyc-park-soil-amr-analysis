# 1 Check input data and folder, create folder if it does not exists

project_root <- getwd()

data_folder <- file.path(
  project_root,
  "data"
)

if (!dir.exists(data_folder)) {
  
  data_folder_created <- dir.create(
    data_folder,
    recursive = T,
    showWarnings = F
  )
  
  if (!data_folder_created && !dir.exists(data_folder)) {
    stop(
      paste0(
        "The data folder could not be created:\n",
        data_folder))
  }
  
  message("Created the data folder: ", data_folder)
}

required_input_file_name <- "amr_with_environmental_data_cleaned.csv"

required_input_file <- file.path(
  data_folder,
  required_input_file_name
)

if (!file.exists(required_input_file)) {
  stop(
    paste0(
      "The required input file was not found.\n\n",
      "Place this file:\n",
      required_input_file_name,
      "\n\n",
      "inside the project's data folder:\n",
      data_folder,
      "\n\n",
      "Expected file location is:\n",
      required_input_file))
}

message(
  "Found required input file: ",
  required_input_file
)

# 2 List the scripts in the order they should run

analysis_scripts <- c(
  "./scripts/01_calculate_arg_richness.R",
  "./scripts/02-rank_predictors_arg_richness.R",
  "./scripts/03_arg_richness_predictors_adjusted_for_park_and_month.R",
  "./scripts/04-plot_arg_richness_predictor_results.R"
)


# 3 Check that all scripts exist

missing_scripts <- analysis_scripts[
  !file.exists(
    file.path(
      project_root,
      analysis_scripts
    )
  )
]

if (length(missing_scripts) > 0) {
  stop(
    paste0(
      "The following scripts are missing:\n",
      paste(
        missing_scripts,
        collapse = "\n"
      )
    )
  )
}


# 4 Find the Rscript program

rscript_executable <- file.path(
  R.home("bin"),
  if (.Platform$OS.type == "windows") {
    "Rscript.exe"
  } else {
    "Rscript"
  }
)


# 5 Run all scripts from project folder

original_working_directory <- getwd()

setwd(project_root)

for (script_path in analysis_scripts) {
  
  message("")
  message("Running: ", script_path)
  
  exit_status <- system2(
    command = rscript_executable,
    args = shQuote(script_path)
  )
  
  if (exit_status != 0) {
    setwd(original_working_directory)
    
    stop(
      paste0(
        "The pipeline stopped because this script failed:\n",
        script_path
      )
    )
  }
  
  message("Finished: ", script_path)
}


# 6 Render the Quarto document

quarto_document <- file.path(
  "outputs",
  "reports",
  "arg_richness_adjusted_aicc_results.qmd"
)

if (!file.exists(quarto_document)) {
  setwd(original_working_directory)
  
  stop(
    paste0(
      "The Quarto document was not found:\n",
      quarto_document
    )
  )
}

message("")
message("Rendering: ", quarto_document)

quarto_status <- system2(
  command = "quarto",
  args = c(
    "render",
    shQuote(quarto_document)
  )
)

if (quarto_status != 0) {
  setwd(original_working_directory)
  
  stop(
    paste0(
      "The Quarto document could not be rendered:\n",
      quarto_document
    )
  )
}

message("Finished rendering: ", quarto_document)

setwd(original_working_directory)


# 7 ompletion message

message("")
message("The ARG richness pipeline completed successfully.")
message("Outputs were saved in the project data and outputs folders.")