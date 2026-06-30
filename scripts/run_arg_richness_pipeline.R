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

# 2 Create a test-run folder with the current date and time

test_run_name <- paste0(
  "arg_richness_test_",
  format(
    Sys.time(),
    "%Y-%m-%d_%H-%M-%S"
  )
)

test_run_folder <- file.path(
  project_root,
  "test_runs",
  test_run_name
)
dir.create(
  test_run_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!dir.exists(test_run_folder)) {
  stop(
    paste0(
      "The test run folder could not be created:\n",
      test_run_folder
    )
  )
}

message(
  "Created test-run folder: ",
  test_run_folder
)

# 3 Copy the input data and code into the test-run folder

test_data_folder <- file.path(
  test_run_folder,
  "data"
)

dir.create(
  test_data_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

input_file_copied <- file.copy(
  from = required_input_file,
  to = file.path(
    test_data_folder,
    required_input_file_name
  )
)

if (!input_file_copied) {
  stop("The input CSV could not be copied into the test-run folder.")
}



scripts_folder_copied <- file.copy(
  from = file.path(
    project_root,
    "scripts"
  ),
  to = test_run_folder,
  recursive = TRUE
)

required_packages_folder_copied <- file.copy(
  from = file.path(
    project_root,
    "required_packages"
  ),
  to = test_run_folder,
  recursive = TRUE
)

ranking_script_copied <- file.copy(
  from = file.path(
    project_root,
    "02-rank_predictors_arg_richness.R"
  ),
  to = test_run_folder
)

if (
  !scripts_folder_copied ||
  !required_packages_folder_copied ||
  !ranking_script_copied
) {
  stop("One or more required code files could not be copied.")
}


# 4 List the scripts in the order they should run

analysis_scripts <- c(
  "./scripts/01_calculate_arg_richness.R",
  "./02-rank_predictors_arg_richness.R",
  "./scripts/03_arg_richness_predictors_adjusted_for_park_and_month.R",
  "./scripts/04-plot_arg_richness_predictor_results.R"
)


# 5 Check that all copied scripts exist

missing_scripts <- analysis_scripts[
  !file.exists(
    file.path(
      test_run_folder,
      analysis_scripts
    )
  )
]

if (length(missing_scripts) > 0) {
  stop(
    paste0(
      "The following scripts are missing from the test-run folder:\n",
      paste(
        missing_scripts,
        collapse = "\n"
      )
    )
  )
}


# 6 Find the Rscript program

rscript_executable <- file.path(
  R.home("bin"),
  if (.Platform$OS.type == "windows") {
    "Rscript.exe"
  } else {
    "Rscript"
  }
)


# 7 Run all scripts from test-run folder

original_working_directory <- getwd()

setwd(test_run_folder)

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
        script_path,
        "\n\n",
        "Test files created before the error remain in:\n",
        test_run_folder
      )
    )
  }
  
  message("Finished: ", script_path)
}

setwd(original_working_directory)


# 8 ompletion message

message("")
message("The ARG richness test pipeline completed successfully.")
message("Test outputs were saved in: ", test_run_folder)