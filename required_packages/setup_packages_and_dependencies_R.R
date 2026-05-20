# required_packages/setup_packages_and_dependencies_R.R

required_packages <- c(
  "readr",
  "dplyr",
  "stringr",
  "tibble",
  "purrr",
  "broom",
  "ggplot2"
)

for (package_name in required_packages) {
  
  if (!requireNamespace(package_name, quietly = T)) {
    install.packages(package_name)
  }
  
  suppressPackageStartupMessages(
    library(package_name, character.only = T)
  )
}