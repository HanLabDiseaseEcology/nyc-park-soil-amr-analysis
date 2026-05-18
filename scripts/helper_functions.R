# Helper functions 

first_non_missing <- function(x) {
  x <- x[ !is.na(x) ]
  if (length(x) == 0) {
    return(NA) }
    x[1]
}

n_missing_values <- function(x) {
  sum(is.na(x))
}

n_non_missing_values <- function(x) {
  sum(!is.na(x))
  
}
n_distinct_non_missing_values <- function(x) {
  n_distinct(x, na.rm = TRUE)
}

#Model functions
calculate_aicc <- function(model) {
  model_aic <- AIC(model)
  n <- stats::nobs(model)
  k <- length(stats::coef(model))
  
  model_aic + ((2 * k * (k + 1)) / (n - k - 1))
}