# Helper functions 


#Creating sample-level summary datasets

first_non_missing <- function(x) {
  x <- x[ !is.na(x) ]
  if (length(x) == 0) {
    return(NA) }
    x[1]
}