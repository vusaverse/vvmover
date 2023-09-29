#' Minimum Numeric
#'
#' This function finds the minimum of a numeric vector.
#' If the vector is not numeric, it returns NA.
#'
#' @param x Vector of which minimum is wanted.
#' @export
min_numeric <- function(x) {
  if (is.numeric(x)) {
    non_missing_values <- x[!is.na(x)]
    if (length(non_missing_values) > 0) {
      return(round(min(non_missing_values), digits = 0))
    } else {
      return(NA_real_)
    }
  } else {
    return(NA_real_)
  }
}

#' Maximum Numeric
#'
#' This function finds the maximum of a numeric vector.
#' If the vector is not numeric, it returns NA.
#'
#' @param x Vector of which maximum is wanted.
#' @export
max_numeric <- function(x) {
  if (is.numeric(x)) {
    non_missing_values <- x[!is.na(x)]
    if (length(non_missing_values) > 0) {
      return(round(max(non_missing_values), digits = 0))
    } else {
      return(NA_real_)
    }
  } else {
    return(NA_real_)
  }
}
