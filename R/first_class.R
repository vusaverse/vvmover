#' Get the first class of an object
#'
#' This function returns the first class of an object or vector.
#'
#' @param obj The object or vector whose class is to be found.
#' @return The first class of the object or vector.
#' @export
#'
#' @examples
#' # Create a vector of different classes
#' my_vector <- c(1, "two", 3.0)
#' # Get the first class of the vector
#' first_class(my_vector)
first_class <- function(obj) {
  return(class(obj)[1])
}
