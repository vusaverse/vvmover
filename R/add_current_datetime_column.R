#' Add current date and time to data frame.
#'
#' This function adds a new column to a data frame with the current date and time.
#' The name of the new column is a combination of the provided prefix, stage, and "Date_time".
#' If the new column already exists, it will be overwritten.
#'
#' @param data Data frame.
#' @return Data frame with an additional column containing the current date and time.
#'
#' @examples
#' \dontrun{
#' # Create a sample data frame
#' data <- data.frame(a = 1:5, b = letters[1:5])
#'
#' # Add date to file name
#' add_current_datetime_column(data)
#' }
#' @export
add_current_datetime_column <- function(data) {
  new_var <- "Date_time"

  # Check if new column already exists
  if (new_var %in% colnames(data)) {
    warning("Column ", new_var, " already exists. It will be overwritten.")
  }

  data[, new_var] <- Sys.time()

  return(data)
}
