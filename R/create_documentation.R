#' Create Documentation
#'
#' This function creates documentation for a given dataframe.
#'
#' @param df The dataframe for which documentation is to be created.
#' @param doc_file_name Name of the documentation file to create.
#'
#' @return A table containing the documentation.
#' @export
#'
create_documentation <- function(df, doc_file_name) {
  column_names <- column_types <- lower_new <- upper_new <- lower <- upper <- NULL

  ## Step 1: Create vectors for the columns of the test documentation

  ## Save the column names
  column_names <- names(df)

  ## Save the column types
  column_types <- purrr::map_chr(df, first_class)

  ## Check if the character types are subsets;
  ## The current rule is that the field should have less than 20 unique values
  column_types <- purrr::map_chr(column_names, is_field_subset, df = df, column_names = column_names, column_types = column_types)

  ## Check for the percentage of missing values in the columns
  p_NAs <- colMeans(is.na(df)) * 100

  ## Find the max and min values for numeric columns.
  lower <- purrr::map_dbl(df, ~ ifelse(all(is.na(.)), NA, min_numeric(.)))
  upper <- purrr::map_dbl(df, ~ ifelse(all(is.na(.)), NA, max_numeric(.)))

  unique_identifier <- purrr::map_lgl(column_names, is_unique_identifier, df = df)

  ## Set all columns to In_use as TRUE
  In_use <- rep(TRUE, length(column_names))

  ##
  percentages <- purrr::map_chr(df, get_ratio)

  ##
  distribution <- purrr::map_chr(df, get_distribution)

  ## Create an empty column for comments
  comments <- ""

  ## Step 2: Create a dataframe
  ## Combine vectors to form a table
  documentation_table <- tibble::tibble(
    column_name = column_names,
    column_type = column_types,
    p_na = p_NAs,
    lower = lower,
    upper = upper,
    unique_identifier = unique_identifier,
    percentages = percentages,
    distribution = distribution,
    comments = comments
  )
}
