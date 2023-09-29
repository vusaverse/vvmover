#' Check if Field is Subset
#'
#' This function checks if a field is a subset.
#' We define a subset as a character field that contains fewer than 20 unique values.
#'
#' @param field_name Name of the field to check.
#' @param df Dataframe to check.
#' @param column_names Set that the field is part of.
#' @param column_types Types that belong to the column names.
#' @family tests
#' @family assertions
is_field_subset <- function(field_name, df, column_names, column_types) {
  ## Find the index position of the field name
  index <- which(column_names == field_name)
  ## Check if Column is character
  if (column_types[index] == "character") {
    ## Check if this contains fewer than 20 unique values
    if (length(unique(df[[field_name]])) < 20) {
      ## Return type is subset
      return("subset")
    } else {
      ## Otherwise, the type remains the same
      return(column_types[[index]])
    }
  } else {
    ## Otherwise, the type remains the same
    return(column_types[[index]])
  }
}
