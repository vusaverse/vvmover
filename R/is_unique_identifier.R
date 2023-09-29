#' Is Unique Identifier
#'
#' This function checks if a field is a unique identifier.
#'
#' @param field_name The name that is being tested for uniqueness.
#' @param df Dataframe where the unique identifier is being searched for.
#' @export
is_unique_identifier <- function(field_name, df) {
  column <- df %>% dplyr::select(field_name)
  ## Search for unique identifier
  ## length(df$field_name) == length(unique(df$field_name))
  if (nrow(df) == dplyr::n_distinct(column)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
