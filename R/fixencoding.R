#' Fix encoding
#'
#' a generic function that can read an R object into an RDS, fst and CSV.
#' First determine the extension and then apply the appropriate read function
#'
#' @param df Dataframe
#' @param originalEncoding The encoding to apply
#' @return Dataframe
fixencoding <- function(df, originalEncoding = "latin1") {
  numCols <- ncol(df)
  for (col in 1:numCols)
  {
    if (inherits(df[, col], "character")) {
      base::Encoding(df[, col]) <- originalEncoding
    }

    if (inherits(df[, col], "factor")) {
      base::Encoding(levels(df[, col])) <- originalEncoding
    }
  }
  return(dplyr::as_data_frame(df))
}
