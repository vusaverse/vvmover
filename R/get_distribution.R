#' Get Distribution
#'
#' This function gets the distribution for numeric columns.
#'
#' @param data_vector Column to check.
#' @family assertions
#' @family tests
get_distribution <- function(data_vector) {
  if (is.numeric(data_vector)) {
    quantiles <- stats::quantile(data_vector, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
    summary_stats <- c(
      Mean = mean(data_vector, na.rm = TRUE),
      Std_Dev = stats::sd(data_vector, na.rm = TRUE)
    )
    quantiles <- paste(
      "Q1 =", quantiles[1], " | median =", quantiles[2], " | Q3 = ", quantiles[3],
      " | mean =", format(summary_stats[1], digits = 3), " | stdev =", format(summary_stats[2], digits = 3)
    )
    return(quantiles)
  } else if (is.character(data_vector)) {
    if (any(grepl("^\\d*\\.?\\d*$", sample(data_vector, 100)))) {
      data_vector <- data_vector[grepl("^\\d*\\.?\\d*$", data_vector)]
      data_vector <- data_vector[!is.na(data_vector)]
      data_vector <- as.double(data_vector)
      quantiles <- stats::quantile(data_vector, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
      summary_stats <- c(
        Mean = mean(data_vector, na.rm = TRUE),
        Std_Dev = stats::sd(data_vector, na.rm = TRUE)
      )
      quantiles <- paste(
        "Q1 =", quantiles[1], " | median =", quantiles[2], " | Q3 = ", quantiles[3],
        " | mean =", format(summary_stats[1], digits = 3), " | stdev =", format(summary_stats[2], digits = 3)
      )
      return(quantiles)
    } else {
      default_na <- as.double(NA_character_)
      return(default_na)
    }
  } else {
    default_na <- as.double(NA_character_)
    return(default_na)
  }
}
