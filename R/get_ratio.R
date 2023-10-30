#' get_ratio
#'
#' Get percentages for factor columns
#' @param data_vector column to check
#' @family assertions
#' @family tests
get_ratio <- function(data_vector) {

  # Initialize percent variable
  percent <- NULL

  # Check if the data_vector meets certain conditions
  if (!lubridate::is.POSIXct(data_vector)) {

    # Create frequency table of the data_vector and sort it by descending order
    frequency_table <- janitor::tabyl(data_vector) %>%
      dplyr::arrange(dplyr::desc(percent)) %>%
      utils::head(10) %>%
      dplyr::filter(!is.na(data_vector))

    # Assign the categories to a variable
    categories <- frequency_table$data_vector

    # Assign the percentages to a variable
    percentages <- format(frequency_table$percent * 100, digits = 2)

    if (sum(as.numeric(percentages)) < 100) {
      percentages <- append(percentages, 100 - sum(as.numeric(percentages)))
      categories <- append(categories, "Overig")
    } else {
      percentages[length(percentages)] <- 100 - sum(as.numeric(percentages[-length(percentages)]))
    }


    # Combines the categories and percentages into a single string
    result <- paste0(categories, ": (", percentages, "%)", collapse = " | ")
    return(result)
  }
  else if (lubridate::is.POSIXct(data_vector)) {

    # Create frequency table of the data_vector and sort it by descending order and keep top 10 values
    frequency_table <- janitor::tabyl(data_vector) %>%
      dplyr::arrange(dplyr::desc(percent)) %>%
      dplyr::top_n(10, "valid_percent") %>%
      utils::head(10) %>%
      dplyr::filter(!is.na(data_vector))

    # Assign the categories to a variable
    categories <- as.character(frequency_table$data_vector)

    # Assign the percentages to a variable
    percentages <- format(frequency_table$percent * 100, digits = 2)

    # Append the remaining percentage to the percentages
    # Append the remaining percentage to the percentages
    if (sum(as.numeric(percentages)) < 100) {
      percentages <- append(percentages, 100 - sum(as.numeric(percentages)))
      categories <- append(categories, "Overig")
    } else {
      percentages[length(percentages)] <- 100 - sum(as.numeric(percentages[-length(percentages)]))
    }

    # Check if first percentage is less than 1
    if (as.numeric(percentages[1]) < 0.01) {
      return("Not enough occurences")
    } else {
      # Combines the categories and percentages into a single string
      result <- paste0(categories, ": (", percentages, "%)", collapse = " | ")
      return(result)
    }
  }
  else{
    return("Too many categories")
  }
}
