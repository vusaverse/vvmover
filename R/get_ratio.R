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
    
    # Assign the percentages to a variable without rounding
    percentages <- frequency_table$percent * 100
    
    # Only add "Overig" category if there are more than 10 unique elements
    if (sum(percentages) < 100 && length(unique(data_vector)) > 10) {
      percentages <- append(percentages, 100 - sum(percentages))
      categories <- append(categories, "Overig")
    } else {
      percentages[length(percentages)] <- 100 - sum(percentages[-length(percentages)])
    }
    
    # Round percentages to two decimal places for display
    percentages <- format(percentages, digits = 2)
    
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
    
    # Assign the percentages to a variable without rounding
    percentages <- frequency_table$percent * 100
    
    # Only add "Overig" category if there are more than 10 unique elements
    if (sum(percentages) < 100 && length(unique(data_vector)) > 10) {
      percentages <- append(percentages, 100 - sum(percentages))
      categories <- append(categories, "Overig")
    } else {
      percentages[length(percentages)] <- 100 - sum(percentages[-length(percentages)])
    }
    
    # Round percentages to two decimal places for display
    percentages <- format(percentages, digits = 2)
    
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
