############################################### #############################
## get_recent_file_date_modified()
############################################### #############################
## get_recent_file_date_modified() gets the most recent version of a
## file on

#'Get recent file date modified
#' This function determines the path of the most recent version of a file in
#' a folder. The sorting is determined based on the date of the last
#' change.
#' @param path The path to search for the file.
#' @param match The search term to match in the file name.
#' @param echo Print the date the file was last modified
#' in the console.
#' @family Get recent files
#' @return The most recent file.
#' @export
get_recent_file_date_modified <- function(path, match, echo = TRUE) {

  ## List the files and get the file information
  ## Sort descending: most recent files first
  ## Then find the first element that contains the match
  ## and return that filename
  files <- list.files(path, full.names = TRUE)
  file_details <- file.info(list.files(path, full.names = TRUE))
  file_details <- file_details[with(file_details, order(as.POSIXct(mtime),decreasing = TRUE)), ]
  files <- rownames(file_details)
  for (file in files) {
    if(
      grepl(match, file, fixed=TRUE)
    ) {
      this_file <- file
      break
    } else {
      this_file <- NA
    }
  }

  return_var <- this_file

  ## Correct for double slashes
  return_var <- gsub("//","/",return_var)

  ## Print when the file was last modified
  if (echo) {
    ## Get the last modified date from the file_details table
    ## and format this to yyyy-mm-dd
    Date <- format(file_details[this_file, "mtime"], "%Y-%m-%d")
    ## print this
    message(paste(match, "last modified", Date))
  }
  return(return_var)
}
