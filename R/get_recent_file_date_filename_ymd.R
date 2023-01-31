#'Get recent file date filename ymd
#'
#'This function determines the path of the most recent version of a file in
#' a folder. Sorting is determined by file name
#' where it is a condition that the filename starts with ymd encoding.
#' @param path The path to search for the file.
#' @param match The search term to match in the file name.
#' @return The most recent file
#' @family Get recent files
#' @export
get_recent_file_date_filename_ymd <- function(path, match){
  ## List the files based on match and
  ## Sort descending by name with date format (most recent files
  ## first. Condition is file names starting with ymd quote
  ## Then find the first element that contains the match
  ## and return that filename
  files <- list.files(path, match, full.names = TRUE)
  return_var <- basename(files)
  no <- order(dplyr::desc(return_var))[[1]]
  thisfile <- files[[no]]
  if (grepl("^20[0-9]{2}", basename(thisfile))) {
    ## Print found file name
    message(paste("Most recent file is", basename(thisfile)))
    return(thisfile)
  }
  else
  { warning("File names do not follow ymd encoding,
              function get_recent_file_date_modified has been used")
    return(get_recent_file_date_modified(path=path, match=match))
  }
}
