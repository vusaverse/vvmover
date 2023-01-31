############################################### #############################
## get_recent_file()
############################################### #############################
## get_recent_file() is a wrapper around get_recent_file_date_filename_ymd and
## get_recent_file_date_modified and retrieves the most recent version of a
## file based on naming or date modified.

#'Get recent file
#'
#' Is a wrapper around get_recent_file_date_filename_ymd en
#' get_recent_file_date_modified and retrieves the most recent version of a
#' file based on naming or date modified.
#' @param path The path to search for the file
#' @param match The search term matched in the file name
#' @param date_type The way to find the recent file
#' date_type = "modified" is based on customization, date_type = "filename_ymd"
#' is based on file name.
#' @return The most recent file.
#' @family Get recent files
#' @export
get_recent_file <- function(path, match, date_type = "modified") {
  ## Wrapper around functions get recent file based on
  ## naming (filename_ymd) or date(date_modified)
  if (date_type == "filename_ymd") {
    return(get_recent_file_date_filename_ymd(path = path, match = match))
  }
  if (date_type == "modified") {
    return(get_recent_file_date_modified(path = path, match = match))
  }
}
