#' Print date the file was last modified
#'
#' This function prints the date a file was last modified
#' @param path The file path
#' @examples
#' print_last_modified(readr::readr_example("mtcars.csv"))
#'
#' @return message
#' @export
print_last_modified <- function(path) {
  ## Extract the last modified date from the file and format it to yyyy-mm-dd
  Date <- format(file.info(path)$mtime, "%Y-%m-%d")
  ## print this
  message(paste(basename(path), "last modified", Date))
}

#' get_last_modified_date
#'
#' @param file_path Path to the file.
#'
#' @return Date that file was last modified
#' @export
get_last_modifed_date <- function(file_path) {
  Datetime <- format(file.info(file_path)$mtime, "%Y-%m-%d")
  return(Datetime)
}
