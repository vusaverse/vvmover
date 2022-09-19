#' Print date the file was last modified
#'
#' This function prints the date a file was last modified
#' @param path The file path
#' @export
print_last_modified <- function(path) {
  ## Extract the last modified date from the file and format it to yyyy-mm-dd
  Date <- format(file.info(path)$mtime, "%Y-%m-%d")
  ## print this
  print(paste(basename(path), "last modified", Date))
}