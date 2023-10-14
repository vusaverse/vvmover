#' Save session info to a file
#'
#' Stores session info in a .txt file.
#'
#' @param path The directory path where the session info file will be saved.
#'
#' @return A .txt file containing session info, saved at the specified path.
#' @export
save_session_info_to_file <- function(path) {
  writeLines(utils::capture.output(utils::sessionInfo()), paste(path, "sessionInfo.txt", sep = ""))
}
