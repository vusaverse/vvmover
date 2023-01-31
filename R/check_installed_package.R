#' check_installed_package
#'
#' Check if a package is installed. If not,
#' throw an error message
#'
#' @param package_name the name of the package (quoted)
#' @param check the function should work as a boolean operator
#' @return Boolean value whether package is installed.
#' @examples
#'   check_installed_package("dplyr")
#'
#' @export
check_installed_package <- function(package_name, check = FALSE) {
  if (check) {
    if (!requireNamespace(package_name, quietly = TRUE)) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else {
    if (!requireNamespace(package_name, quietly = TRUE)) {
      stop(paste("Package", package_name, "required to use this function",
                 "install this:", paste0("\ninstall.packages(\"",package_name, "\")")),
           call. = FALSE)
    }
  }
}
