#' Read Excel Allsheets
#'
#' Read in all sheets in an Excel file.
#'
#' @param filename Name of Excel file
#' @examples
#' read_excel_allsheets(readxl::readxl_example("clippy.xls"))
#'
#' @return Dataframe
#'
#' @export
read_excel_allsheets <- function(filename) {
    check_installed_package("readxl")

    sheets <- readxl::excel_sheets(filename)

    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))

    names(x) <- sheets

    return(x)
}
