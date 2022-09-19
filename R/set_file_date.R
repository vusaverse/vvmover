#' Set file data
#'
#' Add datet to file name.
#'
#' @param data Data frame.
#' @param prefix Textstring.
#' @param stage Textstring.
#' @return Data frame.
#'
#'@export
set_file_date <- function(data, prefix, stage){
    new_var        <- paste(prefix, stage, "Datum_tijd", sep='_')
    data[,new_var] <- Sys.time()

    return(data)
}
