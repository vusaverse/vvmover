#' Extract a .zip archive and read in with read_delim
#'
#' Extract a .zip archive and read it in with readr's read_delim function. The file is extracted to a temporary location, and then deleted after reading it.
#'
#' @param zip_path The file path of the .zip archive
#' @param filename OPTIONAL: The file name of the file in the .zip archive to be read. This parameter can be left empty if there is only 1 file in the archive.
#' @param ... arguments to the readr::read_delim function. see: \link[readr]{read_delim}
#' @examples
#' unzip_read_delim(readr::readr_example("mtcars.csv.zip"))
#'
#' @return Dataframe
#'
#' @export
unzip_read_delim <- function(zip_path, filename = NULL, ...) {

  ## Create a temporary directory, and extract the file into it
  temporary_dir <- tempfile()
  utils::unzip(zip_path, exdir = temporary_dir)

  ## Check how many files are available
  files <- list.files(temporary_dir, full.names = TRUE)

  ## Determine the file to be read
  ## If the filename is specified, it will be read
  if (!is.null(filename)){

    file <- paste(temporary_dir, filename, sep = "/")
    # If there is more than 1 file in the .zip, an error will be given
  } else if (length(files) > 1) {
    stop(paste0("There is more than 1 file in the .zip file. specify a filename. The files are:\n",
                paste("- ",basename(files), collapse = "\n")))
    ## If there is 1 file in the zip, it will be used
  } else {
    file <- files[1]
  }

  ## Finally, run read_delim, after running the file will be deleted.
  ## this is in a tryCatch, so that in case of an error while reading, the file
  ## will still be removed
  tryCatch(
    {
      ## Use read_delim
      df <- readr::read_delim(file = file, ...)
      return(df)
    },
    ## If there is an error, the file will be deleted, and only then will the error be given
    finally = {unlink(temporary_dir,
                      force = TRUE,
                      recursive = TRUE)})

}
