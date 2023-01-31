#' move files pattern
#'
#' Move files in a directory based on regular expression
#' @param Folder_origin Source folder.
#' @param Folder_dest Destination folder.
#' @param pattern Pattern to match files in source folder on.
#' @param recursive Default: FALSE. Whether to use recursive search in directory.
#' @return message
#' @export
move_files_pattern <- function(Folder_origin, Folder_dest, pattern, recursive = FALSE) {

    Filepaths_origin <- list.files(path       = Folder_origin,
                                   pattern    = pattern,
                                   full.names = TRUE,
                                   recursive  = recursive)

    if (length(Filepaths_origin) == 0) {
        return(paste("There are no files matching the pattern::", pattern))
    }

    Filepaths_dest <- paste0(Folder_dest, basename(Filepaths_origin))

    Moved <- file.rename(Filepaths_origin,
                         Filepaths_dest)

    message(paste("Number of files matching the pattern:", pattern, "are moved:", length(Filepaths_dest[Moved])))

}
