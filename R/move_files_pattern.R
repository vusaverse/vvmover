#' move files pattern
#'
#' Move files in a directory based on regular expression
#' @param Folder_origin Source folder.
#' @param Folder_dest Destination folder.
#' @param pattern Pattern to match files in source folder on.
#' @param recursive Default: FALSE. Whether to use recursive search in directory.
#' @export
move_files_pattern <- function(Folder_origin, Folder_dest, pattern, recursive = F) {

    Filepaths_origin <- list.files(path       = Folder_origin,
                                   pattern    = pattern,
                                   full.names = T,
                                   recursive  = recursive)

    if (length(Filepaths_origin) == 0) {
        return(paste("There are no files matching the pattern::", pattern))
    }

    Filepaths_dest <- paste0(Folder_dest, basename(Filepaths_origin))

    Moved <- file.rename(Filepaths_origin,
                         Filepaths_dest)

    print(paste("Aantal bestanden met patroon:", pattern, "verplaatst:", length(Filepaths_dest[Moved])))

}
