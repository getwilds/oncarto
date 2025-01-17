#' Get the paths to files installed with oncarto
#'
#' This function allows you to quickly access files that are installed with
#' oncarto.
#'
#' @param path The name of the file. If no argument is provided then
#' all of the example files will be listed.
#'
#' @return A vector of file paths
#' @export
#' @examples
#' oncarto_file()
#'
oncarto_file <- function(path = NULL){
  if(is.null(path)) {
    list.files(
      system.file(c("app"), package = "oncarto"),
      full.names = TRUE)
  } else {
    system.file(c("app"), path, package = "oncarto",
                mustWork = TRUE)
  }
}
