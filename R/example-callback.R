#' A function that provides an example dataframe for testing.
#' @param key A string used internally (in a specific shiny server module
#' implementation) to indicate which data to return.
#' @export
#' @importFrom utils read.table
#' @examples
#' example_callback("county-incidence")
#'
example_callback <- function(key) {
  stopifnot(key %in% c("county-incidence"))

  if (key == "county-incidence") {
    utils::read.table(
      header = TRUE,
      system.file(file.path("test", "example-incidence.tsv"), package = "oncarto"),
      sep = "\t"
    )
  }
}
