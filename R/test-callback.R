# A function that provides sample data (a .tsv) for testing.
#' @importFrom utils read.table

test_callback <- function(file_name) {
  function() {
    utils::read.table(
      header = TRUE,
      system.file(file.path("test", file_name), package = "oncarto"),
      sep = "\t"
    )
  }
}
