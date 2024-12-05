# A function that provides the sample data for testing.
# The sample data must be a saved as a .tsv

test_callback <- function(file_name) {
  function() {
    read.table(
      header = TRUE,
      system.file(file.path("test", file_name), package = "oncarto"),
      sep = "\t"
    )
  }
}
