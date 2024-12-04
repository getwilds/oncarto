# A function that provides the test data for testing. Data must be a .TSV
#

test_callback <- function(file_name) {
  function() {
    read.table(
      header = TRUE,
      system.file(file.path("test", file_name), package = "oncarto"),
      sep = "\t"
    )
  }
}


#callback <- test_callback("sample-data.tsv", get_data_from_DB())
#callback()
