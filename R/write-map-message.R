#' Filter input data by chosen values of selectInputs
#' @importFrom shiny renderUI
#'
write_map_message <- function(filtered_data) {
  print(head(filtered_data))
  out <- NULL
  if(all(is.na(filtered_data$Age_Adjusted_Incidence_Rate))) {
    out <- HTML("<h4>No cancer incidence data are available for this specific combination of inputs. Please try a different combination of inputs.</h4>")
  } else {
    print("Safe!")
  }

  return(shiny::renderUI({out}))
}
