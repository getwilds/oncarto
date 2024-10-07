#' Run the Shiny app
#'
#' This function runs the Shiny app packaged within this R package.
#' @export
#'

runApp <- function() {
  appDir <- system.file("shinyApp", package = "oncarto")
  if (appDir == "") {
    stop("Could not find Shiny app directory. Try re-installing `oncarto`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
