#' This function specifies the UI for the background tab of the app.
#' @importFrom shinydashboard tabItem box
#' @importFrom shiny fluidRow column NS uiOutput
#'
ui_background <- function(id) {
  tabItem(
    tabName = "background",
    fluidRow(
      column(
        12,
        box(
          width = 12,
          uiOutput(NS(id, "backgroundInfo"))
          #uiOutput(outputId = "backgroundInfo")
        )
      )
    )
  )
}
