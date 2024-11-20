#' This function specifies the UI for the county incidence tab of the app.
#' @importFrom shinydashboard tabItem box
#' @importFrom shiny fluidRow column selectInput NS uiOutput
#'
ui_background <- function(id) {
  tabItem(
    tabName = "background",
    fluidRow(
      column(
        12,
        box(
          width = 12,
          uiOutput("")
          #uiOutput(outputId = "backgroundInfo")
        )
      )
    )
  )
}
