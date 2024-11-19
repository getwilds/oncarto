#' This function specifies the UI for the county incidence tab of the app.
#' @importFrom shinydashboard tabItem box
#' @importFrom shiny fluidRow column selectInput NS
#'
ui_county_incidence <- function(id) {
  tabItem(
    tabName = "county-incidence",
    fluidRow(
      column(
        4,
        box(
          width = 12,
          selectInput(
            NS(id, "cancer_type"),
            "Select cancer subtype of interest:",
            choices = cancer_types
          )
        )
      )
    )
  )
}
