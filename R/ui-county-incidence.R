# This function specifies the UI for the county incidence tab of the app.
#' @importFrom shinydashboard tabItem box
#' @importFrom shiny fluidRow column selectInput NS uiOutput includeMarkdown
#' @importFrom shinycssloaders withSpinner
#' @importFrom leaflet leafletOutput
#' @importFrom htmltools a
#'
ui_county_incidence <- function(id, callback) {
  shinydashboard::tabItem(
    tabName = "county-incidence",
    shiny::fluidRow(
      shiny::column(
        4,
        shinydashboard::box(
          width = 12,

          shiny::selectInput(
            shiny::NS(id, "cancer_type"),
            "Select cancer subtype of interest:",
            choices = cancer_types
          ),

          shiny::selectInput(
            shiny::NS(id, "race"),
            "Select population race/ethnicity:",
            choices = races
          ),

          shiny::selectInput(
            shiny::NS(id, "sex"),
            "Select sex:",
            choices = sexes
          ),

          shiny::selectInput(
            shiny::NS(id, "age"),
            "Select age range:",
            choices = ages
          ),

          shiny::selectInput(
            shiny::NS(id, "stage"),
            "Select cancer stage:",
            choices = stages
          ),

          shiny::selectInput(
            shiny::NS(id, "year"),
            "Select time span:",
            choices = years
          )
        )
      ),

      shiny::column(
        8,
        shinydashboard::box(
          width = 12,
          shiny::htmlOutput(
            shiny::NS(id, "map_message")
          ),
          shinycssloaders::withSpinner(
            leaflet::leafletOutput(
              shiny::NS(id, "choropleth")
            )
          )
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        12,
        shinydashboard::box(
          width = 12,
          shiny::includeMarkdown(callback("contact-info"))
        )
      )
    )
  )
}
