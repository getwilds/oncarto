#' This function specifies the UI for the county incidence tab of the app.
#' @importFrom shinydashboard tabItem box
#' @importFrom shiny fluidRow column selectInput NS uiOutput
#' @importFrom shinycssloaders withSpinner
#' @importFrom leaflet leafletOutput
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
          ),

          selectInput(
            NS(id, "race"),
            "Select population race/ethnicity:",
            choices = races
          ),

          selectInput(
            NS(id, "sex"),
            "Select sex:",
            choices = sexes
          ),

          selectInput(
            NS(id, "age"),
            "Select age range:",
            choices = ages
          ),

          selectInput(
            NS(id, "stage"),
            "Select cancer stage:",
            choices = stages
          ),

          selectInput(
            NS(id, "year"),
            "Select time span:",
            choices = years
          )
        )
      ),

      column(
        8,
        box(
          width = 12,
          uiOutput(""),
          #uiOutput("map_message"),
          shinycssloaders::withSpinner(leaflet::leafletOutput(""))
          #withSpinner(leafletOutput("choropleth"))
        )
      )
    ),

    fluidRow(
      column(
        12,
        box(
          width = 12,
          #uiOutput(outputId = "contactInfo")
          uiOutput("")
        )
      )
    )
  )
}
