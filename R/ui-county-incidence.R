# This function specifies the UI for the county incidence tab of the app.
#' @importFrom shinydashboard tabItem box
#' @importFrom shiny fluidRow column selectInput NS uiOutput
#' @importFrom shinycssloaders withSpinner
#' @importFrom leaflet leafletOutput
#' @importFrom htmltools a
#'
ui_county_incidence <- function(id) {
  organization <- htmltools::a("Fred Hutch Data Science Lab (DaSL)", href = "https://hutchdatascience.org")
  team <- htmltools::a("DaSL Translational Analytics", href = "https://hutchdatascience.org/tr-analytics/")
  team_email <- htmltools::a("analytics@fredhutch.org", href = "mailto:analytics@fredhutch.org")

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
          uiOutput(
            NS(id, "map_message")
          ),
          shinycssloaders::withSpinner(
            leaflet::leafletOutput(
              NS(id, "choropleth")
            )
          )
        )
      )
    ),

    fluidRow(
      column(
        12,
        box(
          width = 12,
          HTML(
            paste(
              "This application was developed by the ",
              organization,
              ". For questions or feedback regarding this application, email ",
              team,
              " at ",
              team_email,
              "."
            )
          )
        )
      )
    )
  )
}
