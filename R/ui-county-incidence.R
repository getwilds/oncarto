# This function specifies the UI for the county incidence tab of the app.
#' @importFrom shinydashboard tabItem box
#' @importFrom shiny fluidRow column selectInput NS uiOutput
#' @importFrom shinycssloaders withSpinner
#' @importFrom leaflet leafletOutput
#' @importFrom htmltools a
#'
ui_county_incidence <- function(id) {
  # Contact info should be saved in a markdown file, which is passed as an
  # argument to runApp(). Keep as is for now - in the future can be a part of
  # callback
  organization <- htmltools::a("Fred Hutch Data Science Lab (DaSL)", href = "https://hutchdatascience.org")
  team <- htmltools::a("DaSL Translational Analytics", href = "https://hutchdatascience.org/tr-analytics/")
  team_email <- htmltools::a("analytics@fredhutch.org", href = "mailto:analytics@fredhutch.org")

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
          shiny::uiOutput(
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
