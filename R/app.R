#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Call required libraries / packages
library(pak)
pak("getwilds/cancerprof@dev")
library(shiny)
library(duckdb)
library(duckplyr)
library(DBI)
library(dbplyr)
library(dplyr)
library(sf)
library(leaflet)
library(tigris)
library(shinythemes)
library(shinydashboard)

# Source in data ingestion helper functions
source("./helper_functions.R")

# Define the options for cancer types that can be viewed in the app
cancer_types = c(
  "all cancer sites",
  "bladder",
  "brain & ons",
  "colon & rectum",
  "esophagus",
  "kidney & renal pelvis",
  "leukemia",
  "liver & bile duct",
  "lung & bronchus",
  "melanoma of the skin",
  "non-hodgkin lymphoma",
  "oral cavity & pharynx",
  "pancreas",
  "stomach",
  "thyroid"
)

# Get state boundaries for the choropleth visualization using the tigris package
states_sf <- tigris::states(cb = TRUE) %>%
  st_as_sf()

# Define UI
ui <- dashboardPage(
    dashboardHeader(
      title = tags$a(
        href='https://hutchdatascience.org',
        tags$img(
          src='fhLogo.png',
          height='35px',
          width='155px'
        )
      )
    ),

    dashboardSidebar(
      sidebarMenu(
        menuItem(
          "Cancer Incidence by State",
          tabName = "state-incidence"
        )
      )
    ),

    # Show the generated choropleth plot
    dashboardBody(
      includeCSS("www/hutch_theme.css"),

      tags$head(tags$style(HTML(
          '.myClass {
          font-size: 20px;
          line-height: 50px;
          text-align: left;
          font-family: "Arial",Helvetica,Arial,sans-serif;
          padding: 0 15px;
          overflow: hidden;
          color: white;
      }'))),

      tags$script(HTML(
        '$(document).ready(function() {
          $("header").find("nav").append(\'<span class="myClass"> Oncology Cartographer (Oncarto) </span>\');
      })')),

      fluidRow(
        box(
          selectInput(
            "cancer_type",
            "Select Cancer Type:",
            choices = c(
              "all cancer sites" = "allsites",
              "bladder",
              "brain & ons" = "brain",
              "colon & rectum" = "colon",
              "esophagus",
              "kidney & renal pelvis" = "kidney",
              "leukemia",
              "liver & bile duct" = "liver",
              "lung & bronchus" = "lung",
              "melanoma of the skin" = "melanoma",
              "non-hodgkin lymphoma" = "lymphoma",
              "oral cavity & pharynx" = "oral",
              "pancreas",
              "stomach",
              "thyroid"
            ),
            selected = "allsites"
          ),

          selectInput(
            "race",
            "Select Race:",
            choices = c(
              "All Races (includes Hispanic)" = "allraces",
              "White (non-Hispanic)" = "white",
              "Black (non-Hispanic)" = "black",
              "American Indian / Alaska Native (non-Hispanic)" = "native",
              "Asian / Pacific Islander (non-Hispanic)" = "asian",
              "Hispanic (Any Race)" = "hisp"
            ),
            selected = "allraces"
          ),

          selectInput(
            "sex",
            "Select sex:",
            choices = c(
              "both sexes" = "both",
              "males",
              "females"
            ),
            selected = "both"
          )
        ),
        box(
          selectInput(
            "age",
            "Select age range:",
            choices = c(
              "all ages" = "all",
              "ages <50" = "<50",
              "ages 50+" = "50+",
              "ages <65" = "<65",
              "ages 65+" = "65+",
              "ages <15" = "15",
              "ages <20" = "20"
            ),
            selected = "all"
          ),

          selectInput(
            "stage",
            "Select cancer stage:",
            choices = c(
              "all stages" = "allstages",
              "late stage (regional & distant)" = "latestage"
            ),
            selected = "allstages"
          ),

          selectInput(
            "year",
            "Select time span:",
            choices = c(
              "latest 5 year average" = "5yr",
              "latest single year (us by state)" = "1yr"
            ),
            selected = "5yr"
          )
        )
      ),

      fluidRow(
        column(12, leafletOutput("choropleth"))
      )
    )
)

# Define server logic
server <- function(input, output) {

    # Generate a choropleth plot using {leaflet}
    output$choropleth <- renderLeaflet({
      chosen_cancer = input$cancer_type
      race = input$race
      sex = input$sex
      age = input$age
      stage = input$stage
      year = input$year

      # Create a new DuckDB corresponding to incidence data from US states
      con <- dbConnect(duckdb::duckdb(), "cancer-incidence-usa-state.duckdb")

      ## 1. Ingest data from State Cancer Profiles if needed
      # Iterate over each possible type of cancer
      for (cancer in cancer_types) {
        # For each cancer, get the name of the data file to be saved
        table_name = get_incidence_db_name(cancer, race, sex, age, stage, year)
        # If the data file is not already in the database...
        if (table_name %in% dbListTables(con) == FALSE) {
          # Then write the dataframe corresponding to the incidence data to the db
          dbWriteTable(
            con,
            table_name,
            get_incidence_df(cancer, race, sex, age, stage, year)
          )
        }
      }

      ## 2. Munge relevant data from State Cancer Prof to wide format for viz
      # Based on input parameters,
      incidence_by_cancer_type = merge_all_incidence(
        cancer_types,
        race, sex, age, stage, year,
        con
      )

      # Disconnect from the database
      dbDisconnect(con)

      ## 3. Visualize wide data using {leaflet}
      # Make a new column called "NAME" in the incidence output data which includes
      # the name of the state and excludes data contained in parentheses in the
      # original {cancerprof} state name
      incidence_by_cancer_type$NAME = gsub(
        "\\s*\\([^\\)]+\\)",
        "",
        incidence_by_cancer_type$State
      )

      # Join the cancer data with state boundaries based on state name
      incidence_by_type_with_shape <- states_sf %>%
        left_join(incidence_by_cancer_type, by = "NAME")

      # Generate color palette based on the selected cancer data
      pal <- colorNumeric(
        "Blues",
        domain = incidence_by_type_with_shape[[chosen_cancer]],
        na.color = "transparent"
      )

      # Create an interactive choropleth map using {leaflet}
      leaflet(data = incidence_by_type_with_shape) %>%
        addTiles() %>%
        # set initial zoom to focus on center of United States
        setView(lng = -98.583, lat = 39.828, zoom = 2) %>%
        addPolygons(
          fillColor = ~pal(incidence_by_type_with_shape[[chosen_cancer]]),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = ~paste(NAME, ": ", incidence_by_type_with_shape[[chosen_cancer]]),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          pal = pal,
          values = ~incidence_by_type_with_shape[[chosen_cancer]],
          opacity = 0.7,
          title = "Cancer Incidence",
          position = "topright"
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
