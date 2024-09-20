#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

## TODO:
## make Shiny app run when you call "oncarto" from the R console
## fill out additional tabs / background
## for certain cancer types, stage must be all stages

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
      # FH logo that links to DaSL website
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
        # Multiple tabs, each correspond to a different type of map / info
        menuItem(
          "Cancer Incidence by State",
          tabName = "state-incidence"
        ),

        menuItem(
          "Cancer Incidence by County",
          tabName = "county-incidence"
        ),

        menuItem(
          "Cancer Incidence by HSA",
          tabName = "hsa-incidence"
        ),

        menuItem(
          "Background",
          tabName = "background"
        )
      )
    ),

    # Main body of the app
    dashboardBody(
      # Include proper styling
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

      # Specify app title: Oncology Cartographer
      tags$script(HTML(
        '$(document).ready(function() {
          $("header").find("nav").append(\'<span class="myClass"> Oncology Cartographer (Oncarto) </span>\');
      })')),

      tabItems(
        tabItem(
          # State incidence tab
          tabName = "state-incidence",
          fluidRow(
            box(
              # Include input options as well as map generation and reset buttons
              selectInput(
                "cancer_type",
                "Select cancer subtype of interest:",
                choices = c(
                  "All subtypes" = "all cancer sites",
                  "Bladder" = "bladder",
                  "Brain and other nervous system" = "brain & ons",
                  "Colon and rectum" = "colon & rectum",
                  "Esophagus" = "esophagus",
                  "Kidney and renal pelvis" = "kidney & renal pelvis",
                  "Leukemia" = "leukemia",
                  "Liver & bile duct" = "liver & bile duct",
                  "Lung and bronchus" = "lung & bronchus",
                  "Melanoma of the skin" = "melanoma of the skin",
                  "Non-hodgkin lymphoma" = "non-hodgkin lymphoma",
                  "Oral cavity and pharynx" = "oral cavity & pharynx",
                  "Pancreas" = "pancreas",
                  "Stomach" = "stomach",
                  "Thyroid" = "thyroid"
                ),
                selected = "all cancer sites"
              ),

              selectInput(
                "race",
                "Select population race/ethnicity:",
                choices = c(
                  "All races/ethnicities (including Hispanic)" = "All Races (includes Hispanic)",
                  "White (non-Hispanic)",
                  "Black (non-Hispanic)",
                  "American Indian / Alaska Native (non-Hispanic)",
                  "Asian / Pacific Islander (non-Hispanic)",
                  "Hispanic (Any Race)"
                ),
                selected = "All Races (includes Hispanic)"
              ),

              selectInput(
                "sex",
                "Select population sex:",
                choices = c(
                  "Any sex" = "both sexes",
                  "Male" = "males",
                  "Female" = "females"
                ),
                selected = "both sexes"
              )
            ),
            box(
              selectInput(
                "age",
                "Select population age range:",
                choices = c(
                  "All ages" = "all ages",
                  "Ages <50" = "ages <50",
                  "Ages 50+" = "ages 50+",
                  "Ages <65" = "ages <65",
                  "Ages 65+" = "ages 65+",
                  "Ages <15" = "ages <15",
                  "Ages <20" = "ages <20"
                ),
                selected = "all ages"
              ),

              selectInput(
                "stage",
                "Select cancer stage of interest:",
                choices = c(
                  "All stages" = "all stages",
                  "Late stage (regional & distant)" = "late stage (regional & distant)"
                ),
                selected = "all stages"
              ),

              selectInput(
                "year",
                "Select time span of interest:",
                choices = c(
                  "Latest 5 year average" = "latest 5 year average",
                  "Latest single year" = "latest single year (us by state)"
                ),
                selected = "latest 5 year average"
              ),

              actionButton(
                inputId = "generateMap",
                label = strong("Generate map")
              ),

              actionButton(
                inputId = "reset",
                label = strong("Clear map")
              )
            )
          ),

          # Show choropleth map
          fluidRow(
            column(
              12,
              box(
                width = 12,
                leafletOutput("choropleth"))
              )
            ),

          # Show contact info
          fluidRow(
            column(
              12,
              box(
                width = 12,
                uiOutput(outputId = "contactInfo")
              )
            )
          )
        ),

        # Tabs for other levels of maps - TO BE DONE
        tabItem(
          tabName = "county-incidence"
        ),

        tabItem(
          tabName = "hsa-incidence"
        ),

        # Tab for background info - TO BE DONE
        tabItem(
          tabName = "background"
        )
      )
    )
)

# Define server logic
server <- function(input, output, session) {
    # Output text that clarifies contact info for the app
    daslWebsite <- a("Data Science Lab (DaSL).", href="https://hutchdatascience.org")
    daslTA <- a("Translational Analytics", href="https://hutchdatascience.org/tr-analytics/")
    daslEmail <- a("analytics@fredhutch.org.", href="mailto:analytics@fredhutch.org")
    output$contactInfo <- renderUI({
      HTML(
        paste(
          "This application was developed by the Fred Hutch ",
          daslWebsite,
          "For questions or feedback regarding this application, email DaSL ",
          daslTA,
          " at ",
          daslEmail,
          ""
        )
      )
    })

    # If the user clicks the "generate map" button...
    observeEvent(input$generateMap, {
      # Then we collect the inputs
      chosen_cancer = input$cancer_type
      race = input$race
      sex = input$sex
      age = input$age
      stage = input$stage
      year = input$year

      # Create a new DuckDB corresponding to cancer incidence data for this tab
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
        inner_join(incidence_by_cancer_type, by = "NAME")

      # Generate a choropleth plot using {leaflet}
      output$choropleth <- renderLeaflet({
        # Generate color palette based on the selected cancer data (FH yellow)
        pal <- colorNumeric(
          c("#F4F4F4", "#FFB500"),
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
            title = "Age-Adjusted Cancer Incidence",
            position = "topright"
          )
      })
    })

    # Hitting the reset button will reset all values and clear the map
    observeEvent(input$reset, {
      updateSelectInput(session,"cancer_type", selected = "all cancer sites")
      updateSelectInput(session,"race", selected = "All Races (includes Hispanic)")
      updateSelectInput(session,"sex", selected = "both sexes")
      updateSelectInput(session,"age", selected = "all ages")
      updateSelectInput(session,"stage", selected = "all stages")
      updateSelectInput(session,"year", selected = "latest 5 year average")
      output$choropleth <- renderLeaflet({})
    })
}

# Run the application
shinyApp(
  ui = ui,
  server = server
)
