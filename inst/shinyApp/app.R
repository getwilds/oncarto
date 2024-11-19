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

# Call required libraries / packages
library(cancerprof) # @dev
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
library(shinycssloaders)

# User-provided parameters for app aesthetics
# Set aesthetics of app (logo and title)
#source("R/set-aesthetics.R")
source("../../R/set-aesthetics.R")
set_aesthetics()
# Set CSS file path
#css_file_path = "R/www/hutch_theme.css"
css_file_path = "www/hutch_theme.css"

# Get data that have been previously ingested from SCP
get_incidence_data <- function() {
  db_connection <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = Sys.getenv("DB_PORT")
  )

  on.exit(DBI::dbDisconnect(db_connection))

  DBI::dbReadTable(
    db_connection,
    "wa_county_incidence"
  )
}

incidence_data <- get_incidence_data()

## TODO: come back to this
# Get county boundaries for the choropleth visualization using the tigris package
wa_counties_sf <- st_transform(
  tigris::counties(state = "WA", class = "sf"),
  crs = 4326
)

# Define UI
ui <- dashboardPage(
    dashboardHeader(
      # Get logo information from set-aesthetics.R
      title = tags$a(
        href = Sys.getenv("LOGO_URL"),
        tags$img(
          src = Sys.getenv("LOGO_SRC"),
          height = Sys.getenv("LOGO_HEIGHT"),
          width = Sys.getenv("LOGO_WIDTH")
        )
      )
    ),

    dashboardSidebar(
      sidebarMenu(
        id = "sidebarid",

        # Multiple tabs, each correspond to a different type of map / info
        menuItem(
          "Cancer Incidence by County",
          tabName = "county-incidence"
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
      includeCSS(css_file_path),

      # Specify app title from set-aesthetics.R
      tags$script(HTML(
        paste0(
          '$(document).ready(function() { $("header").find("nav").append(\'<span class="dashboard-title"> ',
          Sys.getenv("APP_TITLE"),
          ' </span>\');})'
        )
      )),

      tabItems(
        tabItem(
          # County incidence tab
          tabName = "county-incidence",
          # Show choropleth map
          fluidRow(
            column(
              4,
              box(
                width = 12,
                selectInput(
                  "cancer_type",
                  "Select cancer subtype of interest:",
                  choices = c(
                    "All subtypes" = "all cancer sites",
                    "Bladder" = "bladder",
                    "Brain and other nervous system" = "brain & ons",
                    "Breast (female)" = "breast (female)",
                    "Breast (female in situ)" = "breast (female in situ)",
                    "Cervix" = "cervix",
                    "Childhood (ages <15, all subtypes)" = "childhood (ages <15, all sites)",
                    "Childhood (ages <20, all subtypes)" = "childhood (ages <20, all sites)",
                    "Colon and rectum" = "colon & rectum",
                    "Esophagus" = "esophagus",
                    "Kidney and renal pelvis" = "kidney & renal pelvis",
                    "Leukemia" = "leukemia",
                    "Liver and bile duct" = "liver & bile duct",
                    "Lung and bronchus" = "lung & bronchus",
                    "Melanoma of the skin" = "melanoma of the skin",
                    "Non-hodgkin lymphoma" = "non-hodgkin lymphoma",
                    "Oral cavity and pharynx" = "oral cavity & pharynx",
                    "Ovary" = "ovary",
                    "Pancreas" = "pancreas",
                    "Prostate" = "prostate",
                    "Stomach" = "stomach",
                    "Thyroid" = "thyroid",
                    "Uterus (corpus and not otherwise specified)" = "uterus (corpus & uterus, nos)"
                  ),
                  selected = "all cancer sites"
                ),

                selectInput(
                  "race",
                  "Select population race/ethnicity:",
                  choices = c(
                    "All races/ethnicities (including Hispanic)" =  "All Races (includes Hispanic)",
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
                  "Select sex:",
                  choices = c(
                    "Any sex" = "both sexes",
                    "Male" = "males",
                    "Female" = "females"
                  ),
                  selected = "both sexes"
                ),

                selectInput(
                  "age",
                  "Select age range:",
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
                  "Select cancer stage:",
                  choices = c(
                    "All stages" = "all stages",
                    "Late stage (regional & distant)" = "late stage (regional & distant)"
                  ),
                  selected = "all stages"
                ),

                selectInput(
                  "year",
                  "Select time span:",
                  choices = c(
                    "Latest 5 year average" = "latest 5 year average"
                  ),
                  selected = "latest 5 year average"
                )
              )
            ),
            column(
              8,
              box(
                width = 12,
                uiOutput("map_message"),
                withSpinner(leafletOutput("choropleth"))
              )
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

        # Tab for background info - TO BE DONE
        tabItem(
          tabName = "background",
          # Show contact info
          fluidRow(
            column(
              12,
              box(
                width = 12,
                uiOutput(outputId = "backgroundInfo")
              )
            )
          )
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

    output$backgroundInfo <- renderUI({
      HTML(
        paste(
          "Oncarto (Oncology Cartographer) is an R package / Shiny dashboard developed by the Fred Hutch Data Science Lab for the automated integration and visualization of publicly available cancer incidence data for the Fred Hutch Cancer Center catchment area. We aim to develop a robust, open-source cartographic data visualization package from the ground up that can take the data made available by State Cancer Profiles and make it easily accessible by the public. TO BE EDITED FURTHER."
        )
      )
    })

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

    # Generate a choropleth plot using {leaflet}
    output$choropleth <- renderLeaflet({
      # Get the county-level incidence data related to our desired input
      county_level_incidence <- incidence_data %>%
        filter(cancer_type == input$cancer_type) %>%
        filter(race == input$race) %>%
        filter(sex == input$sex) %>%
        filter(age == input$age) %>%
        filter(stage == input$stage) %>%
        filter(year == input$year) %>%
        mutate(NAMELSAD = County)

      # If data do not exist for this combination of inputs, print a warning message
      if(all(is.na(county_level_incidence$Age_Adjusted_Incidence_Rate))){
        output$map_message <- renderUI({
          HTML("<h4>No cancer incidence data are available for this specific combination of inputs. Please try a different combination of inputs.</h4>")
        })

        return(NULL)
      }

      # Otherwise, we can print our map
      else {
        output$map_message <- renderUI({
          NULL
        })

        # Join the cancer data with counties boundaries based on county name
        county_level_incidence_with_shape <- wa_counties_sf %>%
          left_join(county_level_incidence, by = "NAMELSAD")

        # Generate color palette based on the selected cancer data (FH yellow)
        pal <- colorNumeric(
          c("#F4F4F4", "#FFB500"),
          domain = county_level_incidence_with_shape$Age_Adjusted_Incidence_Rate,
          na.color = "transparent"
        )

        # Create an interactive choropleth map using {leaflet}
        leaflet(data = county_level_incidence_with_shape) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~pal(Age_Adjusted_Incidence_Rate),
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
            label = ~paste(NAMELSAD, ": ", Age_Adjusted_Incidence_Rate),
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"
            )
          ) %>%
          addLegend(
            pal = pal,
            values = ~Age_Adjusted_Incidence_Rate,
            opacity = 0.7,
            title = "Age-Adjusted Cancer Incidence Rate",
            position = "topright"
          )
      }
    })
}

# Run the application
shinyApp(
  ui = ui,
  server = server
)
