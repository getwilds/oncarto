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
library(shinycssloaders)

# Get data that have been previously ingested from SCP
db_connection <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST"),
  dbname = Sys.getenv("DB_NAME"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  port = Sys.getenv("DB_PORT")
)

incidence_data <- DBI::dbReadTable(
  db_connection,
  "wa_county_incidence"
)

DBI::dbDisconnect(db_connection)


# Get county boundaries for the choropleth visualization using the tigris package
wa_counties_sf <- st_transform(
  tigris::counties(state = "WA", class = "sf"),
  crs = 4326
)

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Cancer Incidence Data"),

    # Sidebar with a dropdown inputs
    sidebarLayout(
        sidebarPanel(

          # Cancer Type
          selectInput(
            "cancer_type",
            "Select Cancer Type:",
            choices = c(
              "all cancer sites",
              "bladder",
              "brain & ons",
              "breast (female)",
              "breast (female in situ)",
              "cervix",
              "childhood (ages <15, all sites)",
              "childhood (ages <20, all sites)",
              "colon & rectum",
              "esophagus",
              "kidney & renal pelvis",
              "leukemia",
              "liver & bile duct",
              "lung & bronchus",
              "melanoma of the skin",
              "non-hodgkin lymphoma",
              "oral cavity & pharynx",
              "ovary",
              "pancreas",
              "prostate",
              "stomach",
              "thyroid",
              "uterus (corpus & uterus, nos)"
            ),
            selected = "all cancer sites"
          ),

          selectInput(
            "race",
            "Select Race:",
            choices = c(
              "All Races (includes Hispanic)",
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
              "both sexes",
              "males",
              "females"
            ),
            selected = "both sexes"
          ),

          selectInput(
            "age",
            "Select age range:",
            choices = c(
              "all ages",
              "ages <50",
              "ages 50+",
              "ages <65",
              "ages 65+",
              "ages <15",
              "ages <20"
            ),
            selected = "all ages"
          ),

          selectInput(
            "stage",
            "Select cancer stage:",
            choices = c(
              "all stages",
              "late stage (regional & distant)"
            ),
            selected = "all stages"
          ),

          selectInput(
            "year",
            "Select time span:",
            choices = c(
              "latest 5 year average"
            ),
            selected = "latest 5 year average"
          )
        ),

        # Show the generated choropleth plot
        mainPanel(
           uiOutput("map_message"),
           withSpinner(leafletOutput("choropleth"))
        )
    )
)

# Define server logic
server <- function(input, output) {

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

        # Generate color palette based on the selected cancer data
        pal <- colorNumeric(
          "Blues",
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
            title = "Cancer Incidence",
            position = "topright"
          )
      }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
