#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(pak)
pak("getwilds/cancerprof@dev")
pak("shiny")
pak("duckdb")
pak("duckplyr")
pak("DBI")
pak("dbplyr")
pak("sf")
pak("ggplot2")
pak("dplyr")
pak("RColorBrewer")
pak("tmap")
pak("leaflet")
pak("tigris")

library(shiny)
library(duckdb)
library(duckplyr)
library(DBI)
library(dbplyr)
library(sf)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(tmap)
library(leaflet)
library(tigris) # state boundaries






# Define UI for application that draws a histogram
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
            "Select Race:",
            choices = c(
              "both sexes" = "both",
              "male",
              "female"
            ),
            selected = "both"
          ),

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
              "ages <20" = "20",
            ),
            selected = "all"
          ),

          selectInput(
            "stage",
            "Select cancer stage:",
            choices = c(
              "all stages" = "allstages",
              "late stage (regional & distant)" = "latestage",
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
        ),


        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("choropleth")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$choropleth <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application
shinyApp(ui = ui, server = server)
