# This function specifies the server logic for the county incidence tab of the
# app.
#
#' @importFrom shiny renderUI moduleServer
#' @importFrom leaflet renderLeaflet colorNumeric leaflet addTiles addPolygons
#'  addLegend labelOptions highlightOptions
#' @importFrom dplyr filter left_join
#'
server_county_incidence <- function(id, func_to_apply, data_table_name,
                                    state_abbr) {
  shiny::moduleServer(id, function(input, output, session) {
    # Warning message if map can't be shown
    warning_message <- "<h4>No cancer incidence data are available for this specific combination of inputs. Please try a different combination of inputs.</h4>"

    # Get the shape file that includes county boundaries for the given US state
    county_boundaries <- get_county_boundaries(state_abbr)

    # Get input cancer incidence data based on the user-provided 'get_data' fn
    # and table name
    fn_to_get_data <- get(func_to_apply)
    input_data <- fn_to_get_data(data_table_name)

    output$choropleth <- leaflet::renderLeaflet({
      # Filter the incidence data by the input parameters
      county_level_incidence <- input_data |>
        dplyr::filter(cancer_type == input$cancer_type) |>
        dplyr::filter(race == input$race) |>
        dplyr::filter(sex == input$sex) |>
        dplyr::filter(age == input$age) |>
        dplyr::filter(stage == input$stage) |>
        dplyr::filter(year == input$year)

      # If data do not exist for this combination of inputs, print a warning and
      # return NULL instead of a map
      if(all(is.na(county_level_incidence$Age_Adjusted_Incidence_Rate))){
        output$map_message <- renderUI({HTML(warning_message)})

        NULL
      }

      # Otherwise, do not print a warning, and return the map
      else {
        # Set warning message to NULL
        output$map_message <- renderUI({NULL})

        # Join the cancer data with counties boundaries based on county name
        county_level_incidence_with_shape <- county_boundaries |>
          left_join(county_level_incidence, by = "County")

        # Generate color palette based on the selected cancer data
        pal <- make_palette(county_level_incidence_with_shape,
                            "Age_Adjusted_Incidence_Rate", "#F4F4F4", "#FFB500",
                            "transparent")

        # Make the leaflet map
        make_leaflet(county_level_incidence_with_shape, pal)
      }
    })
  })
}
