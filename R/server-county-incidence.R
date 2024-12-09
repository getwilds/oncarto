server_county_incidence <- function(id, callback) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      callback("county-incidence") |>
        dplyr::filter(cancer_type == input$cancer_type,
                      race == input$race,
                      sex == input$sex,
                      age == input$age,
                      stage == input$stage,
                      year == input$year)
    })
  })
}

# This function specifies the server logic for the county incidence tab of the
# app.
#
#' @importFrom shiny renderUI moduleServer
#' @importFrom dplyr filter left_join
#'
server_county_map <- function(id, callback, state_abbr, value_col,
                              county_col, county_boundaries, county_incidence) {

  shiny::moduleServer(id, function(input, output, session) {
    # Warning message if map can't be shown
    warning_message <- htmltools::tags$h4("No cancer incidence data are available for this specific combination of inputs. Please try a different combination of inputs.")

    output$choropleth <- leaflet::renderLeaflet({
      if(nrow(county_incidence) == 0) {
        NULL
      } else {
          # Join the cancer data with counties boundaries based on county name
          county_incidence_with_shape <- county_boundaries |>
            dplyr::left_join(county_incidence, by = county_col)

          # Generate color palette
          pal <- colorNumeric("viridis", NULL)

          # Make the leaflet map
          make_leaflet(county_incidence_with_shape, pal, value_col,
                       county_col, value_col)
      }
    })

    output$map_message <- renderUI({
      if(nrow(county_incidence) == 0) {
        warning_message
      } else {
        NULL
      }
    })
  })
}
