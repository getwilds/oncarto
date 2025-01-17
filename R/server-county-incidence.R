# This function specifies the server logic for the county incidence tab of the
# app.
#
#' @importFrom shiny renderUI moduleServer
#' @importFrom dplyr filter left_join
#'
server_county_incidence <- function(id, callback, state_abbr,
                                    incidence_col_name, county_col_name) {

  shiny::moduleServer(id, function(input, output, session) {
    # Warning message if map can't be shown
    warning_message <- htmltools::tags$h4("No cancer incidence data are available for this specific combination of inputs. Please try a different combination of inputs.")

    # Get the shape file that includes county boundaries for the given US state
    county_boundaries <- get_county_boundaries(state_abbr, county_col_name)

    county_level_incidence <- shiny::reactive({
      callback("county-incidence") |>
        dplyr::filter(cancer_type == input$cancer_type, race == input$race,
                      sex == input$sex, age == input$age, stage == input$stage,
                      year == input$year)
    })

    output$choropleth <- leaflet::renderLeaflet({
      if(nrow(county_level_incidence()) == 0) {
        NULL
      } else {
          # Join the cancer data with counties boundaries based on county name
          county_level_incidence_with_shape <- county_boundaries |>
            dplyr::left_join(county_level_incidence(), by = county_col_name)

          # Generate color palette based on the selected cancer data
          pal <- make_palette(county_level_incidence_with_shape,
                              incidence_col_name, "#F4F4F4", "#FFB500",
                              "transparent")

          # Make the leaflet map
          make_leaflet(county_level_incidence_with_shape, pal, incidence_col_name,
                       county_col_name, "Age-Adjusted Incidence Rate")
      }
    })

    output$map_message <- renderUI({
      if(nrow(county_level_incidence()) == 0) {
        warning_message
      } else {
        NULL
      }
    })
  })
}
