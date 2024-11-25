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
    func <- get(func_to_apply)
    input_data <- func(data_table_name)

    county_boundaries <- get_county_boundaries(state_abbr)

    output$choropleth <- leaflet::renderLeaflet({
      # county_level_incidence <- filter_input_data(input_data, input$cancer_type,
      #                                             input$race, input$sex,
      #                                             input$age, input$stage,
      #                                             input$year)

      county_level_incidence <- input_data |>
        dplyr::filter(cancer_type == input$cancer_type) |>
        dplyr::filter(race == input$race) |>
        dplyr::filter(sex == input$sex) |>
        dplyr::filter(age == input$age) |>
        dplyr::filter(stage == input$stage) |>
        dplyr::filter(year == input$year)

      write_map_message(county_level_incidence)

      NULL
    })

  })
}
