#' This function specifies the server logic for the county incidence tab of the
#' app.
#'
#' @importFrom shiny renderUI moduleServer
#' @importFrom leaflet renderLeaflet
#'
server_county_incidence <- function(id, func_to_apply, data_table_name, state_abbr) {

  #source("./inst/get-data.R")
  #input_data <- get_data(data_table_name)

  func <- get(func_to_apply)
  input_data <- func(data_table_name)

  county_boundaries <- get_county_boundaries(state_abbr)

  shiny::moduleServer(id, function(input, output, session){

    output$contact_information <- shiny::renderUI({
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
    })

    output$choropleth <- leaflet::renderLeaflet({
      NULL
    })

    output$map_message <- shiny::renderUI({
      NULL
    })

  })
}
