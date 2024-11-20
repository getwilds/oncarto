#' This function specifies the server logic for the county incidence tab of the
#' app.
#'
#' @importFrom shiny renderUI moduleServer
#' @importFrom leaflet renderLeaflet
#'
server_county_incidence <- function(id) {
  moduleServer(id, function(input, output, session){

    output$choropleth <- renderLeaflet({
      NULL
    })

    output$map_message <- renderUI({
      NULL
    })

    output$contact_information <- renderUI({
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

  })
}
