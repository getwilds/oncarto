#' This function specifies the server logic for the background tab of the app.
#' @importFrom shiny renderUI moduleServer
#'
server_background <- function(id) {
  moduleServer(id, function(input, output, session){
    output$backgroundInfo <- renderUI({
      HTML(paste(background))
    })
  })
}
