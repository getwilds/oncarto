server_customize_map <- function(id, map_output_id, callback) {
  shiny::moduleServer(id, function(input, output, session) {


    observe({
      leafletProxy(map_output_id, session = session) |>
        clearShapes()

    })

    list(
      pal = reactive({ input$select_pal })
    )
  })
}
