ui_customize_map <- function(id) {
  shinydashboard::tabItem(
    tabName = "customize-map",
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shinydashboard::box(
          width = 12,
          shiny::selectInput(
            shiny::NS(id, "select_pal"),
            "Color Palette",
            choices = c("viridis", "magma", "inferno", "plasma",
                        rownames(RColorBrewer::brewer.pal.info)),
            selected = "viridis"
          )
        )
      )
    )
  )
}
