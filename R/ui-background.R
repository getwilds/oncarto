# This function specifies the UI for the background tab of the app.
#' @importFrom shinydashboard tabItem box
#' @importFrom shiny fluidRow column
#'
ui_background <- function(id) {
  background = "Oncarto (Oncology Cartographer) is an R package / Shiny dashboard developed by the Fred Hutch Data Science Lab for the automated integration and visualization of publicly available cancer incidence data for the Fred Hutch Cancer Center catchment area. We aim to develop a robust, open-source cartographic data visualization package from the ground up that can take the data made available by State Cancer Profiles and make it easily accessible by the public. TO BE EDITED FURTHER."

  shinydashboard::tabItem(
    tabName = "background",
    shiny::fluidRow(
      shiny::column(
        12,
        shinydashboard::box(
          width = 12,
          HTML(paste(background))
        )
      )
    )
  )
}
