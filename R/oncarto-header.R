#' This function fills in the header of the shiny dashboard
#' @importFrom shinydashboard dashboardHeader
#' @importFrom htmltools tags
#'
oncarto_header <- function(title, logo_src, logo_href, logo_width, logo_height) {
  dashboardHeader(
    title = tags$a(
      href = logo_href,
      tags$img(
        src = logo_src, height = logo_height, width = logo_width
      )
    )
  )
}
