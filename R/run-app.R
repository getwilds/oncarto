#' This function runs the Shiny app packaged within this R package.
#' @param title Title of the application
#' @param logo_src PNG or JPEG for the logo
#' @param logo_href Website link for the logo
#' @param logo_width Width of logo in pixels
#' @param logo_height Height of logo in pixels
#' @param css Filepath to the .css file that sets aesthetics for the whole app
#' @param get_data A function that returns a dataframe according to the name
#' (a string) specified by the first argument
#' @importFrom shiny shinyApp
#' @importFrom shinydashboard dashboardPage dashboardBody tabItems
#' @export
#'
run_app <- function(title, logo_src, logo_href, logo_width, logo_height, css,
                    get_data) {
  shiny::shinyApp(
    ui = dashboardPage(
      oncarto_header(title, logo_src, logo_href, logo_width, logo_height),
      oncarto_sidebar(),
      dashboardBody(
        shiny::includeCSS(css),
        set_title(title),
        shinydashboard::tabItems(
          ui_county_incidence("incidence")
        )
      )
      #oncarto_body()
    ),

    server = function(input, output) {

    }
  )
}




