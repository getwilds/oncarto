#' This function runs the Shiny app packaged within this R package.
#' @param title Title of the application
#' @param logo_src PNG or JPEG for the logo
#' @param logo_href Website link for the logo
#' @param logo_width Width of logo in pixels
#' @param logo_height Height of logo in pixels
#' @param css Filepath to the .css file that sets aesthetics for the whole app
#' @param get_data_fn A function that returns a dataframe according to the name
#' (a string) specified by the first argument
#' @param data_table_name Name of the table to be accessed by get_data_fn()
#' @param state_abbr State abbreviation for the U.S. state of interest for
#' for county-level information (e.g. 'WA')
#' @importFrom shiny shinyApp moduleServer
#' @importFrom shinydashboard dashboardPage dashboardBody tabItems
#' @export
#'

run_app <- function(title, logo_src, logo_href, logo_width, logo_height, css,
                    get_data_fn, data_table_name, state_abbr) {

  shiny::shinyApp(

    ui = dashboardPage(
      oncarto_header(title, logo_src, logo_href, logo_width, logo_height),
      oncarto_sidebar(),
      dashboardBody( # maybe replace with a new oncarto_body()?
        shiny::includeCSS(css),
        set_title(title),
        shinydashboard::tabItems(
          ui_county_incidence("incidence"),
          ui_background("background")
        )
      )
    ),

    server = function(input, output, session) {
      server_county_incidence("incidence", get_data_fn, data_table_name, state_abbr)
      server_background("background")
    }

  )
}




