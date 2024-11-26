#' This function runs the Shiny app packaged within this R package.
#' @param title Title of the application
#' @param logo_src PNG or JPEG for the logo
#' @param logo_href Website link for the logo
#' @param logo_width Width of logo in pixels
#' @param logo_height Height of logo in pixels
#' @param css Filepath to the .css file that sets aesthetics for the whole app
#' @param get_data_fn A function that returns a dataframe according to the name
#' (a string) specified by the first argument
#' @importFrom shiny shinyApp
#' @importFrom shinydashboard dashboardPage dashboardBody tabItems
#' @export
#'

run_app <- function(title, logo_src, logo_href, logo_width, logo_height, css,
                    get_data_fn) {

  shiny::shinyApp(

    ui = shinydashboard::dashboardPage(
      oncarto_header(title, logo_src, logo_href, logo_width, logo_height),
      oncarto_sidebar(),
      shinydashboard::dashboardBody( # maybe replace with a new oncarto_body()?
        shiny::includeCSS(css),
        set_title(title),
        shinydashboard::tabItems(
          ui_county_incidence("incidence"),
          ui_background("background")
        )
      )
    ),

    server = function(input, output, session) {
      # WA County Incidence
      server_county_incidence("incidence", get_data_fn, "wa_county_incidence",
                              "WA", "Age_Adjusted_Incidence_Rate", "County")

      # Background Information
      server_background("background")
    }

  )
}




