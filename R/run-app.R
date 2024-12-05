#' This function runs the Shiny app packaged within this R package.
#' @param title Title of the application
#' @param logo_src PNG or JPEG for the logo
#' @param logo_href Website link for the logo
#' @param logo_width Width of logo in pixels
#' @param logo_height Height of logo in pixels
#' @param css Filepath to the .css file that sets aesthetics for the whole app
#' @param contact_info Filepath to a markdown file that includes contact
#'  information to be displayed at the bottom of the page
#' @param callback A callback that returns a function that returns the relevant
#'  data
#' @importFrom shiny shinyApp addResourcePath
#' @importFrom shinydashboard dashboardPage dashboardBody tabItems
#' @export
#'

run_app <- function(title, logo_src, logo_href, logo_width, logo_height, css,
                    contact_info, callback) {

  shiny::addResourcePath(prefix = "img", directoryPath = dirname(logo_src))
  logo_src <- file.path("img", basename(logo_src))

  shiny::shinyApp(

    ui = shinydashboard::dashboardPage(
      oncarto_header(title, logo_src, logo_href, logo_width, logo_height),
      oncarto_sidebar(),
      shinydashboard::dashboardBody( # maybe replace with a new oncarto_body()?
        shiny::includeCSS(css),
        set_title(title),
        shinydashboard::tabItems(
          ui_county_incidence("incidence", contact_info),
          ui_background("background")
        )
      )
    ),

    server = function(input, output, session) {
      # WA County Incidence
      server_county_incidence("incidence", callback, "WA",
                              "Age_Adjusted_Incidence_Rate", "County")

      # Background Information
      server_background("background")
    }

  )
}




