# Specify app title from set-aesthetics.R
# @param title Title of app
#' @importFrom htmltools tags HTML
#'
set_title <- function(title){
  htmltools::tags$script(
    htmltools::HTML(
      paste0(
        '$(document).ready(function() { $("header").find("nav").append(\'<span class="dashboard-title"> ',
        title,
        ' </span>\');})'
      )
    )
  )
}
