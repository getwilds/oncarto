#'# Specify app title from set-aesthetics.R
#' @param title
#' @importFrom htmltools tags
#'
set_title <- function(title){
  tags$script(HTML(
    paste0(
      '$(document).ready(function() { $("header").find("nav").append(\'<span class="dashboard-title"> ',
      title,
      ' </span>\');})'
    )
  ))
}
