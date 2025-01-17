# This function fills in the sidebar of the shiny dashboard
#' @importFrom shinydashboard dashboardSidebar sidebarMenu menuItem
#'
oncarto_sidebar <- function() {
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      # Multiple tabs, each correspond to a different type of map / info
      shinydashboard::menuItem(
        "Cancer Incidence by County",
        tabName = "county-incidence"
      ),

      shinydashboard::menuItem(
        "Background",
        tabName = "background"
      )
    )
  )
}
