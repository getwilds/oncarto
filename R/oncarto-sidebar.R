# This function fills in the sidebar of the shiny dashboard
#' @importFrom shinydashboard dashboardSidebar sidebarMenu menuItem
#'
oncarto_sidebar <- function() {
  dashboardSidebar(
    sidebarMenu(
      # Multiple tabs, each correspond to a different type of map / info
      menuItem(
        "Cancer Incidence by County",
        tabName = "county-incidence"
      ),

      menuItem(
        "Background",
        tabName = "background"
      )
    )
  )
}
