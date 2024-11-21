#' Print contact information for the developers of the app
#' @importFrom shiny renderUI
#'
print_contact_information <- function(organization, team, team_email) {
  shiny::renderUI({
    HTML(
      paste(
        "This application was developed by the ",
        organization,
        ". For questions or feedback regarding this application, email ",
        team,
        " at ",
        team_email,
        "."
      )
    )
  })
}
