#' A function that returns a dataframe according to the name
#' (a string) specified by the first argument
#' @importFrom DBI dbConnect dbDisconnect dbReadTable
#' @importFrom RPostgres Postgres
#'
get_data <- function(name_of_table) {
  db_connection <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = Sys.getenv("DB_PORT")
  )

  on.exit(DBI::dbDisconnect(db_connection))

  DBI::dbReadTable(
    name = name_of_table,
    conn = db_connection
  )
}
