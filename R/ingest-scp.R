# Source in data ingestion helper functions
source("./helper_functions.R")

# Define the options for cancer types that can be viewed in the app
cancer_types = c(
  "all cancer sites",
  "bladder",
  "brain & ons",
  "colon & rectum",
  "esophagus",
  "kidney & renal pelvis",
  "leukemia",
  "liver & bile duct",
  "lung & bronchus",
  "melanoma of the skin",
  "non-hodgkin lymphoma",
  "oral cavity & pharynx",
  "pancreas",
  "stomach",
  "thyroid"
)

race_options = c(
  "All Races (includes Hispanic)",
  "White (non-Hispanic)",
  "Black (non-Hispanic)",
  "American Indian / Alaska Native (non-Hispanic)",
  "Asian / Pacific Islander (non-Hispanic)",
  "Hispanic (Any Race)"
)

sex_options = c(
  "both sexes",
  "males",
  "females"
)

age_options = c(
  "all ages",
  "ages <50",
  "ages 50+",
  "ages <65",
  "ages 65+",
  "ages <15",
  "ages <20"
)

year_options = c(
  "latest 5 year average",
  "latest single year (us by state)"
)

db_host <- Sys.getenv("DB_HOST")
db_name <- Sys.getenv("DB_NAME")
db_user <- Sys.getenv("DB_USER")
db_password <- Sys.getenv("DB_PASSWORD")
db_port <- Sys.getenv("DB_PORT")

# Example of connecting to a database using R's DBI package
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  user = db_user,
  password = db_password,
  host = db_host,
  dbname = db_name
)

