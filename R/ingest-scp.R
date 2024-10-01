# Source in data ingestion helper functions
source("./R/helper_functions.R")
library(pak)
pak("RPostgres")

# Define the options for cancer types that can be viewed in the app
cancer_types = c(
  "all cancer sites",
  "bladder",
  "brain & ons",
  "breast (female)",
  "breast (female in situ)",
  "cervix",
  "childhood (ages <15, all sites)",
  "childhood (ages <20, all sites)",
  "colon & rectum",
  "esophagus",
  "kidney & renal pelvis",
  "leukemia",
  "liver & bile duct",
  "lung & bronchus",
  "melanoma of the skin",
  "non-hodgkin lymphoma",
  "oral cavity & pharynx",
  "ovary",
  "pancreas",
  "prostate",
  "stomach",
  "thyroid",
  "uterus (corpus & uterus, nos)"
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
  "ages <15",
  "ages <20",
  "ages <50",
  "ages 50+",
  "ages <65",
  "ages 65+"
)

year_options = c(
  "latest 5 year average"
)

input_combinations <- expand.grid(
  list(
    cancer_type = cancer_types,
    race = race_options,
    sex = sex_options,
    age = age_options,
    timespan = year_options
  ),
  stringsAsFactors = FALSE
)


# Define a function to process each row
process_row <- function(current_row) {
  current_cancer <- as.character(current_row["cancer_type"])
  current_race <- as.character(current_row["race"])
  current_sex <- as.character(current_row["sex"])
  current_age <- as.character(current_row["age"])
  current_timespan <- as.character(current_row["timespan"])

  print(paste("Processing:", current_cancer, current_race, current_sex, current_age, current_timespan))

  current_df <- tryCatch({
    get_incidence_df(
      current_cancer,
      current_race,
      current_sex,
      current_age,
      current_timespan
    )
  }, error = function(e) {
    # More detailed error message
    message(paste("Error in row with cancer type:", current_cancer,
                  "race:", current_race, "sex:", current_sex,
                  "age:", current_age, "timespan:", current_timespan,
                  " - Error message:", e$message))
    return(NULL)  # Return NULL in case of error
  })

  return(current_df)
}

# Apply the function to each row of the input_combinations df
result_list <- lapply(
  1:nrow(input_combinations),
  function(i) process_row(input_combinations[i, ])
)

# Combine the resulting data frames into a single output
out <- do.call(
  rbind,
  result_list[!sapply(result_list, is.null)] # Exclude NULL results
)


##########################################

db_host <- Sys.getenv("DB_HOST")
db_name <- Sys.getenv("DB_NAME")
db_user <- Sys.getenv("DB_USER")
db_password <- Sys.getenv("DB_PASSWORD")
db_port <- Sys.getenv("DB_PORT")

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = db_name,
  user = db_user,
  password = db_password,
  host = db_host
)

