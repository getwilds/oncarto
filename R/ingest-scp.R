# Source in data ingestion helper functions
library(pak)
pak("RPostgres")
pak("dplyr")

##############################################################################
#' get_input_combinations: Get all combinations of input parameters
#'
#' @description
#' Based on the individual options for all of the input variables, generate
#' all combinations of cancer type, race, sex, age, and year.
#'
#' @param cancer The type of cancer (character)
#' @param race The race of the population (character)
#' @param sex The sex of the population (character)
#' @param age The age group of the population (character)
#' @param year The desired timespan for the data (character)
#'
#' @return
#' A dataframe of all possible combinations of input variables
#'
#' @examples
#' get_input_combinations(
#'    cancer_types,
#'    race_options,
#'    sex_options,
#'    age_options,
#'    year_options
#' )
#'

get_input_combinations <- function(cancer_types, race_options, sex_options, age_options, year_options){
  out <- expand.grid(
    list(
      cancer_type = cancer_types,
      race = race_options,
      sex = sex_options,
      age = age_options,
      timespan = year_options
    ),
    stringsAsFactors = FALSE
  )

  return(out)
}

##############################################################################
#' get_incidence_data: Pull cancer incidence data by county for a given set of inputs
#'
#' @description
#' Based on the user-specified values of state, cancer, race, sex, age, stage, and
#' year, collect cancer incidence data by county for the state of interest
#'
#' @param state The US state of interest (character)
#' @param cancer The type of cancer (character)
#' @param race The race of the population (character)
#' @param sex The sex of the population (character)
#' @param age The age group of the population (character)
#' @param stage The stage of the cancer (character)
#' @param year The desired timespan for the data (character)
#'
#' @return
#' A dataframe of age-adjusted cancer incidence rates by county for this
#' combination of parameters
#'
#' @examples
#' get_incidence_data(
#'    "wa",
#'    "all cancer sites",
#'    "All Races (includes Hispanic)",
#'    "both sexes",
#'    "all ages",
#'    "all stages",
#'    "latest 5 year average"
#' )
#'
#'

get_incidence_data <- function(state, chosen_cancer, chosen_race, chosen_sex, chosen_age, chosen_year){
  out = as.data.frame(
    cancerprof::incidence_cancer(
      state, "county",
      chosen_cancer,
      chosen_race,
      chosen_sex,
      chosen_age,
      "all stages",
      chosen_year
    )
  ) %>%
    select(
      County,
      Age_Adjusted_Incidence_Rate
    )

  out <- out %>%
    mutate(
      cancer_type = replicate(nrow(out), chosen_cancer)
    ) %>%
    mutate(
      race = replicate(nrow(out), chosen_race)
    ) %>%
    mutate(
      sex = replicate(nrow(out), chosen_sex)
    ) %>%
    mutate(
      age = replicate(nrow(out), chosen_age)
    ) %>%
    mutate(
      year = replicate(nrow(out), chosen_year)
    )

  return(out)
}

###################################################################
#' process_row: Get cancer incidence for a given row in the input_combinations df
#'
#' @description
#' Based on a row including a single combination of inputs in the input_combinations df,
#' get corresponding cancer incidence for the state by county
#' the user-specified parameters of cancer, race, sex, age, stage, and
#' year, collect cancer incidence data by WA county
#'
#' @param state The state of interest
#' @param row The current row of the input_combinations df
#' @param sex The sex of the population (character)
#' @param age The age group of the population (character)
#' @param stage The stage of the cancer (character)
#' @param year The desired timespan for the data (character)
#'
#' @return
#' The output cancer incidence dataframe to be saved to duckdb for this
#' combination of parameters
#'
#' @examples
#' process_row(
#'    "wa",
#'    current_row
#' )
#'

process_row <- function(state, row) {
  current_cancer <- as.character(row["cancer_type"])
  current_race <- as.character(row["race"])
  current_sex <- as.character(row["sex"])
  current_age <- as.character(row["age"])
  current_timespan <- as.character(row["timespan"])

  print(
    paste(
      "Processing:",
      current_cancer, current_race, current_sex, current_age, current_timespan
    )
  )

  current_df <- tryCatch({
    get_incidence_data(
      state,
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


###################################################################
#' get_incidence_for_all_inputs: Get incidence for all rows in input_combinations
#'
#' @description
#' Iterate over all of the rows in input_combinations to get cancer incidence by
#' county for all possible combinations of input values
#'
#' @param state The state of interest (character)
#' @param input_combinations All possible combinations of input values (dataframe)
#'
#' @return
#' All cancer incidence rates for all counties and all input value combinations
#'
#' @examples
#' get_incidence_for_all_inputs(
#'   state,
#'   all_inputs
#' )
#'

get_incidence_for_all_inputs <- function(state, input_combinations) {
  # Apply `process_row` to each row of the input_combinations df
  result_list <- lapply(
    1:nrow(input_combinations),
    function(i) process_row(state, input_combinations[i, ])
  )

  # Combine the resulting dataframes into a single output
  out <- do.call(
    rbind,
    result_list[!sapply(result_list, is.null)] # Exclude NULL results
  )

  return(out)
}


###################################################################
#' write_to_db: Write the dataframe to a database
#'
#' @description
#' Save the input dataframe to a database
#'
#' @param df The type of cancer (character)
#' @param df_name The race of the population (character)
#'
#' @return
#' N/A
#'
#' @examples
#' write_to_db(
#'   wa_county_incidence,
#'   "wa_county_incidence"
#' )
#'

write_to_db <- function(df, df_name) {
  db_host <- Sys.getenv("DB_HOST")
  db_name <- Sys.getenv("DB_NAME")
  db_user <- Sys.getenv("DB_USER")
  db_password <- Sys.getenv("DB_PASSWORD")
  db_port <- Sys.getenv("DB_PORT")

  db_connection <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = db_host,
    dbname = db_name,
    user = db_user,
    password = db_password,
    port = db_port
  )

  DBI::dbWriteTable(
    db_connection,
    df_name,
    df,
    overwrite = TRUE,
    row.names = FALSE
  )

  DBI::dbDisconnect(db_connection)
}


###################################################################
#' ingest_scp_incidence: Ingest SCP data by county for all possible inputs
#'
#' @description
#' Collect state cancer incidence data for all combinations of cancer types,
#' race, sex, age, stage, and year
#'
#' @param cancer The type of cancer (character)
#' @param race The race of the population (character)
#' @param sex The sex of the population (character)
#' @param age The age group of the population (character)
#' @param stage The stage of the cancer (character)
#' @param year The desired timespan for the data (character)
#'
#' @return
#' The output cancer incidence dataframe to be saved to duckdb for this
#' combination of parameters
#'
#' @examples
#' ingest_scp_incidence(
#'    "wa",
#'    cancer_types,
#'    race_options,
#'    sex_options,
#'    age_options,
#'    year_options
#' )
#'

ingest_scp_incidence <- function(state, cancer_types, race_options, sex_options, age_options, year_options) {
  all_inputs = get_input_combinations(
    cancer_types, race_options, sex_options, age_options, year_options
  )

  out = get_incidence_for_all_inputs(
    state, all_inputs
  )

  return(out)
  #write_to_db(
  #  out,
  #  paste0(state, "_county_incidence")
  #)
}

#################################################################

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

out = ingest_scp_incidence(
  "wa",
  cancer_types,
  race_options,
  sex_options,
  age_options,
  year_options
)

head(out)
