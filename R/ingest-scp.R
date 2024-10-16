library(RPostgres)
library(dplyr)
library(DBI)
library(purrr)

##############################################################################
#' get_input_combinations: Get all combinations of input parameters
#'
#' @description
#' Based on the individual options for all of the input variables, generate
#' all combinations of cancer type, race, sex, age, and year and save them into
#' a dataframe
#'
#' @param cancer The type of cancer (character)
#' @param race The race of the population (character)
#' @param sex The sex of the population (character)
#' @param age The age group of the population (character)
#' @param stage The stage of the cancer (character)
#' @param year The desired timespan for the data (character)
#'
#' @return
#' A dataframe of all possible combinations of values of the input variables
#'
#' @examples
#' get_input_combinations(
#'    cancer_types,
#'    race_options,
#'    sex_options,
#'    age_options,
#'    stage_options
#'    year_options
#' )
#'

get_input_combinations <- function(cancer_types, race_options, sex_options, age_options, stage_options, year_options){
  expand.grid(
    list(
      cancer_type = cancer_types,
      race = race_options,
      sex = sex_options,
      age = age_options,
      stage = stage_options,
      timespan = year_options
    ),
    stringsAsFactors = FALSE
  )
}

##############################################################################
#' get_incidence_data: Pull cancer incidence data by county for a given set of inputs
#'
#' @description
#' Based on user-specified values of cancer, race, sex, age, stage, and
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
#' A dataframe of age-adjusted cancer incidence rates by county for the input
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
#'

get_incidence_data <- function(state, cancer, race, sex, age, stage, year){
  cancerprof::incidence_cancer(
      state, "county",
      cancer,
      race,
      sex,
      age,
      stage,
      year
    ) %>%
  select(
    County,
    Age_Adjusted_Incidence_Rate
  ) %>%
  mutate(
    Cancer_Type = cancer,
    Race = race,
    Sex = sex,
    Age = age,
    Stage = stage,
    Year = year
  )
}

###################################################################
#' process_row: Get cancer incidence for a given row in the input_combinations df
#'
#' @description
#' Based on a row including a single combination of inputs in the input_combinations df,
#' get corresponding cancer incidence for the state by county. If the {cancerprof}
#' API returns a warning, return NULL and print out the warning.
#'
#' @param state The state of interest
#' @param row The current row of the input_combinations df
#'
#' @return
#' The output county-level cancer incidence dataframe for this combination of parameters
#'
#' @examples
#' process_row(
#'    "wa",
#'    current_row
#' )

process_row <- function(state, row) {
  row <- map_chr(row, as.character)

  tryCatch({
    get_incidence_data(
      state,
      row["cancer_type"],
      row["race"],
      row["sex"],
      row["age"],
      row["stage"],
      row["timespan"]
    )
  }, error = function(e) {
    paste0(c(row["cancer_type"],
           row["race"],
           row["sex"],
           row["age"],
           row["stage"],
           row["timespan"]), collapse = ",")
  })
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
  input_combinations %>%
    rowwise() %>%
    group_split() %>%
    map(process_row, state = state, .progress = TRUE) %>%
    keep(is.data.frame) %>%
    list_rbind()
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
  db_connection <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = Sys.getenv("DB_PORT")
  )

  on.exit(DBI::dbDisconnect(db_connection))

  DBI::dbWriteTable(
    db_connection,
    df_name,
    df,
    overwrite = TRUE,
    row.names = FALSE
  )
}


###################################################################
#' ingest_scp_incidence: Ingest SCP data by county for all possible inputs
#'
#' @description
#' Collect state cancer profiles (SCP) county-level incidence data for all
#' combinations of cancer types, race, sex, age, stage, and year. Save this data
#' into the appropriate database
#'
#' @param state The state of interest (character)
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
#'    stage_options
#'    year_options
#' )
#'

ingest_scp_incidence <- function(state, cancer_types, race_options, sex_options, age_options, stage_options, year_options) {
  all_inputs <- get_input_combinations(
    cancer_types, race_options, sex_options, age_options, stage_options, year_options
  )

  write_to_db(
    get_incidence_for_all_inputs(state, all_inputs),
    paste0(state, "_county_incidence")
  )
}

#################################################################
# Example of running the above functions

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

stage_options = c(
  "all stages",
  "late stage (regional & distant)"
)

year_options = c(
  "latest 5 year average"
)

ingest_scp_incidence("wa", cancer_types, race_options, sex_options,
 age_options, stage_options, year_options)
