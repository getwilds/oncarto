#' get_incidence_db_name: Get the right name for the data file to be saved
#'
#' @description
#' Based on the user-specified parameters of cancer, race, sex, age, stage, and
#' year, return an appropriate name for the database file to be saved
#'
#' @param cancer The type of cancer (character)
#' @param race The race of the population (character)
#' @param sex The sex of the population (character)
#' @param age The age group of the population (character)
#' @param stage The stage of the cancer (character)
#' @param year The desired timespan for the data (character)
#'
#' @return
#' The name of the database file to be saved to duckdb for this combination of
#' parameters
#'
#' @examples
#' get_incidence_db_name(
#'    "all cancer sites",
#'    "All Races (includes Hispanic)",
#'    "both sexes",
#'    "all ages",
#'    "all stages",
#'    "latest 5 year average"
#' )
#'

get_incidence_db_name <- function(cancer, race, sex, age, stage, year){
  ## all cancer sites = all
  ## bladder = bladder
  ## brain & ons = brain
  ## colon & rectum = colon
  ## esophagus = esophagus
  ## kidney & renal pelvis = kidney
  ## leukemia = leukemia
  ## liver & bile duct = liver
  ## lung & bronchus = lung
  ## melanoma of the skin = melanoma
  ## non-hodgkin lymphoma = lymphoma
  ## oral cavity & pharynx = oral
  ## pancreas = pancreas
  ## stomach = stomach
  ## thyroid = thyroid
  c_name = "allsites" # all cancer sites
  if (cancer == "bladder" ||
      cancer == "esophagus" ||
      cancer == "pancreas" ||
      cancer == "stomach" ||
      cancer == "thyroid" ||
      cancer == "leukemia") {
    c_name = cancer
  } else if (cancer == "brain & ons") {
    c_name = "brain"
  } else if (cancer == "colon & rectum") {
    c_name = "colon"
  } else if (cancer == "kidney & renal pelvis") {
    c_name = "kidney"
  } else if (cancer == "liver & bile duct") {
    c_name = "liver"
  } else if (cancer == "lung & bronchus") {
    c_name = "lung"
  } else if (cancer == "melanoma of the skin") {
    c_name = "melanoma"
  } else if (cancer == "non-hodgkin lymphoma") {
    c_name = "lymphoma"
  } else if (cancer == "oral cavity & pharynx") {
    c_name = "oral"
  }

  ## All Races (includes Hispanic) = allraces
  ## White (non-Hispanic) = white
  ## Black (non-Hispanic) = black
  ## American Indian / Alaska Native (non-Hispanic) = native
  ## Asian / Pacific Islander (non-Hispanic) = asian
  ## Hispanic (Any Race) = hisp
  r_name = "allraces" # All Races (includes Hispanic)
  if (race == "White (non-Hispanic)") {
    r_name = "white"
  } else if (race == "Black (non-Hispanic)") {
    r_name = "black"
  } else if (race == "American Indian / Alaska Native (non-Hispanic)") {
    r_name = "native"
  } else if (race == "Asian / Pacific Islander (non-Hispanic)") {
    r_name = "asian"
  } else if (race == "Hispanic (Any Race)") {
    r_name = "hisp"
  }

  ## both sexes = both
  ## males = male
  ## females = female
  sx_name = "bothsexes" # both sexes
  if (sex == "males") {
    sx_name = "male"
  } else if (sex == "females") {
    sx_name = "female"
  }

  ## all ages = all
  ## ages <50 = <50
  ## ages 50+ = 50+
  ## ages <65 = <65
  ## ages 65+ = 65+
  ## ages <15 = 15
  ## ages <20 = 20
  a_name = "allages" # all ages
  if (age == "ages <50") {
    a_name = "<50"
  } else if (age == "ages 50+") {
    a_name = "50+"
  } else if (age == "ages <65") {
    a_name = "<65"
  } else if (age == "ages 65+") {
    a_name = "65+"
  } else if (age == "ages <15") {
    a_name = "15"
  } else if (age == "ages <20") {
    a_name = "20"
  }

  ## all stages = allstage
  ## late stage (regional & distant) = latestage
  st_name = "allstages" # all stages
  if (stage == "late stage (regional & distant)") {
    st_name = "latestage"
  }

  ## latest 5 year average = 5yr
  ## latest single year (us by state) = 1yr
  y_name = "5yr" # latest 5 year average
  if (stage == "latest single year (us by state)") {
    y_name = "1yr"
  }

  #out = paste0("usa_state_", c_name, "_", r_name, "_", sx_name, "_", a_name, "_", st_name, "_", y_name)
  out = paste0("usa_state_", cancer, "_", race, "_", sex, "_", age, "_", stage, "_", year)
  return(out)
}



#' get_incidence_df: Collect cancer incidence data by US state
#'
#' @description
#' Based on the user-specified parameters of cancer, race, sex, age, stage, and
#' year, collect cancer incidence data by US state
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
#' get_incidence_df(
#'    "all cancer sites",
#'    "All Races (includes Hispanic)",
#'    "both sexes",
#'    "all ages",
#'    "all stages",
#'    "latest 5 year average"
#' )
#'
#'

get_incidence_df <- function(cancer, race, sex, age, stage, year){
  out = as.data.frame(
    cancerprof::incidence_cancer(
      "USA", "state",
      cancer,
      race,
      sex,
      age,
      stage,
      year
    )
  )

  return(out)
}



#' merge_all_incidence: Join cancer incidence data across cancer types by state
#'
#' @description
#' Based on the user-specified parameters of race, sex, age, stage, and year,
#' collect cancer incidence data for all desired cancer types by US state
#'
#' @param cancer_types A list of all desired cancer types (vector)
#' @param race The race of the population (character)
#' @param sex The sex of the population (character)
#' @param age The age group of the population (character)
#' @param stage The stage of the cancer (character)
#' @param year The desired timespan for the data (character)
#' @param con The duckdb database to be accessed
#'
#' @return
#' The output cancer incidence dataframe to be saved to duckdb for this
#' combination of parameters
#'
#' @examples
#' merge_all_incidence(
#'    c("all cancer sites", "thyroid"),
#'    "All Races (includes Hispanic)",
#'    "both sexes",
#'    "all ages",
#'    "all stages",
#'    "latest 5 year average",
#'     dbConnect(duckdb::duckdb(), "cancer-incidence-usa-state.duckdb")
#' )
#'
#'

merge_all_incidence <- function(cancer_types, race, sex, age, stage, year, con){
  # Get a list of relevant tables from the db related to the parameters of interest
  relevant_tables = c()
  # Iterate over each cancer type
  for (cancer in cancer_types) {
    # Figure out what the name of the table should be if it exists in the db
    table_name = get_incidence_db_name(cancer, race, sex, age, stage, year)
    # If the table exists in the db, add its name to our list
    if (table_name %in% dbListTables(con)) {
      relevant_tables = c(relevant_tables, table_name)
    }
  }

  # Merge relevant tables from the db to get final output data for visualization
  out = data.frame()
  # Iterate over tables in our db related to our parameters of interest
  for (tableName in relevant_tables) {
    # Get the type of cancer from the table's name
    cancerName = strsplit(tableName, "_")[[1]][3]
    # If this is the first table we're adding to the output...
    if (nrow(out) == 0) {
      # Read in the data, change the variable named "Age_Adjusted_Incidence_Rate"
      # to be the name of the cancer type, and select this new column as well as
      # the "State" column
      out = dbReadTable(con, tableName) %>%
        rename(!!cancerName := Age_Adjusted_Incidence_Rate) %>%
        select(State, !!cancerName)
    }
    # Otherwise, the output already has some information in it
    else {
      # Read in the data and join it to the existing output by State. Make sure
      # to rename the Incidence Rate column to correspond to the cancer type
      out <- out %>%
        left_join(
          dbReadTable(con, tableName) %>%
            rename(!!cancerName := Age_Adjusted_Incidence_Rate) %>%
            select(State, !!cancerName),
          by = "State"
        )
    }
  }

  # Return cancer incidence by state and cancer type
  return(out)
}
