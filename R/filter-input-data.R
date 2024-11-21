#' Filter input data by chosen values of selectInputs
#' @importFrom dplyr filter
#'
filter_input_data <- function(input_data, cancer_type, race, sex, age, stage, year) {
  input_data |>
    filter(cancer_type == cancer_type) |>
    filter(race == race) |>
    filter(sex == sex) |>
    filter(age == age) |>
    filter(stage == stage) |>
    filter(year == year)
}
