# Filter input data by chosen values of selectInputs
#' @importFrom dplyr filter
#'
filter_input_data <- function(input_data, cancer_type, race, sex, age, stage, year) {
  input_data |>
    dplyr::filter(cancer_type == cancer_type) |>
    dplyr::filter(race == race) |>
    dplyr::filter(sex == sex) |>
    dplyr::filter(age == age) |>
    dplyr::filter(stage == stage) |>
    dplyr::filter(year == year)
}
