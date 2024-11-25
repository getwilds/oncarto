# Get the right county boundaries for the desired choropleth visualization
#' @importFrom tigris counties
#' @importFrom sf st_transform
#' @importFrom dplyr mutate
#'
get_county_boundaries <- function(state_abbr) {
  # Get county boundaries for the choropleth visualization using the tigris package
  sf::st_transform(
    tigris::counties(state = state_abbr, class = "sf"),
    crs = 4326
  ) |> dplyr::mutate(County = NAMELSAD)
}
