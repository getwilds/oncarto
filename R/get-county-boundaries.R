# Get the right county boundaries for the desired choropleth visualization
#' @importFrom tigris counties
#' @importFrom sf st_transform
#'
get_county_boundaries <- function(state_abbr, county_col_name) {
  # Get county boundaries for the choropleth visualization using the tigris package
  out <- sf::st_transform(
    tigris::counties(state = state_abbr, class = "sf"),
    crs = 4326
  )

  out[[county_col_name]] = out[['NAMELSAD']]
  out
}
