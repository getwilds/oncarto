# Generate palette for choropleth map
#' @importFrom leaflet colorNumeric
#'
make_palette <- function(in_data, col_name, lower_color, upper_color,
                         na_color) {

  leaflet::colorNumeric(
    c(lower_color, upper_color),
    domain = in_data[[col_name]],
    na.color = na_color
  )
}
