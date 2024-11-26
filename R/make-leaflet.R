# Make the choropleth map
#' @importFrom leaflet leaflet addTiles addPolygons addLegend labelOptions
#' highlightOptions
#'
make_leaflet <- function(in_data, in_palette, incidence_col_name,
                         county_col_name, legend_title) {

  leaflet::leaflet(data = in_data) |>
    leaflet::addTiles() |>
    leaflet::addPolygons(
      fillColor = ~in_palette(
        in_data[[incidence_col_name]]
      ),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlightOptions = leaflet::highlightOptions(
        weight = 3,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~paste(in_data[[county_col_name]], ": ",
                     in_data[[incidence_col_name]]),
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) |>
    leaflet::addLegend(
      pal = in_palette,
      values = ~in_data[[incidence_col_name]],
      opacity = 0.7,
      title = legend_title,
      position = "topright"
    )
}
