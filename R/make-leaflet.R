# Make the choropleth map
#' @importFrom leaflet leaflet addTiles addPolygons addLegend
#'
make_leaflet <- function(in_data, in_palette) {

  leaflet::leaflet(data = in_data) |>
    leaflet::addTiles() |>
    leaflet::addPolygons(
      fillColor = ~in_palette(Age_Adjusted_Incidence_Rate),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 3,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~paste(County, ": ", Age_Adjusted_Incidence_Rate),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) |>
    leaflet::addLegend(
      pal = in_palette,
      values = ~Age_Adjusted_Incidence_Rate,
      opacity = 0.7,
      title = "Age-Adjusted Cancer Incidence Rate",
      position = "topright"
    )
}
