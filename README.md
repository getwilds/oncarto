# Oncarto

<!-- badges: start -->
[![Project Status: Prototype – Useable, some support, open to feedback, unstable API.](https://getwilds.org/badges/badges/prototype.svg)](https://getwilds.org/badges/#prototype)
[![R-CMD-check](https://github.com/getwilds/oncarto/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/getwilds/oncarto/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Oncarto (Oncology Cartographer) is a flexible Shiny application, structured
as an R package, for visualizing and communicating geospatial data related to
cancer epidemiology.

## Installation

You can install the development version of `oncarto` as follows:

```{r}
# install.packages("pak")
pak::pak("getwilds/oncarto")
```

## Getting Started

Get Oncarto running locally:

```{r}
library(oncarto)

run_app(title = "Oncarto (Oncology Cartographer)", 
        logo_src = oncarto_file("fh-logo.png"), 
        logo_href = "https://hutchdatascience.org", 
        logo_width = "155px", 
        logo_height = "35px", 
        css = oncarto_file("fh.css"), 
        callback = example_callback)
```
