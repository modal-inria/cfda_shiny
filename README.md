# Shiny interface of the cfda package

Shiny dashboard for the [cfda](https://github.com/modal-inria/cfda) R package.

## Installation

``` r
install.packages(c("cfda", "tidyverse", "shiny", "shinydashboard", "shinyMatrix", "shinyWidgets", "shinycssloaders", "tractor.base", "dplyr", "ggpubr", "DT", "plotly", "questionr", "scales", "stringr"))
```

## Run the shiny app

``` r
if (!require("shiny")) install.packages("shiny")
shiny::runGitHub("cfda_shiny", "modal-inria")
```
