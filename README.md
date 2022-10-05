# Shiny interface of the cfda package

Shiny dashboard for the [cfda](https://github.com/modal-inria/cfda) R package.

## Installation

Install the following R packages:

``` r
install.packages(c("cfda", "shinydashboard", "shinyMatrix", "shinyWidgets", "shinycssloaders", "dplyr", "DT", "plotly"))
```

## Run the shiny app

Once you have clone this repo, you can run the app with:

``` r
shiny::runApp()
```

To run the app without having this repo:

``` r
shiny::runGitHub("cfda_shiny", "modal-inria")
```
