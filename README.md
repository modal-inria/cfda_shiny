# Shiny interface of the cfda package

this is shiny dashboard for the cfda package. 

## Installation

``` r
install.packages(c("cfda","shinyMatrix","tidyverse","tractor.base","dplyr","shinydashboard","ggpubr","DT","plotly")) 
```

## Run the shiny app
``` r
if (!require('shiny')) install.packages("shiny")
shiny::runGitHub("cfda_shiny", "modal-inria")
```

