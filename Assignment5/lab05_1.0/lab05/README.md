# 732A94 Advanced Programming in R Lab5

 <!-- badges: start -->
  [![R-CMD-check](https://github.com/qqyfly/732A94_Advanced_Programming_in_R_Lab5/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/qqyfly/732A94_Advanced_Programming_in_R_Lab5/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

# Overview

The `lab05` is an API package along with a shiny app. This vignette serves as a guide to understanding and using the main functions provided by the package.

# Installation 

To install the `lab05` package, use the following commands:

```
devtools::install_github("qqyfly/732A94_Advanced_Programming_in_R_Lab5")
library(lab05)
```

# Show the shiny webpage

The shiny web page is a demo site to show api calls to the `lab05` api package.
before running website, you need to install shiny, shinydashboard and ggplot2 package

```
shiny::runGitHub(repo="732A94_Advanced_Programming_in_R_Lab5",username="qqyfly",subdir="web")
