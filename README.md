
<!-- README.md is generated from README.Rmd. Please edit that file -->

# checkndr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/stephenbalogun/checkndr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stephenbalogun/checkndr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`{checkndr}` is a {Shiny} application for performing data quality checks
on the Nigeria HIV program de-identified patient-level line-lists
available through the National Data Repository (NDR) or similar
platforms. While the ideal data quality check is to have the validations
and checks at the point of data entry, the {checkndr} provides
additional layer of check upstream to validate/confirm what has been
entered and promptly identify them for corrective actions. It can also
help to provide further insight to what should be improved at the entry
level.

## Installation

You can install the development version of checkndr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("stephenbalogun/checkndr")
```

A {shiny} app version of the package is also available at:

- [Recency data quality
  checker](https://4gates.shinyapps.io/recencyApp/)

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(checkndr)
recencyApp() ## starts the recency data quality check dashboard
```

You can customise the ‘theme’ by setting the theme options e.g.

``` r
library(checkndr)
theme <- bslib::bs_theme(
  bg = "#202123",
  fg = "#B8BCC2",
  primary = "#EA80FC",
  secondary = "#48DAC6",
  base_font = c("Grandstander", "sans-serif"),
  code_font = c("Courier", "monospace"),
  heading_font = "'Helvetica Neue', Helvetica, sans-serif",
  "input-border-color" = "#EA80FC",
  version = 4
)

recencyApp(
  .theme = theme
)
```

## Code of Conduct

Please note that the checkndr project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
