
<!-- README.md is generated from README.Rmd. Please edit that file -->

# checkndr

<!-- badges: start -->
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

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(checkndr)
recencyApp() ## starts the recency data quality check dashboard
```

## Code of Conduct

Please note that the checkndr project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
