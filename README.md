# bacenR

<!-- badges: start -->

[![R-CMD-check](https://github.com/rtheodoro/bacenR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rtheodoro/bacenR/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of bacenR is to provide R functions to access, download, and work with data from the Central Bank of Brazil (BACEN) APIs.

## Installation

You can install the development version of bacenR like so:

``` r
# install.packages("remotes") # if you don't have remotes installed yet
remotes::install_github("rtheodoro/bacenR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(bacenR)
## basic example code
terms <- c("Cooperativas de Crédito", "Cooperativa de Crédito")
ini_date <- "2020-01-01"
end_date <- Sys.Date()

normative_data <- download_legislacao(terms, ini_date, end_date)
normative_txt <- download_texto_normativo(normative_data)
```
