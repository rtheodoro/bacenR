# bacenR

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/bacenR)](https://CRAN.R-project.org/package=bacenR) [![R-CMD-check](https://github.com/rtheodoro/bacenR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rtheodoro/bacenR/actions/workflows/R-CMD-check.yaml) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The goal of `bacenR` is to provide R functions to download and work with data from the Brazilian Central Bank (Bacen).

The datasets available through `bacenR` include:

-   [Normative legislation](https://www.bcb.gov.br/estabilidadefinanceira/buscanormas)
-   [Financial statements of financial institutions](https://www.bcb.gov.br/estabilidadefinanceira/balancetesbalancospatrimoniais)
-   [List of financial institutions regulated by Bacen](https://www.bcb.gov.br/estabilidadefinanceira/relacao_instituicoes_funcionamento)


For IPCA, Selic, GDP, Government Debt, and others, check the packages: [BacenAPI](https://cran.r-project.org/web/packages/BacenAPI/index.html) and [rbcb](https://cran.r-project.org/web/packages/rbcb/index.html) packages.

## Summary

-   [Install](#install)
-   [Usage](#usage)
-   [To-do](#to-do)
-   [Author](#author)
-   [How to cite](#how-to-cite)
-   [Support](#support)

## Install

You can install the development version of `bacenR` like so:

``` r
# install.packages("pak") # if you don't have pak installed yet
pak::pak("rtheodoro/bacenR")
```

## Usage

This is a basic example of use:

``` r
# Load the package
library(bacenR)

# Download normative legislation related to Credit Cooperatives
normative_data <- get_normative_data(
    terms = c("Cooperativas de Crédito", "Cooperativa de Crédito"), 
    ini_date = "2020-01-01", 
    end_date = Sys.Date()
)

# Download texts of the normative legislation
normative_txt <- get_normative_txt(normative_data)

# Download financial statements of banks and credit cooperatives
get_balance_sheets(
   instituicao = c("BANCOS", "COOPERATIVAS"),
   meses = c(6, 12),
   first_year = 1993,
   final_year = 2023,
   out_dir = "data_raw",
   overwrite = FALSE
)

# Unified treatment of downloaded financial statements
tidy_balance_sheets(
    path_raw = "data_raw", 
    out_dir = "out",
    doc_filter = 4010, 
    save = TRUE
)

# Download list of financial institutions regulated by Bacen
get_institutions(
  institution = c("COOPERATIVAS", "BANCOS"),
  start_date = "202312",
  end_date = "202405",
  out_dir = "data_raw",
  cleanup_zip = TRUE,
  verbose = TRUE
)

# Tidy the institutions data
tidy_institutions(
  path_raw = "data_raw",
  out_dir = "data"
)


```

## To-do

More details about the to-do list can be found in the [GitHub project board](https://github.com/users/rtheodoro/projects/1/views/1).

-   Functions to collect more datasets from Bacen
    -   [Institutions authorized, regulated, or supervised](https://www.bcb.gov.br/meubc/encontreinstituicao)
    -   [Board members](https://github.com/rtheodoro/orgaos-estatutarios-coop-cred-bacen)
    -   [Datasets from IF.data](https://www3.bcb.gov.br/ifdata/)
    -   [Datasets from Bacen.API](https://dadosabertos.bcb.gov.br/)
 
Feel free to contribute to this list by [opening issues or pull requests on GitHub](https://github.com/rtheodoro/bacenR/issues)!

## Author

**Ricardo Theodoro**:

-   Economist, Ph.D. candidate and Master's degree holder in Accounting at [FEARP/USP](http://www.fearp.usp.br/).

-   Researcher at [OBSCOOP/USP](https://linktr.ee/obscoopusp)

-   Contact information:

    -   LinkedIn: [Ricardo Theodoro](https://www.linkedin.com/in/rtheodoro/)

    -   Twitter/X: [\@rxtheodoro](https://x.com/rxtheodoro)

    -   BlueSky: [\@rtheodoro](https://bsky.app/profile/rtheodoro.com)

    -   Email: rtheodoro\@usp.br


## How to cite

To cite `bacenR` in publications, please use:

``` r
citation("bacenR")
```

```         
@manual{,
  title = "bacenR: Tools to Access Data from Banco Central do Brasil",
  author = "Ricardo Theodoro",
  year = "2026",
  version = "v0.2.0",
  url = "https://github.com/rtheodoro/bacenR"
}
```

## Support

If this package is useful and saves you time, please consider starring this GitHub repository.

You can also buy me a coffee via [PIX](https://nubank.com.br/cobrar/ddat/695e7490-e957-4d60-be76-87f08a6d292c).