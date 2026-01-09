# bacenR

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/bacenR)](https://CRAN.R-project.org/package=bacenR) [![R-CMD-check](https://github.com/rtheodoro/bacenR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rtheodoro/bacenR/actions/workflows/R-CMD-check.yaml) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The goal of bacenR is to provide R functions to access, download, and work with data from the Central Bank of Brazil (BACEN).

The datasets available through bacenR include:

-   [Normative legislation and texts](https://www.bcb.gov.br/estabilidadefinanceira/buscanormas)
-   [Financial statements (balancetes) of banks, credit unions, and other financial institutions](https://www.bcb.gov.br/estabilidadefinanceira/balancetesbalancospatrimoniais)

## Summary

-   [Installation](#installation)
-   [Example](#example)
-   [To-do](#to-do)
-   [Author](#author)
-   [Support](#support)
-   [How to cite](#how-to-cite)

## Installation

You can install the development version of bacenR like so:

``` r
# install.packages("remotes") # if you don't have remotes installed yet
remotes::install_github("rtheodoro/bacenR")
```

## Example

This is a basic example of use:

``` r
# Load the package
library(bacenR)

# Download legislation related to Credit Cooperatives
terms <- c("Cooperativas de Crédito", "Cooperativa de Crédito")
ini_date <- "2020-01-01"
end_date <- Sys.Date()

normative_data <- download_normative_data(terms, ini_date, end_date)
normative_txt <- download_normative_txt(normative_data)


# Download financial statements of banks and credit cooperatives
download_balance_sheets(
   instituicao = c("BANCOS", "COOPERATIVAS"),
   meses = c(6, 12),
   first_year = 1993,
   final_year = 2023,
   out_dir = "data_raw/balancetes",
   overwrite = FALSE
)

# Unified treatment of downloaded financial statements
treatment_balance_sheets(path_raw = "data_raw", out_dir = "out",
                      doc_filter = 4010, save = TRUE)
```

## To-do

More details about the to-do list can be found in the [GitHub project board](https://github.com/users/rtheodoro/projects/1/views/1).

-   Functions to collect more datasets from BACEN
    -   [Information about institutions authorized, regulated, or supervised by the Central Bank of Brazil (BC)](https://www.bcb.gov.br/meubc/encontreinstituicao)
    -   [Datasets from IF.data](https://www3.bcb.gov.br/ifdata/) - I need to explode this in sub-tasks
    -   [Information about Institutions Operating in the Country (file transfer)](https://www.bcb.gov.br/estabilidadefinanceira/relacao_instituicoes_funcionamento)
    -   [Another datasets from BACEN APIs](https://dadosabertos.bcb.gov.br/)
    -   [Information about members of statutory bodies](https://github.com/rtheodoro/orgaos-estatutarios-coop-cred-bacen)
    -   Interest rates
    -   Information about inflation-indexed assets

Feel free to contribute to this list by [opening issues or pull requests on GitHub](https://github.com/rtheodoro/bacenR/issues)!

## Author

**Ricardo Theodoro**:

-   Economist, Ph.D. candidate and Master's degree holder in Accounting at [USP/FEARP](http://www.fearp.usp.br/).

-   Researcher at [OBSCOOP/USP](https://linktr.ee/obscoopusp)

-   Contact information:

    -   LinkedIn: [Ricardo Theodoro](https://www.linkedin.com/in/rtheodoro/)

    -   Twitter/X: [\@rxtheodoro](https://x.com/rxtheodoro)

    -   BlueSky: [\@rtheodoro](https://bsky.app/profile/rtheodoro.com)

    -   Email: rtheodoro\@usp.br

## Support

If this package is useful and saves you time, please consider starring this GitHub repository.

You can also buy me a coffee via [PIX](https://nubank.com.br/cobrar/ddat/695e7490-e957-4d60-be76-87f08a6d292c).

## How to cite

To cite bacenR in publications, please use:

``` r
citation("bacenR")
```

```         
@Unpublished{,
    title = {bacenR: Tools to Access Data from Banco Central do Brasil},
    author = {Ricardo Theodoro},
    journal = {Github},
    year = {2026},
    note = {R package version 0.1.0},
    url = {https://github.com/rtheodoro/bacenR},
}
```