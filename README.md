# bacenR

<!-- badges: start -->

[![R-CMD-check](https://github.com/rtheodoro/bacenR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rtheodoro/bacenR/actions/workflows/R-CMD-check.yaml) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The goal of bacenR is to provide R functions to access, download, and work with data from the Central Bank of Brazil (BACEN).

The datasets available through bacenR include: 

- [Normative legislation and texts](https://www.bcb.gov.br/estabilidadefinanceira/buscanormas) 
- [Financial statements (balancetes) of banks, credit unions, and other financial institutions](https://www.bcb.gov.br/estabilidadefinanceira/balancetesbalancospatrimoniais)

## Installation

You can install the development version of bacenR like so:

``` r
# install.packages("remotes") # if you don't have remotes installed yet
remotes::install_github("rtheodoro/bacenR")
```

## Example

This is a basic example which shows you how to solve a common problem:

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
   primeiro_ano = 1993,
   ano_final = 2023,
   out_dir = "data_raw/balancetes",
   overwrite = FALSE
)

# Unified treatment of downloaded financial statements
treatment_balance_sheets(path_raw = "data_raw", out_dir = "out",
                      doc_filter = 4010, save = TRUE)

```

## TO-DO

More details about the to-do list can be found in the [GitHub project board](https://github.com/users/rtheodoro/projects/1/views/1).

-   Choose an idiom (Portuguese/English) for the package
-   Add more datasets from BACEN APIs
    -   [Information about institutions authorized, regulated, or supervised by the Central Bank of Brazil (BC)](https://www.bcb.gov.br/meubc/encontreinstituicao)
    -   [Datasets from IF.data](https://www3.bcb.gov.br/ifdata/) - I need to explode this in sub-tasks
    -   [Information about Institutions Operating in the Country (file transfer)](https://www.bcb.gov.br/estabilidadefinanceira/relacao_instituicoes_funcionamento)
    -   [Another datasets from BACEN APIs](https://dadosabertos.bcb.gov.br/)
    -   [Information about members of statutory bodies](https://github.com/rtheodoro/orgaos-estatutarios-coop-cred-bacen)
    -   Interest rates
    -   Information about inflation-indexed assets
    -   Continue...
-   Improve error handling and user messages
-   Enhance documentation and examples

Feel free to contribute to this list by [opening issues or pull requests on GitHub](https://github.com/rtheodoro/bacenR/issues)!

## Contact

Connect to me on LinkedIn: [Ricardo Theodoro](https://www.linkedin.com/in/rtheodoro/)

Follow me on Twitter/X: [\@rxtheodoro](https://x.com/rxtheodoro)

Follow me on BlueSky: [\@rtheodoro](https://bsky.app/profile/rtheodoro.com)

Send me an email at rtheodoro\@usp.br