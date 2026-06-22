# bacenR
<img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/bacenR)](https://CRAN.R-project.org/package=bacenR) [![CRAN downloads](http://cranlogs.r-pkg.org/badges/grand-total/bacenR?color=yellow)](https://cran.r-project.org/package=bacenR) [![R-CMD-check](https://github.com/rtheodoro/bacenR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rtheodoro/bacenR/actions/workflows/R-CMD-check.yaml) [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

<!-- badges: end -->

The goal of `bacenR` is to provide R functions to download and work with data from the Brazilian Central Bank (Bacen).

- The datasets available through `bacenR` include:
  -   [Normative legislation](https://www.bcb.gov.br/estabilidadefinanceira/buscanormas)
  -   [Financial statements of financial institutions](https://www.bcb.gov.br/estabilidadefinanceira/balancetesbalancospatrimoniais)
  -  [List of financial institutions regulated by Bacen in activity](https://www.bcb.gov.br/estabilidadefinanceira/relacao_instituicoes_funcionamento)
  - [Ifdata resources](https://olinda.bcb.gov.br/olinda/servico/IFDATA/versao/v1/aplicacao#!/recursos) / [IFdata](https://www3.bcb.gov.br/ifdata/index2024.html)


To get data about IPCA, Selic, GDP, Government Debt, and others, check the packages:

  - [BacenAPI](https://CRAN.R-project.org/package=BacenAPI)
  - [rbcb](https://CRAN.R-project.org/package=rbcb)

## Summary

-   [Install](#install)
-   [Usage](#usage)
-   [To-do](#to-do)
-   [Author](#author)
-   [How to cite](#how-to-cite)
-   [Support](#support)

## Install

You can install the *stable* version of `bacenR` like so:

```r
install.packages("bacenR")
```

Or, you can install the *development* version of `bacenR` like so:

```r
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
   institution = c("BANCOS", "COOPERATIVAS"),
   months = c(6, 12),
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

# Get data from IFdata Reports
cc_ativa_pj_modalidade_20142024 <- bacenR::get_ifdata_reports(
  year = c(2014:2024),
  month = 12,
  report = 13,
  type_institution = 2
)

# Tidy data from IFdata Reports
tidy_cc_ativa_pj_modalidade_20142024 <- bacenR::tidy_ifdata_reports(
  cc_ativa_pj_modalidade_20142024
)

# Get data from IFdata Registry (Cadastro)
data <- get_ifdata_registry(
  year = c(2023, 2024), 
  month = c(3, 6, 9, 12)
)
```

## To-do

More details about the to-do list can be found in the [GitHub project board](https://github.com/users/rtheodoro/projects/1/views/1).

-   Functions to collect more datasets from Bacen
    -   [Institutions authorized, regulated, or supervised](https://www.bcb.gov.br/meubc/encontreinstituicao)
    -   [Board members](https://github.com/rtheodoro/orgaos-estatutarios-coop-cred-bacen)
    -   [Datasets from Bacen.API](https://dadosabertos.bcb.gov.br/)
 
Feel free to contribute to this list by [opening issues or pull requests on GitHub](https://github.com/rtheodoro/bacenR/issues)!

## Author

**Ricardo Theodoro**:

-   Economist, Ph.D. candidate and Master's degree holder in Accounting at [FEARP/USP](https://www.fearp.usp.br/).

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
  title = "bacenR: Access Data from Brazilian Central Bank - IFdata, Active Institutions, Balance Sheets and Normative Acts",
  author = "Ricardo Theodoro",
  year = "2026",
  version = "v0.4.3",
  doi = "10.32614/CRAN.package.bacenR",
  url = "https://github.com/rtheodoro/bacenR"
}
```

## Support

If this package is useful and saves you time, please consider starring this GitHub repository.

You can also give a penny to charity.