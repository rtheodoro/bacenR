#' Download institution registry data from Bacen IF.data Cadastro
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Download [institution registry data from the Brazilian Central Bank (Bacen) IF.data](https://olinda.bcb.gov.br/olinda/servico/IFDATA/versao/v1/aplicacao#!/recursos/IfDataCadastro#eyJmb3JtdWxhcmlvIjp7IiRmb3JtYXQiOiJ0ZXh0L2NzdiIsIiR0b3AiOm51bGwsIkFub01lcyI6IjIwMTgwMyIsIiRmaWx0ZXIiOiIifSwicHJvcHJpZWRhZGVzIjpbMCwxLDIsMyw0LDUsNiw3LDgsOSwxMCwxMSwxMiwxMywxNCwxNV19) API for specified years and months. The function handles multiple combinations of parameters and returns a consolidated data frame with the results.
#'
#' @param year Numeric or vector. Year(s) to download (e.g., 2024 or c(2023, 2024))
#' @param month Numeric or vector. Month(s) to download (1 to 12)
#' @param verbose Logical. If TRUE, prints progress messages (default: TRUE)
#'
#' @return Data frame with institution registry data or NULL in case of error
#' @export
#'
#' @examples
#' \donttest{
#' # Single period
#' data <- get_ifdata_registry(year = 2024, month = 12)
#' }
#'\dontrun{
#' # Multiple months in the same year
#' data <- get_ifdata_registry(year = 2024, month = c(6, 12))
#'
#' # Multiple years and months (all combinations)
#' data <- get_ifdata_registry(year = c(2023, 2024), month = c(3, 6, 9, 12))
#'}
get_ifdata_registry <- function(year, month, verbose = TRUE) {
  # Validations
  if (any(month < 1 | month > 12)) {
    stop("month must be between 1 and 12")
  }

  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  if (any(year < 2000 | year > current_year)) {
    warning("Year outside expected range. Data may not be available.")
  }

  # Create grid with all combinations of year and month
  period_grid <- expand.grid(
    year = year,
    month = month,
    stringsAsFactors = FALSE
  )

  if (verbose) {
    cat(sprintf("Total requests: %d\n", nrow(period_grid)))
  }

  # Internal function to download a specific period
  download_period <- function(year_i, month_i) {
    # Format year_month (YYYYMM)
    year_month <- sprintf("%d%02d", year_i, month_i)

    # Build API URL
    url <- glue::glue(
      "https://olinda.bcb.gov.br/olinda/servico/IFDATA/versao/v1/odata/",
      "IfDataCadastro(AnoMes=@AnoMes)?@AnoMes={year_month}&",
      "$format=text/csv&",
      "$select=CodInst,Data,NomeInstituicao,DataInicioAtividade,Tcb,Td,Tc,",
      "SegmentoTb,Atividade,Uf,Municipio,Sr,CodConglomeradoFinanceiro,",
      "CodConglomeradoPrudencial,CnpjInstituicaoLider,Situacao"
    )

    if (verbose) {
      cat(sprintf("Downloading: %d/%02d... ", year_i, month_i))
    }

    tryCatch(
      {
        # Make GET request
        response <- httr::GET(url, httr::timeout(60))

        # Check status
        if (httr::status_code(response) != 200) {
          if (verbose) {
            cat(sprintf("x HTTP %d\n", httr::status_code(response)))
          }
          return(NULL)
        }

        # Read CSV from response
        content <- httr::content(response, "text", encoding = "UTF-8")

        df <- readr::read_csv(
          content,
          show_col_types = FALSE,
          locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
        )

        if (verbose) {
          cat(sprintf("Ok - %s records\n", format(nrow(df), big.mark = ",")))
        }

        return(df)
      },
      error = function(e) {
        if (verbose) {
          cat(sprintf("x Error: %s\n", e$message))
        }
        return(NULL)
      }
    )
  }

  # Use purrr::map2_dfr to download all periods and combine into a dataframe
  result <- purrr::map2_dfr(
    period_grid$year,
    period_grid$month,
    download_period
  )

  result <- result |>
    janitor::clean_names()

  if (verbose && !is.null(result)) {
    cat(sprintf(
      "\n Completed! Total records: %s\n",
      format(nrow(result), big.mark = ","),
      length(unique(result$CodInst))
    ))
  }

  return(result)
}
