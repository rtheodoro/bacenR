#' Download data from [IF.data of Brazilian Central Bank (Bacen)](https://olinda.bcb.gov.br/olinda/servico/IFDATA/versao/v1/aplicacao#!/recursos/IfDataValores#eyJmb3JtdWxhcmlvIjp7IiRmb3JtYXQiOiJ0ZXh0L2NzdiIsIiR0b3AiOjEwMCwiBDIEIjoiMjAxODAzIiwiUmVsYXRvcmlvIjoiVCIsIgQwBCI6IjEyMzQifSwicHJvcHJpZWRhZGVzIjpbMCwxLDIsMyw0LDUsNiw3LDgsOV0sInBlc3F1aXNhZG8iOnRydWUsImFjdGl2ZVRhYiI6InRhYmxlIiwiZ3JpZFN0YXRlIjp7AzADOlt7A0IDIgQwBCIsA0EDfSx7A0IDIgQxBCIsA0EDfSx7A0IDIgQyBCIsA0EDfSx7A0IDIgQzBCIsA0EDfSx7A0IDIgQ0BCIsA0EDfSx7A0IDIgQ1BCIsA0EDfSx7A0IDIgQ2BCIsA0EDfSx7A0IDIgQ3BCIsA0EDfSx7A0IDIgQ4BCIsA0EDfSx7A0IDIgQ5BCIsA0EDfV0sAzEDOnt9LAMyAzpbXSwDMwM6e30sAzQDOnt9LAM1Azp7fX0sInBpdm90T3B0aW9ucyI6ewNhAzp7fSwDYgM6W10sA2MDOjUwMCwDZAM6W10sA2UDOltdLANmAzpbXSwDZwM6ImtleV9hX3RvX3oiLANoAzoia2V5X2FfdG9feiIsA2kDOnt9LANqAzp7fSwDawM6ODUsA2wDOmZhbHNlLANtAzp7fSwDbgM6e30sA28DOiIENgRnZW0iLANwAzoiVGFibGUifX0=)
#'@description
#' `r lifecycle::badge("experimental")`
#'
#' Downloads IF.data values from the Brazilian Central Bank (Bacen) API for specified years,
#'  months, institution types, and report types. The function handles multiple combinations
#'  of parameters and returns a consolidated data frame with the results.
#'
#' @param year Numeric or vector. year (ex: 2024 or c(2023, 2024))
#' @param month Numeric or vector. month (1 to 12, or c(6, 12))
#' @param type_institution Numeric or vector. Type of Institution:
#'   1 = Conglomerados Prudenciais e Instituicoes Independentes
#'   2 = Conglomerados Financeiros e Instituicoes Independentes
#'   3 = Instituicoes Individuais (default)
#'   4 = Instituicoes com Operacoes de Cambio
#'   Or any combination of these (e.g., c(1, 3))
#' @param relatorio Character. Type of document: 'T' = Total (default), 'A' = Ativo, 'P' = Passivo
#' @param verbose Logical. Se TRUE, print progess messages (default: TRUE)
#'
#' @return Data frame with IF.data values or NULL in the case of errors.
#' @export
#'
#' @examples
#' \donttest{
#' # Unique institution type for a specific period
#' data <- get_ifdata_values(year = 2024, month = 12, type_institution = 3)
#' }
#'
#' \dontrun{
#' # Multiple institution types for a specific period
#' data <- get_ifdata_values(year = 2024, month = 12, type_institution = c(2, 3))
#'
#' # Multiple institution types for multiple periods
#' data <- get_ifdata_values(
#'   year = c(2023, 2024),
#'   month = c(6, 12),
#'   type_institution = c(1, 2, 3)
#' )
#' }
get_ifdata_values <- function(
  year,
  month,
  type_institution = 3,
  relatorio = "T",
  verbose = TRUE
) {
  # Validações
  if (any(!type_institution %in% 1:4)) {
    stop("type_institution should be 1, 2, 3, 4 or any combination of them")
  }

  if (any(month < 1 | month > 12)) {
    stop("Month shold be from 1 to 12")
  }

  CodInst <- as.character()
  Saldo <- as.character()

  year_atual <- as.numeric(format(Sys.Date(), "%Y"))
  if (any(year < 2000 | year > year_atual)) {
    warning(
      "Year out of expected. Data could be not available."
    )
  }

  # Grid combines year, month and type_institution
  grid_periodos <- expand.grid(
    year = year,
    month = month,
    type_institution = type_institution,
    stringsAsFactors = FALSE
  )

  if (verbose) {
    cat(sprintf("Total of requests: %d\n", nrow(grid_periodos)))
  }

  # Download function for a single period and institution type
  baixar_periodo <- function(year_i, month_i, tipo_inst_i) {
    # Formata year_month (YYYYMM)
    year_month <- sprintf("%d%02d", year_i, month_i)

    # Monta URL da API
    url <- glue::glue(
      "https://olinda.bcb.gov.br/olinda/servico/IFDATA/versao/v1/odata/",
      "IfDataValores(AnoMes=@AnoMes,TipoInstituicao=@TipoInstituicao,Relatorio=@Relatorio)?",
      "@AnoMes={year_month}&@TipoInstituicao={tipo_inst_i}&@Relatorio='{relatorio}'&",
      "$format=text/csv&",
      "$select=TipoInstituicao,CodInst,AnoMes,NomeRelatorio,NumeroRelatorio,",
      "Grupo,Conta,NomeColuna,DescricaoColuna,Saldo"
    )

    if (verbose) {
      cat(sprintf(
        "Downloading: %d/%02d | Tipo: %d... ",
        year_i,
        month_i,
        tipo_inst_i
      ))
    }

    tryCatch(
      {
        # Faz requisição GET
        response <- httr::GET(url, httr::timeout(60))

        # Verifica status
        if (httr::status_code(response) != 200) {
          if (verbose) {
            cat(sprintf("x HTTP %d\n", httr::status_code(response)))
          }
          return(NULL)
        }

        # Lê CSV da resposta
        conteudo <- httr::content(response, "text", encoding = "UTF-8")

        conteudo <- conteudo |>
          dplyr::mutate(
            Saldo = as.character(Saldo)
          )

        df <- readr::read_csv(
          conteudo,
          show_col_types = FALSE,
          locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
        )

        if (verbose) {
          cat(sprintf("Done %s register\n", format(nrow(df), big.mark = ".")))
        }

        return(df)
      },
      error = function(e) {
        if (verbose) {
          cat(sprintf("x Error: %s\n", e$monthsage))
        }
        return(NULL)
      }
    )
  }

  # Uses purrr to map over all combinations and download data
  resultado <- purrr::pmap_dfr(
    list(
      grid_periodos$year,
      grid_periodos$month,
      grid_periodos$type_institution
    ),
    baixar_periodo
  )

  resultado <- resultado |>
    dplyr::rename(cnpj = CodInst) |>
    janitor::clean_names()

  if (verbose && !is.null(resultado)) {
    cat(sprintf(
      "\n Done! Total of registers: %s | Unique institutions: %d | Types: %d\n",
      format(nrow(resultado), big.mark = "."),
      length(unique(resultado$CodInst)),
      length(unique(resultado$TipoInstituicao))
    ))
  }

  return(resultado)
}
