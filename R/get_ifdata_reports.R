#' Download institution reports data from [IFdata of Brazilian Central Bank (Bacen)](https://olinda.bcb.gov.br/olinda/servico/IFDATA/versao/v1/aplicacao) Relatorios
#'@description
#'  `r lifecycle::badge("stable")`
#'
#' Download IFdata reports from the Brazilian Central Bank (Bacen) API for specified years,
#'  months, institution types, and report types. The function handles multiple combinations
#'  of parameters and returns a consolidated data frame with the results.
#'
#' Also, check the page to see the available reports for each institution type and date: [IFdata](https://www3.bcb.gov.br/ifdata/index2024.html).
#'
#' @param year Numeric or vector. year (ex: 2024 or c(2023, 2024))
#' @param month Numeric or vector. month (ex: 1 to 12, or c(6, 12))
#' @param type_institution Numeric. Type of Institution:
#' \itemize{
#'  \item 1 = Conglomerados Prudenciais e Instituicoes Independentes
#'  \item  2 = Conglomerados Financeiros e Instituicoes Independentes
#'  \item 3 = Instituicoes Individuais
#'  \item 4 = Instituicoes com Operacoes de Cambio
#' }
#' @param report Numeric. Report type:
#'\itemize{
#'  \item 1 = Resumo
#'  \item 2 = Ativo
#'  \item 3 = Passivo
#'  \item 4 = Demonstracao de Resultado
#'  \item 5 = Informacoes de Capital
#'  \item 6 = Segmentacao
#'  \item 7 = Carteira de Credito Ativa - Por indexador
#'  \item 8 = Carteira de Credito ativa - por nivel de risco da operacao
#'  \item 9 = Carteira de Credito ativa - por regiao geografica
#'  \item 10 = Carteira de Credito ativa - quantidade de clientes e de operacoes
#'  \item 11 = Carteira de Credito ativa Pessoa Fisica - modalidade e prazo de vencimento
#'  \item 12 = Carteira de Credito ativa Pessoa Juridica - por atividade economica (CNAE)
#'  \item 13 = Carteira de Credito ativa Pessoa Juridica - modalidade e prazo de vencimento
#'  \item 14 = Carteira de Credito ativa Pessoa Juridica - por porte do tomador
#'  \item 15 = Movimentacao de Cambio no Trimestre
#' \item  16 = Carteira de Credito ativa - por carteiras de instrumentos financeiros
#' }
#' @param verbose Logical. If TRUE, print progress messages (default: TRUE)
#'
#' @return Data frame with IFdata values or NULL in the case of errors.
#' @export
#'
#' @examples
#' \donttest{
#' # Unique institution type for a specific period
#' data <- get_ifdata_reports(
#'  year = 2024,
#'  month = 12,
#'  report = 1,
#'  type_institution = 2
#' )
#'
#' # Multiple periods, take some time to run
#' data <- get_ifdata_reports(
#'   year = c(2023, 2024),
#'   month = c(6, 12),
#'   report = 1,
#'   type_institution = 2
#' )
#' }
get_ifdata_reports <- function(
  year,
  month,
  type_institution,
  report,
  verbose = TRUE
) {
  # Validate
  if (any(!type_institution %in% 1:4)) {
    stop("type_institution should be 1, 2, 3 or 4")
  }

  if (any(!report %in% 1:16)) {
    stop("report should be between 1 and 16")
  }

  if (any(month < 1 | month > 12)) {
    stop("Month should be from 1 to 12")
  }

  year_atual <- as.numeric(format(Sys.Date(), "%Y"))
  if (any(year < 2000 | year > year_atual)) {
    warning(
      "Year out of expected. Data could be not available."
    )
  }

  # Grid combines year, month, report and type_institution
  grid_periodos <- expand.grid(
    year = year,
    month = month,
    report = report,
    type_institution = type_institution,
    stringsAsFactors = FALSE
  )

  if (verbose) {
    cat(sprintf("\nTotal of requests: %d\n\n", nrow(grid_periodos)))
  }

  Saldo = as.character()
  TipoInstituicao = as.character()
  CodInst = as.character()
  AnoMes = as.character()
  NumeroRelatorio = as.character()
  Conta = as.character()

  # Download function for a single period and institution type
  baixar_periodo <- function(year_i, month_i, report_1, tipo_inst_i) {
    # Formata year_month (YYYYMM)
    year_month <- sprintf("%d%02d", year_i, month_i)

    # Monta URL da API
    url <- glue::glue(
      "https://olinda.bcb.gov.br/olinda/servico/IFDATA/versao/v1/odata/IfDataValores(AnoMes=@AnoMes,TipoInstituicao=@TipoInstituicao,Relatorio=@Relatorio)?@AnoMes={year_month}&@TipoInstituicao={tipo_inst_i}&@Relatorio='{report_1}'&$format=text/csv&$select=TipoInstituicao,CodInst,AnoMes,NomeRelatorio,NumeroRelatorio,Grupo,Conta,NomeColuna,DescricaoColuna,Saldo"
    )

    if (verbose) {
      cat(sprintf(
        "Downloading: %d/%02d | Type Institution: %d | Report: %d... \n",
        year_i,
        month_i,
        tipo_inst_i,
        report_1
      ))
    }

    tryCatch(
      {
        # GET
        response <- httr::GET(url, httr::timeout(200))

        # Status
        if (httr::status_code(response) != 200) {
          if (verbose) {
            cat(sprintf("x HTTP %d\n", httr::status_code(response)))
          }
          return(NULL)
        }

        # Read CSV content
        conteudo <- httr::content(response, "text", encoding = "UTF-8")

        df <- readr::read_csv(
          conteudo,
          show_col_types = FALSE,
          locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
        ) |>
          dplyr::mutate(
            CodInst = as.character(CodInst),
            TipoInstituicao = as.character(TipoInstituicao),
            AnoMes = as.character(AnoMes),
            NumeroRelatorio = as.character(NumeroRelatorio),
            Conta = as.character(Conta),
            Saldo = as.character(Saldo)
          )

        if (verbose) {
          cat(sprintf("Done %s register\n\n", format(nrow(df), big.mark = ",")))
        }

        return(df)
      },
      error = function(e) {
        if (verbose) {
          cat(sprintf("x Error: %s\n", e$monthsage))
        }
        cat(sprintf("x Error: %s\n", e$message))
      }
    )
  }

  # Uses purrr to map over all combinations and download data
  resultado <- purrr::pmap_dfr(
    list(
      year_i = grid_periodos$year,
      month_i = grid_periodos$month,
      report_1 = grid_periodos$report,
      tipo_inst_i = grid_periodos$type_institution
    ),
    baixar_periodo
  )

  resultado <- resultado |>
    janitor::clean_names()

  if (verbose && !is.null(resultado)) {
    cat(sprintf(
      "\n----Done! Total of registers: %s \n",
      format(nrow(resultado), big.mark = ",")
    ))
  }

  return(resultado)
}
