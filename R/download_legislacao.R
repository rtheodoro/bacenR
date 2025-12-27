#' Baixa normativos do Banco Central (BCB) por termos e intervalo de datas
#'
#' Faz uma consulta à API de busca de normativos do BCB, coleta todos os resultados
#' no intervalo de datas fornecido e retorna um data.frame com os registros.
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' @param terms Character vector. Termos de busca; serão concatenados com " OR " e URL-encoded.
#' @param ini_date Character. Data inicial no formato "YYYY-MM-DD".
#' @param end_date Character. Data final no formato "YYYY-MM-DD".
#' @details
#' 1. Constrói a URL de consulta usando os termos e o intervalo de datas.
#' 2. Recupera o total de resultados para definir o tamanho da página.
#' 3. Itera sobre as páginas de resultado (incremento de 500 em 500) até que a página retornada tenha menos de 13 linhas (condição de parada).
#' 4. Realiza limpeza simples em campos retornados:
#'    - remove prefixos "string;#",
#'    - remove tags HTML em resumos e assuntos,
#'    - remove parte decimal de números de norma.
#' @return data.frame contendo os registros retornados pela API e as colunas pós-processadas.
#' @examples
#' \dontrun{
#' ini_date <- "2020-01-01"
#' end_date <- Sys.Date()
#' terms <- c("Cooperativas de Crédito", "Cooperativa de Crédito")
#' normas <- download_legislacao(terms, ini_date, end_date)
#' }
#' @export

download_legislacao <- function(terms, ini_date, end_date) {
  # Juntar os termos com " OR " e substituir espaços por "%20"
  terms_joined <- stringr::str_c(terms, collapse = " OR ") |> URLencode()

  # Pegando Qtd de linhas
  site <- glue::glue(
    "https://www.bcb.gov.br/api/search/app/normativos/buscanormativos?querytext=ContentType:normativo%20AND%20contentSource:normativos%20AND%20{terms_joined}&rowlimit=15&startrow=0&sortlist=Data1OWSDATE:descending&refinementfilters=Data:range(datetime({ini_date}),datetime({end_date}))"
  )
  json_file <- httr::GET(site) |>
    httr::content(as = "text") |>
    jsonlite::fromJSON()
  total_rows <- json_file$TotalRows

  startrow <- 0
  all_data <- data.frame()

  repeat {
    site <- glue::glue(
      "https://www.bcb.gov.br/api/search/app/normativos/buscanormativos?querytext=ContentType:normativo%20AND%20contentSource:normativos%20AND%20{terms_joined}&rowlimit={total_rows}&startrow={startrow}&sortlist=Data1OWSDATE:descending&refinementfilters=Data:range(datetime({ini_date}),datetime({end_date}))"
    )

    response <- httr::GET(site) |>
      httr::content(as = "text") |>
      jsonlite::fromJSON() |>
      purrr::pluck("Rows") |>
      as.data.frame()

    if (nrow(response) < 13) {
      break
    }

    response <- response |>
      dplyr::mutate(
        RefinableString01 = stringr::str_replace_all(
          RefinableString01,
          "string;#",
          ""
        ),
        AssuntoNormativoOWSMTXT = stringr::str_replace_all(
          AssuntoNormativoOWSMTXT,
          "<[^>]+>",
          ""
        ),
        RefinableString03 = stringr::str_replace_all(
          RefinableString03,
          "string;#",
          ""
        ),
        HitHighlightedSummary = stringr::str_replace_all(
          HitHighlightedSummary,
          "<[^>]+>",
          ""
        ),
        NumeroOWSNMBR = stringr::str_replace_all(NumeroOWSNMBR, "\\..*$", "")
      )

    all_data <- rbind(all_data, response)
    message(startrow)
    startrow <- startrow + 500
  }

  return(all_data)
}

# ini_date <- "2020-01-01"
# end_date <- lubridate::today()
# terms <- c("Cooperativas de Crédito", "Cooperativa de Crédito")

# normative_data <- download_legislacao(terms, ini_date, end_date)
