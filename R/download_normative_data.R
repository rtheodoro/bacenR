#' Download normative acts from the Central Bank (BCB) by terms and date range
#'
#' Queries the BCB normative search API, collects all results within the provided
#' date range and returns a data.frame with the records.
#' Data are downloaded from https://www.bcb.gov.br/estabilidadefinanceira/buscanormas
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' @param terms Character vector. Search terms; they will be concatenated with " OR " and URL-encoded.
#' @param ini_date Character. Start date in "YYYY-MM-DD" format.
#' @param end_date Character. End date in "YYYY-MM-DD" format.
#' @details
#' 1. Builds the query URL using the terms and the date range.
#' 2. Retrieves the total number of results to determine the page size.
#' 3. Iterates over result pages (incrementing by 500) until a page returns fewer than 13 rows (stop condition).
#' 4. Performs simple cleaning on returned fields:
#'    - removes "string;#" prefixes,
#'    - removes HTML tags in summaries and subjects,
#'    - removes the decimal part of normative numbers.
#' @return data.frame containing the records returned by the API and the post-processed columns.
#' @examples
#' \dontrun{
#' ini_date <- "2020-01-01"
#' end_date <- Sys.Date()
#' terms <- c("Cooperativas de Crédito", "Cooperativa de Crédito")
#' normas <- download_normative_data(terms, ini_date, end_date)
#' }
#' @export

download_normative_data <- function(terms, ini_date, end_date) {
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

# normative_data <- download_normative_data(terms, ini_date, end_date)
