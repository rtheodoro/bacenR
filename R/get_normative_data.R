#' Download [normative acts from the Brazilian Central Bank (Bacen)](https://www.bcb.gov.br/estabilidadefinanceira/buscanormas) by terms and date range
#'
#' @description
#'  `r lifecycle::badge("stable")`
#'
#' Queries the Bacen normative search API, collects all results within the provided
#' date range and returns a data.frame with the records.
#' Data are downloaded from [https://www.bcb.gov.br/estabilidadefinanceira/buscanormas](https://www.bcb.gov.br/estabilidadefinanceira/buscanormas).
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
#'   Returns an empty data.frame if the server is unavailable or no internet connection.
#' @examples
#' \dontrun{
#' ini_date <- "2025-01-01"
#' end_date <- "2025-01-05"
#' terms <- c("Cooperativas", "Cooperativa")
#' normas <- get_normative_data(terms, ini_date, end_date)
#' }
#' @export
#'
get_normative_data <- function(terms, ini_date, end_date) {
  tryCatch(
    {
      # Verify site accessibility before proceeding
      if (!.is_site_accessible()) {
        warning(
          "Bacen API server is not accessible. ",
          "Please check your internet connection or try again later. ",
          "Returning empty data.frame."
        )
        return(data.frame())
      }

      terms_joined <- stringr::str_c(terms, collapse = " OR ") |> URLencode()

      site <- glue::glue(
        "https://www.bcb.gov.br/api/search/app/normativos/buscanormativos?querytext=ContentType:normativo%20AND%20contentSource:normativos%20AND%20{terms_joined}&rowlimit=15&startrow=0&sortlist=Data1OWSDATE:descending&refinementfilters=Data:range(datetime({ini_date}),datetime({end_date}))"
      )

      resp <- httr::GET(site, httr::timeout(10))

      # Check HTTP status
      if (httr::http_error(resp)) {
        warning(
          "Bacen API returned HTTP status ", httr::status_code(resp), ". ",
          "Bacen API returned HTTP status ", httr::status_code(resp), "."
        )
        return(data.frame())
      }

      json_file <- resp |>
        httr::content(as = "text") |>
        jsonlite::fromJSON()
      total_rows <- json_file$TotalRows

      message("total rows ", total_rows)

      startrow <- 0
      all_data <- data.frame()
      starts <- if (total_rows <= 0) integer(0) else seq(0, total_rows, by = 500)

      all_data <- purrr::map_dfr(starts, function(start) {
        site <- glue::glue(
          "https://www.bcb.gov.br/api/search/app/normativos/buscanormativos?querytext=ContentType:normativo%20AND%20contentSource:normativos%20AND%20{terms_joined}&rowlimit=500&startrow={start}&sortlist=Data1OWSDATE:descending&refinementfilters=Data:range(datetime({ini_date}),datetime({end_date}))"
        )

        resp <- httr::GET(site, httr::timeout(10))

        # Check HTTP status for each request
        if (httr::http_error(resp)) {
          message("HTTP status ", httr::status_code(resp), " on start row ", start)
          return(NULL)
        }

        rows <- resp |>
          httr::content(as = "text") |>
          jsonlite::fromJSON() |>
          purrr::pluck("Rows")

        if (is.null(rows) || length(rows) == 0) {
          message("No rows for start ", start)
          return(NULL)
        }

        df <- as.data.frame(rows) |>
          dplyr::mutate(
            dplyr::across(
              dplyr::any_of("RefinableString01"),
              ~ stringr::str_replace_all(., "string;#", "")
            ),
            dplyr::across(
              dplyr::any_of("AssuntoNormativoOWSMTXT"),
              ~ stringr::str_replace_all(., "<[^>]+>", "")
            ),
            dplyr::across(
              dplyr::any_of("RefinableString03"),
              ~ stringr::str_replace_all(., "string;#", "")
            ),
            dplyr::across(
              dplyr::any_of("HitHighlightedSummary"),
              ~ stringr::str_replace_all(., "<[^>]+>", "")
            ),
            dplyr::across(
              dplyr::any_of("NumeroOWSNMBR"),
              ~ stringr::str_replace_all(., "\\..*$", "")
            )
          )

        message("Row ", start)
        df
      })

      return(all_data)
    },
    error = function(e) {
      warning(
        "Unable to download normative data. ",
        "Error: ", conditionMessage(e)
      )
      data.frame()
    }
  )
}

#' @keywords internal
#'
.is_site_accessible <- function() {
  tryCatch(
    {
      resp <- httr::HEAD(
        "https://www.bcb.gov.br/api/search/app/normativos/buscanormativos",
        httr::timeout(5)
      )
      status <- httr::status_code(resp)
      status < 500
    },
    error = function(e) {
      FALSE
    }
  )
}