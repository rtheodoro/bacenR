#' Download balance sheets from [Banco Central do Brasil](https://www.bcb.gov.br/estabilidadefinanceira/balancetesbalancospatrimoniais)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Downloads monthly "balancetes" (.CSV or .ZIP) released by the Banco Central do Brasil
#' for one or more institution types and stores them in a local directory. ZIP
#' files found in the destination directory are also extracted (errors during
#' individual extractions are ignored).
#'
#' @param instituicao Character vector of institution types to download.
#'   Accepted values (case-insensitive): "BANCOS" to get data from, "COOPERATIVAS", "CONSORCIOS" (Administradoras de consórcios),
#'   "CONGLOMERADOS" (Conglomerados financeiros), "SOCIEDADES", "BLOPRUDENCIAL" (Conglomerados Prudenciais), "COMBINADOS" (Combinados Cooperativos), "LIQUIDACAO" (Instituições em Regime Especial).
#'   The function maps these tokens to the type names used in the [BCB URL](https://www.bcb.gov.br/estabilidadefinanceira/balancetesbalancospatrimoniais).
#' @param meses Integer or integer vector specifying months to download. Values
#'   should be 1..12 (single values or vectors like c(6, 12)). When a month
#'   single-digit is provided it is zero-padded to two digits in filenames/URLs.
#' @param first_year Integer, first year to download (inclusive). Defaults to
#'   1993 (the oldest available balancete in the source).
#' @param final_year Integer, last year to download (inclusive). Defaults to the
#'   previous calendar year.
#' @param out_dir Character, directory where downloaded ZIP files (and extracted
#'   CSVs) will be written. The directory is created if it does not exist.
#' @param overwrite Logical, whether to overwrite existing files in out_dir
#'   (default FALSE). If FALSE and a file already exists at the destination it
#'   is treated as a successful local result.
#'
#' @details
#' For each combination of year, month and institution type the function
#' constructs a URL of the form:
#' `https://www.bcb.gov.br/content/estabilidadefinanceira/cosif/{tipo}/{YYYY}{MM}{INSTITUICAO}.CSV.ZIP`
#' and attempts to download it with httr::GET. Successful and failed attempts
#' are recorded in the returned tibble. After attempting all downloads the
#' function tries to unzip any .zip files found in out_dir; unzip errors for
#' individual files are caught and ignored so the process continues.
#'
#' Invalid institution tokens cause an immediate error listing the invalid
#' entries.
#'
#' @return A tibble with one row per attempted download and the following
#'   columns:
#'   - ano: Year attempted (integer).
#'   - meses: Month attempted (integer).
#'   - instituicao: Institution token used (character).
#'   - url: The URL attempted (character).
#'   - dest: Destination file path for the downloaded ZIP (character).
#'   - success: Logical indicating if the download (or existing-file fallback)
#'     was considered successful.
#'   - http_status: HTTP status code returned by the request (integer NA if not
#'     available).
#'   - error: Character string with an error message when success is FALSE, or
#'     a short note when a local file existed and was not overwritten.
#'
#' @examples
#' \dontrun{
#' # Download balancetes for banks and cooperatives for June and December from 1993 to 2023
#' download_balance_sheets(
#'   instituicao = c("BANCOS", "COOPERATIVAS"),
#'   meses = c(6, 12),
#'   first_year = 1993,
#'   final_year = 2023,
#'   out_dir = "data_raw",
#'   overwrite = FALSE
#' )
#' }
#'
#' @references
#' Source Banco Central do Brasil (Bacen): [https://www.bcb.gov.br/estabilidadefinanceira/balancetesbalancospatrimoniais](https://www.bcb.gov.br/estabilidadefinanceira/balancetesbalancospatrimoniais)
#' Note:
#'   The values are not changed or processed in any way, they are downloaded as-is from the source.
#'   Check the function `treat_balance_sheets()` for tools to read and process
#'   Site: https://www.bcb.gov.br/estabilidadefinanceira/balancetesbalancospatrimoniais
#'
#' @export
#'
download_balance_sheets <- function(
   instituicao = "COOPERATIVAS",
   meses = 12,
   first_year = 1993,
   final_year = as.numeric(format(Sys.time(), "%Y")) - 1,
   out_dir = "data_raw",
   overwrite = FALSE
) {
   # Dependencies used: glue, httr, purrr, tibble, stringr, tidyr, fs
   if (!fs::dir_exists(out_dir)) {
      fs::dir_create(out_dir)
   }

   tipo_lookup <- c(
      BANCOS = "Bancos",
      COOPERATIVAS = "Cooperativas-de-credito",
      CONSORCIOS = "Administradoras-de-consorcios",
      CONGLOMERADOS = "Conglomerados-financeiros",
      SOCIEDADES = "Sociedades",
      BLOPRUDENCIAL = "Conglomerados-prudenciais",
      COMBINADOS = "Combinados-cooperativos",
      LIQUIDACAO = "Instituicoes-em-regime-especial"
   )

   instituicao <- toupper(instituicao)
   invalido <- setdiff(instituicao, names(tipo_lookup))
   if (length(invalido) > 0) {
      stop("Instituicao(s) invalida(s): ", paste(invalido, collapse = ", "))
   }

   combos <- tidyr::crossing(
      ano = first_year:final_year,
      mes = meses,
      instituicao = instituicao
   ) |>
      dplyr::mutate(
         mes2 = stringr::str_pad(
            as.character(mes),
            width = 2,
            side = "left",
            pad = "0"
         ),
         tipo = unname(tipo_lookup[instituicao])
      )

   download_single_balancete <- function(
      ano,
      mes,
      instituicao,
      mes2,
      tipo,
      out_dir,
      overwrite
   ) {
      # try candidates in order: .CSV.ZIP, .CSV, .ZIP and pick first that returns HTTP 200
      suffixes <- c(".CSV.ZIP", ".CSV", ".ZIP")
      # default to the first candidate
      url <- glue::glue(
         "https://www.bcb.gov.br/content/estabilidadefinanceira/cosif/{tipo}/{ano}{mes2}{instituicao}{suffixes[1]}"
      )
      dest <- file.path(
         out_dir,
         glue::glue("{ano}{mes2}_{instituicao}{suffixes[1]}")
      )

      candidate <- purrr::map(
         suffixes,
         ~ {
            suf <- .
            candidate_url <- glue::glue(
               "https://www.bcb.gov.br/content/estabilidadefinanceira/cosif/{tipo}/{ano}{mes2}{instituicao}{suf}"
            )
            head_resp <- tryCatch(
               httr::HEAD(candidate_url, httr::timeout(10)),
               error = function(e) NULL
            )
            head_status <- if (!is.null(head_resp)) {
               httr::status_code(head_resp)
            } else {
               NA_integer_
            }
            list(
               suf = suf,
               url = as.character(candidate_url),
               status = head_status
            )
         }
      ) |>
         purrr::detect(~ identical(.x$status, 200L))

      if (!is.null(candidate)) {
         url <- candidate$url
         dest <- file.path(
            out_dir,
            glue::glue("{ano}{mes2}_{instituicao}{candidate$suf}")
         )
      }

      res <- tryCatch(
         {
            resp <- httr::GET(
               url,
               httr::write_disk(dest, overwrite = overwrite),
               httr::progress()
            )
            httr::stop_for_status(resp)
            list(
               success = TRUE,
               status = httr::status_code(resp),
               error = NA_character_
            )
         },
         error = function(e) {
            if (file.exists(dest) && !overwrite) {
               # Existent file note overwrite is a local success
               list(
                  success = TRUE,
                  status = NA_integer_,
                  error = "arquivo existente, nao sobrescrito"
               )
            } else {
               list(
                  success = FALSE,
                  status = NA_integer_,
                  error = conditionMessage(e)
               )
            }
         }
      )

      tibble::tibble(
         ano = ano,
         mes = as.integer(mes),
         instituicao = instituicao,
         url = as.character(url),
         dest = dest,
         success = res$success,
         http_status = res$status,
         error = res$error
      )
   }

   download_results <- purrr::pmap_dfr(
      combos,
      download_single_balancete,
      out_dir = out_dir,
      overwrite = overwrite
   )

   # Extract ZIPs and ignore extraction errors
   zips <- list.files(
      path = out_dir,
      pattern = "\\.zip$",
      ignore.case = TRUE,
      full.names = TRUE
   )

   purrr::walk(
      zips,
      ~ {
         extracted <- tryCatch(
            {
               utils::unzip(.x, exdir = out_dir)
               TRUE
            },
            error = function(e) FALSE
         )
         if (isTRUE(extracted)) {
            tryCatch(fs::file_delete(.x), error = function(e) NULL)
         }
      }
   )

   download_results
}

if (getRversion() >= "2.15.1") {
   utils::globalVariables(c("mes"))
}
