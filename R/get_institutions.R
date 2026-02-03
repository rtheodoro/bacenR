#' Download and Process Brazilian Financial Institutions Data from Bacen
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This function downloads [financial institutions data from the Brazilian Central Bank (Bacen)](https://www.bcb.gov.br/estabilidadefinanceira/relacao_instituicoes_funcionamento)
#' website for specified institution types and date ranges. The data is downloaded as ZIP files,
#' extracted, and can be optionally cleaned up.
#'
#' @param institution Character vector. Type(s) of financial institutions to download.
#'   Valid options are: "CONGLOMERADOS", "BANCOS", "COOPERATIVAS", "CONSORCIO", "SOCIEDADES".
#'   Default is "COOPERATIVAS". Case-insensitive. Check the details on [Bacen's website](https://www.bcb.gov.br/estabilidadefinanceira/relacao_instituicoes_funcionamento).
#' @param start_date Character. Start date in "YYYYMM" format (e.g., "200709") or
#'   a parsable date string (e.g., "2007-09-01"). Default is "200709".
#' @param end_date Character. End date in "YYYYMM" format (e.g., "202409") or
#'   a parsable date string (e.g., "2024-09-01"). Default is "202409".
#' @param out_dir Character. Directory path where downloaded files will be saved.
#'   Default is "data". The directory will be created if it doesn't exist.
#' @param cleanup_zip Logical. If TRUE, removes ZIP files after extraction.
#'   Default is TRUE.
#' @param verbose Logical. If TRUE, displays progress messages and warnings.
#'   Default is TRUE.
#'
#' @return Invisible NULL. The function is called for its side effects (downloading
#'   and extracting files to the specified directory).
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Validates institution types against known valid options
#'   \item Generates a sequence of months between start_date and end_date
#'   \item Downloads ZIP files for each institution and month from BCB website
#'   \item Extracts the downloaded ZIP files to the output directory
#'   \item Optionally removes ZIP files after extraction
#'   \item Displays progress information if verbose = TRUE
#' }
#'
#' Institution type mappings:
#' \itemize{
#'   \item CONGLOMERADOS: Conglomerados
#'   \item BANCOS: Bancos comerciais, múltiplos e caixa
#'   \item COOPERATIVAS: Cooperativas de crédito
#'   \item CONSORCIO: Consórcios administrativos
#'   \item SOCIEDADES: Bancos de investimentos, desenvolvimento e sociedades corretoras
#' }
#'
#' @examples
#' \donttest{
#' # Download cooperative credit unions data for 2023
#' get_institutions(
#'   institution = "COOPERATIVAS",
#'   start_date = "202301",
#'   end_date = "202312",
#'   out_dir = tempdir()
#' )
#'
#' # Download multiple institution types
#' get_institutions(
#'   institution = c("BANCOS", "COOPERATIVAS"),
#'   start_date = "202201",
#'   end_date = "202212",
#'   out_dir = tempdir()
#' )
#'
#' # Skip downloading, just use existing files
#' get_institutions(
#'   institution = "BANCOS",
#'   start_date = "202201",
#'   end_date = "202212",
#'   out_dir = tempdir(),
#'   verbose = FALSE
#' )
#'}
#' @seealso
#' \url{https://www.bcb.gov.br/estabilidadefinanceira/relacao_instituicoes_funcionamento}
#'
#' @export
get_institutions <- function(
  institution,
  start_date,
  end_date,
  out_dir,
  cleanup_zip = TRUE,
  verbose = TRUE
) {
  type_institution <- c(
    CONGLOMERADOS = "Conglomerados",
    BANCOS = "Bancos_comerciais-multiplos-caixa",
    COOPERATIVAS = "Cooperativas-de-credito",
    CONSORCIO = "Consorcios-adm",
    SOCIEDADES = "Bancos-investimentos-desenvolvimento-sociedade-corretoras"
  )

  insts <- toupper(as.character(institution))
  valid <- insts %in% names(type_institution)
  if (any(!valid)) {
    warning(
      "Unknown institution keys skipped: ",
      paste(unique(insts[!valid]), collapse = ", ")
    )
    insts <- unique(insts[valid])
  }
  if (length(insts) == 0) {
    stop("No valid institution keys provided.")
  }

  make_date_ym <- function(ym) {
    ym <- as.character(ym)
    if (grepl("^\\d{6}$", ym)) {
      y <- substr(ym, 1, 4)
      m <- substr(ym, 5, 6)
      as.Date(paste0(y, "-", m, "-01"))
    } else {
      as.Date(ym)
    }
  }

  start_dt <- make_date_ym(start_date)
  end_dt <- make_date_ym(end_date)
  if (is.na(start_dt) || is.na(end_dt)) {
    stop(
      "start_date or end_date could not be parsed as dates; expected 'YYYYMM' or a parsable date string (e.g. 'YYYY-MM-DD')."
    )
  }

  datas <- format(seq(start_dt, end_dt, by = "month"), "%Y%m")

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # helper: download & unzip one month for one institution directly into out_dir
  get_files_for_inst <- function(inst, year_month) {
    prefix <- type_institution[[inst]]
    if (is.null(prefix)) {
      return(FALSE)
    }

    url <- paste0(
      "https://www.bcb.gov.br/content/estabilidadefinanceira/relacao_instituicoes_funcionamento/",
      prefix,
      "/",
      year_month,
      inst,
      ".zip"
    )

    zip_path <- file.path(out_dir, paste0(inst, "_", year_month, ".zip"))

    resp <- tryCatch(
      {
        httr::GET(url)
      },
      error = function(e) {
        if (isTRUE(verbose)) {
          warning(
            sprintf(
              "Failed to GET URL for %s %s: %s",
              inst,
              year_month,
              conditionMessage(e)
            ),
            call. = FALSE
          )
        }
        NULL
      }
    )
    if (is.null(resp)) {
      return(FALSE)
    }
    if (httr::status_code(resp) == 200) {
      writeBin(httr::content(resp, "raw"), zip_path)
      unzip_ok <- tryCatch(
        {
          utils::unzip(zip_path, exdir = out_dir)
          TRUE
        },
        error = function(e) {
          if (isTRUE(verbose)) {
            warning(
              sprintf(
                "Failed to unzip archive for %s %s: %s",
                inst,
                year_month,
                conditionMessage(e)
              ),
              call. = FALSE
            )
          }
          FALSE
        }
      )
      if (!unzip_ok) {
        if (cleanup_zip && file.exists(zip_path)) {
          unlink(zip_path)
        }
        return(FALSE)
      }
      if (cleanup_zip) {
        unlink(zip_path)
      }
      TRUE
    } else {
      FALSE
    }
  }

  # main loop: process each institution
  results <- lapply(insts, function(inst) {
    total <- length(datas)

    if (verbose) {
      message(glue::glue("Downloading for {inst}: {total} months to try"))
    }
    pb <- NULL
    if (verbose) {
      pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
    }
    purrr::iwalk(datas, function(ym, i) {
      if (verbose) {
        message(glue::glue("[{i}/{total}] {inst} {ym}"))
      }
      try(get_files_for_inst(inst, ym), silent = TRUE)
      if (!is.null(pb)) {
        utils::setTxtProgressBar(pb, i)
      }
    })
    if (!is.null(pb)) {
      close(pb)
    }
    if (verbose) message(glue::glue("Downloads attempted for {inst}"))
  })
}
