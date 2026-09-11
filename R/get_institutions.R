#' Download and Process Brazilian Financial Institutions Data from Bacen
#'
#' @description
#'  `r lifecycle::badge("stable")`
#'
#' This function downloads [financial institutions data from the Brazilian Central Bank (Bacen)](https://www.bcb.gov.br/estabilidadefinanceira/relacao_instituicoes_funcionamento)
#' website for specified institution types and date ranges. The data is downloaded as ZIP files,
#' extracted, and can be optionally cleaned up.
#'
#' @param institution Character vector. Type(s) of financial institutions to download.
#'   Valid options are:
#' \itemize{
#' \item "CONGLOMERADOS"
#' \item "BANCOS"
#' \item "COOPERATIVAS"
#' \item "ADMCONSORCIO"
#' \item "SOCIEDADES"
#' }
#' Default is "COOPERATIVAS". Case-insensitive. Check the details on [Bacen's website](https://www.bcb.gov.br/estabilidadefinanceira/relacao_instituicoes_funcionamento).
#' @param years Numeric vector. Year(s) to download, e.g. `c(2020:2023)` or `c(2020, 2022)`.
#' @param months Numeric vector. Month(s) to download, values between 1 and 12,
#'   e.g. `c(1:12)` or `c(6, 12)`.
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
#'   \item Generates every combination of `years` x `months` as year-month strings
#'   \item Downloads ZIP files for each institution and month from BCB website
#'   \item Extracts the downloaded ZIP files to the output directory
#'   \item Optionally removes ZIP files after extraction
#'   \item Displays progress information if verbose = TRUE
#' }
#'
#' For `CONGLOMERADOS`, the file suffix on BCB's server changed from the plural
#' "CONGLOMERADOS" to the singular "CONGLOMERADO" starting in months after 2022
#' (e.g. `.../202208CONGLOMERADOS.zip` vs. `.../202607CONGLOMERADO.zip`). This is
#' handled internally and is transparent to the caller.
#'
#' Institution type mappings:
#' \itemize{
#'   \item CONGLOMERADOS: Conglomerados
#'   \item BANCOS: Bancos comerciais, múltiplos e caixa
#'   \item COOPERATIVAS: Cooperativas de crédito
#'   \item ADMCONSORCIO: Consórcios administrativos
#'   \item SOCIEDADES: Bancos de investimentos, desenvolvimento e sociedades corretoras
#' }
#'
#' @examples
#' # Download cooperative credit unions data for 2023
#' get_institutions(
#'   institution = "COOPERATIVAS",
#'   years = 2023,
#'   months = 11:12,
#'   out_dir = tempdir()
#' )
#'\donttest{
#' # Download multiple institution types
#' get_institutions(
#'   institution = c("BANCOS", "COOPERATIVAS"),
#'   years = 2022,
#'   months = 1:12,
#'   out_dir = tempdir()
#' )
#'
#' # Skip downloading, just use existing files
#' get_institutions(
#'   institution = "BANCOS",
#'   years = 2022,
#'   months = 1:12,
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
  years,
  months,
  out_dir,
  cleanup_zip = TRUE,
  verbose = TRUE
) {
  type_institution <- c(
    CONGLOMERADOS = "Conglomerados",
    BANCOS = "Bancos_comerciais-multiplos-caixa",
    COOPERATIVAS = "Cooperativas-de-credito",
    ADMCONSORCIO = "Consorcios-adm",
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

  if (!is.numeric(years) || anyNA(years)) {
    stop("years must be a numeric vector, e.g. c(2020:2023).")
  }
  if (!is.numeric(months) || anyNA(months) || any(months < 1 | months > 12)) {
    stop(
      "months must be a numeric vector with values between 1 and 12, e.g. c(1:12)."
    )
  }

  grid <- expand.grid(year = as.integer(years), month = as.integer(months))
  datas <- sprintf("%04d%02d", grid$year, grid$month) |> sort()

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # helper: resolve the file-name label for an institution/year-month,
  # accounting for the CONGLOMERADOS -> CONGLOMERADO rename after 2022
  resolve_inst_label <- function(inst, year_month) {
    ano <- as.integer(substr(year_month, 1, 4))
    if (inst == "CONGLOMERADOS" && !is.na(ano) && ano > 2022) {
      "CONGLOMERADO"
    } else {
      inst
    }
  }

  # helper: download & unzip one month for one institution directly into out_dir
  get_files_for_inst <- function(inst, year_month) {
    prefix <- type_institution[[inst]]
    if (is.null(prefix)) {
      return(FALSE)
    }

    inst_label <- resolve_inst_label(inst, year_month)

    url <- paste0(
      "https://www.bcb.gov.br/content/estabilidadefinanceira/relacao_instituicoes_funcionamento/",
      prefix,
      "/",
      year_month,
      inst_label,
      ".zip"
    )

    zip_path <- file.path(out_dir, paste0(inst_label, "_", year_month, ".zip"))

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
