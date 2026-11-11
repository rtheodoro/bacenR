#' Download ESTBAN (Banking Statistics by Municipality) Files from Bacen
#' @description
#'  `r lifecycle::badge("experimental")`
#' Downloads the monthly [ESTBAN](https://www.bcb.gov.br/estatisticas/estatisticabancariamunicipios)
#' files (Estatística Bancária Mensal por Município) from the Brazilian Central
#' Bank (Bacen) website. Each file reports, for every municipality and
#' institution, the number of branches (`agen_processadas`) and the balances of
#' the COSIF accounts (`verbete_*`) of commercial banks, multiple banks with a
#' commercial portfolio and Caixa Econômica Federal.
#' @param years Numeric vector. Year(s) to download, e.g. `c(2020:2023)`.
#' @param months Numeric vector. Month(s) to download, values between 1 and 12.
#' @param out_dir Character. Directory where the files are saved. Created if it
#'   does not exist.
#' @param cleanup_zip Logical. If TRUE, removes ZIP files after extraction.
#'   Default is TRUE.
#' @param verbose Logical. If TRUE, displays progress messages. Default is TRUE.
#' @return Invisibly, a character vector with the paths of the extracted CSV
#'   files.
#' @details
#' The file name changed over the years (`YYYYMM_ESTBAN.csv.zip`,
#' `YYYYMM_ESTBAN.ZIP`, `YYYYMM_ESTBAN.csv`); each one is tried in turn, and
#' error pages served by the website with status 200 are discarded. Files are
#' saved as `YYYYMM_ESTBAN.csv` in `out_dir`. Use [tidy_estban()] to read them.
#' @examples
#' \donttest{
#' get_estban(years = 2023, months = 12, out_dir = tempdir())
#' }
#' @seealso [tidy_estban()]
#' @export
get_estban <- function(
  years,
  months,
  out_dir,
  cleanup_zip = TRUE,
  verbose = TRUE
) {
  check_years_months(years, months)
  check_out_dir(out_dir)

  grid <- expand.grid(year = as.integer(years), month = as.integer(months))
  datas <- sort(sprintf("%04d%02d", grid$year, grid$month))
  base <- "https://www.bcb.gov.br/content/estatisticas/estatistica_bancaria_estban/municipio/"

  get_one <- function(ym) {
    dest <- file.path(out_dir, paste0(ym, "_ESTBAN.csv"))
    tmp <- tempfile(fileext = ".download")
    on.exit(unlink(tmp), add = TRUE)
    for (suffix in c("_ESTBAN.csv.zip", "_ESTBAN.ZIP", "_ESTBAN.csv")) {
      ok <- download_bacen(paste0(base, ym, suffix), tmp)
      if (ok && is_zip(tmp)) {
        exdir <- tempfile()
        on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
        utils::unzip(tmp, exdir = exdir)
        csv <- list.files(exdir, recursive = TRUE, full.names = TRUE)[1]
        if (!is.na(csv) && is_estban_csv(csv)) {
          file.copy(csv, dest, overwrite = TRUE)
          if (!cleanup_zip) {
            file.copy(tmp, file.path(out_dir, paste0(ym, "_ESTBAN.zip")), overwrite = TRUE)
          }
          return(dest)
        }
      } else if (ok && is_estban_csv(tmp)) {
        file.copy(tmp, dest, overwrite = TRUE)
        return(dest)
      }
    }
    NA_character_
  }

  paths <- purrr::map_chr(datas, function(ym) {
    if (verbose) message("ESTBAN ", ym)
    get_one(ym)
  })
  missing <- datas[is.na(paths)]
  if (verbose) {
    message(sum(!is.na(paths)), " of ", length(datas), " files downloaded.")
    if (length(missing) > 0) {
      message("Not available: ", paste(missing, collapse = ", "))
    }
  }
  invisible(paths[!is.na(paths)])
}

#' Read and Tidy ESTBAN Files
#' @description
#'  `r lifecycle::badge("experimental")`
#' Reads the ESTBAN files downloaded with [get_estban()] and combines them into
#' a single table, one row per year-month, municipality and institution.
#' @param path_dir Character. Directory with the `YYYYMM_ESTBAN.csv` files
#'   (searched recursively).
#' @param out_dir Character or NULL. If given, the combined table is also
#'   written to `out_dir/ESTBAN.csv`. Default is NULL.
#' @param verbose Logical. If TRUE, displays progress messages. Default is TRUE.
#' @return A tibble with the columns of the files in snake case: `data_base`,
#'   `uf`, `codmun_ibge` (when available in the file), `municipio`, `cnpj`,
#'   `nome_instituicao`, `agencias` (branches processed), `agen_esperadas` and
#'   the `verbete_*` balances as numeric, plus `year_month` (`YYYYMM`).
#' @details
#' The header line starts with `#DATA_BASE` and is located automatically;
#' values use a comma as decimal mark and the files are Latin-1 encoded. The
#' column with the IBGE municipality code (`codmun_ibge`) exists only in the
#' more recent files; older files identify the municipality by name and state.
#' @examples
#' \donttest{
#' dir <- file.path(tempdir(), "estban")
#' get_estban(years = 2023, months = 12, out_dir = dir)
#' estban <- tidy_estban(dir)
#' }
#' @seealso [get_estban()]
#' @export
tidy_estban <- function(path_dir, out_dir = NULL, verbose = TRUE) {
  files <- list.files(
    path_dir,
    pattern = "^[0-9]{6}_ESTBAN\\.csv$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  if (length(files) == 0) {
    if (verbose) message("No ESTBAN files found in ", path_dir)
    return(tibble::tibble())
  }

  read_one <- function(file) {
    if (verbose) message("  - ", basename(file))
    df <- data.table::fread(
      file,
      skip = "DATA_BASE",
      sep = ";",
      dec = ",",
      encoding = "Latin-1",
      colClasses = "character"
    )
    names(df) <- tolower(sub("^#", "", names(df)))
    df <- tibble::as_tibble(janitor::clean_names(df))
    if ("agen_processadas" %in% names(df)) {
      names(df)[names(df) == "agen_processadas"] <- "agencias"
    }
    num_cols <- grep("^verbete_|^agencias$|^agen_esperadas$", names(df), value = TRUE)
    df[num_cols] <- lapply(df[num_cols], function(x) {
      readr::parse_number(x, locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
    })
    df$year_month <- substr(basename(file), 1, 6)
    df
  }

  estban <- purrr::map(files, read_one) |> purrr::list_rbind()

  if (!is.null(out_dir)) {
    check_out_dir(out_dir)
    out_file <- file.path(out_dir, "ESTBAN.csv")
    utils::write.csv(estban, out_file, row.names = FALSE)
    if (verbose) message("Wrote ", out_file)
  }
  estban
}
