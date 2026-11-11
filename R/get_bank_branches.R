#' Download the List of Bank Branches from Bacen
#' @description
#'  `r lifecycle::badge("experimental")`
#' Downloads the monthly list of branches (agências) of institutions supervised
#' by the Brazilian Central Bank (Bacen), available from September 2007 on the
#' [Bacen website](https://www.bcb.gov.br/fis/info/agencias.asp). Each file
#' lists the branches in operation, with the full CNPJ (root, sequential number
#' and check digits), address, municipality and opening date, as declared by
#' the institutions to Bacen.
#' @param years Numeric vector. Year(s) to download, e.g. `c(2020:2023)`.
#' @param months Numeric vector. Month(s) to download, values between 1 and 12.
#' @param out_dir Character. Directory where the files are saved. Created if it
#'   does not exist.
#' @param cleanup_zip Logical. If TRUE, removes ZIP files after extraction.
#'   Default is TRUE.
#' @param verbose Logical. If TRUE, displays progress messages. Default is TRUE.
#' @return Invisibly, a tibble with the documents found for the requested
#'   months (`year_month`, `title`, `url`) and the folder where each one was
#'   extracted (`path`, NA when the download failed).
#' @details
#' The list of available files is obtained from the document API of the Bacen
#' website (folder `/agencias`). A month may have more than one document, and
#' all of them are downloaded. Each document is extracted to
#' `out_dir/YYYYMM_AGENCIAS_<n>/`. Use [tidy_bank_branches()] to read them.
#' @examples
#' \donttest{
#' get_bank_branches(years = 2023, months = 12, out_dir = tempdir())
#' }
#' @seealso [tidy_bank_branches()]
#' @export
get_bank_branches <- function(
  years,
  months,
  out_dir,
  cleanup_zip = TRUE,
  verbose = TRUE
) {
  check_years_months(years, months)
  check_out_dir(out_dir)

  docs <- list_bacen_documents("/agencias")
  if (nrow(docs) == 0) {
    stop("Could not retrieve the list of branch files from Bacen.", call. = FALSE)
  }
  grid <- expand.grid(year = as.integer(years), month = as.integer(months))
  wanted <- sprintf("%04d%02d", grid$year, grid$month)
  docs <- docs[docs$year_month %in% wanted, , drop = FALSE]
  docs <- docs[order(docs$year_month), , drop = FALSE]
  docs$n <- stats::ave(seq_len(nrow(docs)), docs$year_month, FUN = seq_along)

  docs$path <- purrr::pmap_chr(
    list(docs$url, docs$year_month, docs$n),
    function(url, ym, n) {
      if (verbose) message("Branches ", ym, " (", n, ")")
      zip_path <- file.path(out_dir, paste0(ym, "_AGENCIAS_", n, ".zip"))
      if (!download_bacen(url, zip_path) || !is_zip(zip_path)) {
        unlink(zip_path)
        return(NA_character_)
      }
      exdir <- file.path(out_dir, paste0(ym, "_AGENCIAS_", n))
      ok <- tryCatch(
        {
          utils::unzip(zip_path, exdir = exdir)
          TRUE
        },
        error = function(e) FALSE
      )
      if (cleanup_zip) unlink(zip_path)
      if (ok) exdir else NA_character_
    }
  )

  missing <- setdiff(wanted, docs$year_month[!is.na(docs$path)])
  if (verbose) {
    message(sum(!is.na(docs$path)), " documents downloaded.")
    if (length(missing) > 0) {
      message("Not available: ", paste(sort(missing), collapse = ", "))
    }
  }
  invisible(tibble::as_tibble(docs[c("year_month", "title", "url", "path")]))
}

#' Read and Tidy the Lists of Bank Branches
#' @description
#'  `r lifecycle::badge("experimental")`
#' Reads the branch files downloaded with [get_bank_branches()] and combines
#' them into a single table, one row per year-month and branch.
#' @param path_dir Character. Directory with the folders created by
#'   [get_bank_branches()] (`YYYYMM_AGENCIAS_<n>/`).
#' @param out_dir Character or NULL. If given, the combined table is also
#'   written to `out_dir/AGENCIAS.csv`. Default is NULL.
#' @param verbose Logical. If TRUE, displays progress messages. Default is TRUE.
#' @return A tibble with `year_month`, `cnpj` (14 digits), `cnpj_basico`,
#'   `cnpj_ordem`, `cnpj_dv`, `nome_instituicao`, `segmento`, `compe_banco`,
#'   `compe_agencia`, `nome_agencia`, `endereco`, `numero`, `complemento`,
#'   `bairro`, `cep`, `municipio`, `uf`, `codmun_ibge` (in recent files),
#'   `data_inicio` (Date) and `id_instalacao` (in recent files).
#' @details
#' Every spreadsheet and every sheet in each folder is read, and the header row
#' is the first one containing `CNPJ`. Column names changed over the years and
#' are mapped to the names above. Branches abroad, which have no sequential
#' number in the CNPJ, are dropped with a message. Rows repeated within a
#' year-month (same 14-digit CNPJ) are kept once.
#' @examples
#' \donttest{
#' dir <- file.path(tempdir(), "branches")
#' get_bank_branches(years = 2023, months = 12, out_dir = dir)
#' branches <- tidy_bank_branches(dir)
#' }
#' @seealso [get_bank_branches()]
#' @export
tidy_bank_branches <- function(path_dir, out_dir = NULL, verbose = TRUE) {
  folders <- list.dirs(path_dir, recursive = FALSE)
  folders <- folders[grepl("^[0-9]{6}_AGENCIAS_[0-9]+$", basename(folders))]
  if (length(folders) == 0) {
    if (verbose) message("No branch folders found in ", path_dir)
    return(tibble::tibble())
  }

  read_folder <- function(folder) {
    files <- list.files(folder, pattern = "\\.xlsx?$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
    out <- purrr::map(files, function(file) {
      sheets <- tryCatch(readxl::excel_sheets(file), error = function(e) character(0))
      purrr::map(sheets, function(sheet) read_branch_sheet(file, sheet)) |>
        purrr::list_rbind()
    }) |>
      purrr::list_rbind()
    if (nrow(out) > 0) out$year_month <- substr(basename(folder), 1, 6)
    if (verbose) message("  - ", basename(folder), ": ", nrow(out), " rows")
    out
  }

  branches <- purrr::map(folders, read_folder) |> purrr::list_rbind()
  if (nrow(branches) == 0) {
    return(tibble::tibble())
  }

  branches$cnpj_basico <- stringr::str_pad(branches$cnpj_basico, 8, pad = "0")
  abroad <- is.na(branches$cnpj_ordem) | !grepl("^[0-9]+$", branches$cnpj_ordem)
  if (any(abroad) && verbose) {
    message(sum(abroad), " rows without the CNPJ sequential number (branches abroad) dropped.")
  }
  branches <- branches[!abroad, , drop = FALSE]
  branches$cnpj_ordem <- stringr::str_pad(branches$cnpj_ordem, 4, pad = "0")
  branches$cnpj_dv <- stringr::str_pad(branches$cnpj_dv, 2, pad = "0")
  branches$cnpj <- paste0(branches$cnpj_basico, branches$cnpj_ordem, branches$cnpj_dv)
  if ("cep" %in% names(branches)) branches$cep <- gsub("[^0-9]", "", branches$cep)
  if ("data_inicio" %in% names(branches)) branches$data_inicio <- as_bacen_date(branches$data_inicio)

  first <- c("year_month", "cnpj", "cnpj_basico", "cnpj_ordem", "cnpj_dv")
  branches <- branches[c(first, setdiff(names(branches), first))]
  branches <- branches[!duplicated(branches[c("year_month", "cnpj")]), , drop = FALSE]
  branches <- tibble::as_tibble(branches)

  if (!is.null(out_dir)) {
    check_out_dir(out_dir)
    out_file <- file.path(out_dir, "AGENCIAS.csv")
    utils::write.csv(branches, out_file, row.names = FALSE)
    if (verbose) message("Wrote ", out_file)
  }
  branches
}

# Column names after normalization (lower case, no accents, single spaces)
branch_col_map <- c(
  "cnpj" = "cnpj_basico",
  "sequencial do cnpj" = "cnpj_ordem",
  "dv do cnpj" = "cnpj_dv",
  "nome instituicao" = "nome_instituicao",
  "nome da instituicao" = "nome_instituicao",
  "segmento" = "segmento",
  "segmentos" = "segmento",
  "cod compe bco" = "compe_banco",
  "cod compe ag" = "compe_agencia",
  "nome da agencia" = "nome_agencia",
  "nome agencia" = "nome_agencia",
  "endereco" = "endereco",
  "numero" = "numero",
  "complemento" = "complemento",
  "bairro" = "bairro",
  "cep" = "cep",
  "municipio" = "municipio",
  "estado" = "uf",
  "uf" = "uf",
  "municipio ibge" = "codmun_ibge",
  "data inicio" = "data_inicio",
  "id instalacao" = "id_instalacao"
)

normalize_header <- function(x) {
  x <- iconv(as.character(x), from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(x)
}

read_branch_sheet <- function(file, sheet) {
  raw <- tryCatch(
    readxl::read_excel(file, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal"),
    error = function(e) NULL
  )
  if (is.null(raw) || nrow(raw) == 0) {
    return(NULL)
  }
  header_row <- which(apply(raw, 1, function(row) any(normalize_header(row) == "cnpj", na.rm = TRUE)))[1]
  if (is.na(header_row)) {
    return(NULL)
  }
  header <- normalize_header(unlist(raw[header_row, ]))
  df <- raw[-seq_len(header_row), , drop = FALSE]
  new_names <- unname(branch_col_map[header])
  keep <- !is.na(new_names) & !duplicated(new_names)
  df <- df[, keep, drop = FALSE]
  names(df) <- new_names[keep]
  df <- df[!is.na(df$cnpj_basico) & grepl("^[0-9]+$", trimws(df$cnpj_basico)), , drop = FALSE]
  df[] <- lapply(df, function(x) trimws(as.character(x)))
  df
}
