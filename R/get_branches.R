#' Download the Lists of Branches and Service Posts from Bacen
#' @description
#'  `r lifecycle::badge("experimental")`
#' Downloads the monthly lists of dependencies of institutions authorized by the
#' Brazilian Central Bank (Bacen), from the
#' [Bacen website](https://www.bcb.gov.br/estabilidadefinanceira/relacao_instituicoes_funcionamento):
#' branches (agências), service posts (postos de atendimento), electronic
#' service posts (PAE) and branches of consortium administrators. Each file
#' lists the dependencies in operation, with the full CNPJ (root, sequential
#' number and check digits), address, municipality and opening date.
#' @param type Character vector. Type(s) of dependency to download. Valid
#'   options are:
#' \itemize{
#' \item "AGENCIAS": branches (since September 2007)
#' \item "POSTOS": service posts (since September 2007)
#' \item "PAE": electronic service posts (since July 2016)
#' \item "FILIAISCONS": branches of consortium administrators (since July 2016)
#' }
#' Default is "AGENCIAS". Case-insensitive.
#' @param years Numeric vector. Year(s) to download, e.g. `c(2020:2023)`.
#' @param months Numeric vector. Month(s) to download, values between 1 and 12.
#' @param out_dir Character. Directory where the files are saved. Created if it
#'   does not exist.
#' @param cleanup_zip Logical. If TRUE, removes ZIP files after extraction.
#'   Default is TRUE.
#' @param verbose Logical. If TRUE, displays progress messages. Default is TRUE.
#' @return Invisibly, a tibble with one row per type and year-month requested
#'   (`type`, `year_month`, `url`) and the folder where the file was extracted
#'   (`path`, NA when the file is not available).
#' @details
#' Files are downloaded from
#' `https://www.bcb.gov.br/fis/info/cad/<folder>/<YYYYMM><TYPE>.zip` and
#' extracted to `out_dir/<YYYYMM>_<TYPE>/`. Use [tidy_branches()] to read them.
#'
#' Notes from Bacen: from July 2016, the list of branches includes segments
#' other than banks, and the list of service posts includes cooperative service
#' posts (PAC), gold purchase posts (PCO) and temporary exchange posts (PCT)
#' and no longer includes electronic posts (PAE), which have their own list.
#'
#' From 2010 onwards, the list of branches does not include Banco do Brasil
#' and Caixa Econômica Federal; their branch counts by municipality are
#' available in ESTBAN ([get_estban()]).
#' @examples
#' \donttest{
#' get_branches(type = "AGENCIAS", years = 2023, months = 12, out_dir = tempdir())
#' }
#' @seealso [tidy_branches()], [get_estban()]
#' @export
get_branches <- function(
  type = "AGENCIAS",
  years,
  months,
  out_dir,
  cleanup_zip = TRUE,
  verbose = TRUE
) {
  types <- unique(toupper(as.character(type)))
  invalid <- setdiff(types, names(branch_folders))
  if (length(invalid) > 0) {
    warning("Unknown types skipped: ", paste(invalid, collapse = ", "), call. = FALSE)
    types <- setdiff(types, invalid)
  }
  if (length(types) == 0) {
    stop("No valid type provided.", call. = FALSE)
  }
  check_years_months(years, months)
  check_out_dir(out_dir)

  grid <- expand.grid(
    type = types,
    year_month = sort(unique(sprintf("%04d%02d", rep(as.integer(years), each = length(months)), as.integer(months)))),
    stringsAsFactors = FALSE
  )
  grid$url <- sprintf(
    "https://www.bcb.gov.br/fis/info/cad/%s/%s%s.zip",
    branch_folders[grid$type], grid$year_month, grid$type
  )

  grid$path <- purrr::pmap_chr(grid[c("type", "year_month", "url")], function(type, year_month, url) {
    if (verbose) message(type, " ", year_month)
    zip_path <- file.path(out_dir, paste0(year_month, type, ".zip"))
    if (!download_bacen(url, zip_path) || !is_zip(zip_path)) {
      unlink(zip_path)
      return(NA_character_)
    }
    exdir <- file.path(out_dir, paste0(year_month, "_", type))
    ok <- tryCatch(
      {
        utils::unzip(zip_path, exdir = exdir)
        TRUE
      },
      error = function(e) FALSE
    )
    if (cleanup_zip) unlink(zip_path)
    if (ok) exdir else NA_character_
  })

  if (verbose) {
    message(sum(!is.na(grid$path)), " of ", nrow(grid), " files downloaded.")
    miss <- grid[is.na(grid$path), , drop = FALSE]
    if (nrow(miss) > 0) {
      message("Not available: ", paste(miss$type, miss$year_month, collapse = ", "))
    }
  }
  invisible(tibble::as_tibble(grid))
}

#' Read and Tidy the Lists of Branches and Service Posts
#' @description
#'  `r lifecycle::badge("experimental")`
#' Reads the files downloaded with [get_branches()] and combines them into a
#' single table, one row per type, year-month and dependency.
#' @param path_dir Character. Directory with the folders created by
#'   [get_branches()] (`<YYYYMM>_<TYPE>/`).
#' @param type Character vector or NULL. Type(s) to read (see
#'   [get_branches()]); NULL reads all. Default is NULL.
#' @param out_dir Character or NULL. If given, the table is also written to
#'   `out_dir/<TYPE>.csv`, one file per type. Default is NULL.
#' @param verbose Logical. If TRUE, displays progress messages. Default is TRUE.
#' @return A tibble with `type`, `year_month`, `cnpj` (14 digits),
#'   `cnpj_basico`, `cnpj_ordem`, `cnpj_dv` and the remaining columns of the
#'   files in snake case. Columns whose names changed over the years are
#'   harmonized: `nome_instituicao`, `segmento`, `compe_banco`,
#'   `compe_agencia`, `nome_agencia`, `endereco`, `numero`, `complemento`,
#'   `bairro`, `cep`, `municipio`, `uf`, `codmun_ibge`, `data_inicio` (Date)
#'   and `id_instalacao`.
#' @details
#' Every spreadsheet and every sheet in each folder is read, and the header row
#' is the first one containing `CNPJ`. Dependencies abroad, which have no
#' sequential number in the CNPJ, are dropped with a message. Rows repeated
#' within a type and year-month (same 14-digit CNPJ) are kept once.
#' @examples
#' \donttest{
#' dir <- file.path(tempdir(), "branches")
#' get_branches(type = c("AGENCIAS", "POSTOS"), years = 2023, months = 12, out_dir = dir)
#' branches <- tidy_branches(dir)
#' }
#' @seealso [get_branches()]
#' @export
tidy_branches <- function(path_dir, type = NULL, out_dir = NULL, verbose = TRUE) {
  folders <- list.dirs(path_dir, recursive = FALSE)
  folders <- folders[grepl("^[0-9]{6}_[A-Z]+$", basename(folders))]
  folder_type <- sub("^[0-9]{6}_", "", basename(folders))
  keep <- folder_type %in% names(branch_folders)
  if (!is.null(type)) keep <- keep & folder_type %in% toupper(type)
  folders <- folders[keep]
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
    if (verbose) message("  - ", basename(folder), ": ", nrow(out), " rows")
    if (nrow(out) == 0) {
      return(NULL)
    }
    out$type <- sub("^[0-9]{6}_", "", basename(folder))
    out$year_month <- substr(basename(folder), 1, 6)
    out
  }

  deps <- purrr::map(folders, read_folder) |> purrr::list_rbind()
  if (is.null(deps) || nrow(deps) == 0) {
    return(tibble::tibble())
  }

  deps$cnpj_basico <- stringr::str_pad(deps$cnpj_basico, 8, pad = "0")
  abroad <- is.na(deps$cnpj_ordem) | !grepl("^[0-9]+$", deps$cnpj_ordem)
  if (any(abroad) && verbose) {
    message(sum(abroad), " rows without the CNPJ sequential number (abroad) dropped.")
  }
  deps <- deps[!abroad, , drop = FALSE]
  deps$cnpj_ordem <- stringr::str_pad(deps$cnpj_ordem, 4, pad = "0")
  deps$cnpj_dv <- stringr::str_pad(deps$cnpj_dv, 2, pad = "0")
  deps$cnpj <- paste0(deps$cnpj_basico, deps$cnpj_ordem, deps$cnpj_dv)
  if ("cep" %in% names(deps)) deps$cep <- gsub("[^0-9]", "", deps$cep)
  if ("data_inicio" %in% names(deps)) deps$data_inicio <- as_bacen_date(deps$data_inicio)

  first <- c("type", "year_month", "cnpj", "cnpj_basico", "cnpj_ordem", "cnpj_dv")
  deps <- deps[c(first, setdiff(names(deps), first))]
  deps <- deps[!duplicated(deps[c("type", "year_month", "cnpj")]), , drop = FALSE]
  deps <- tibble::as_tibble(deps)

  if (!is.null(out_dir)) {
    check_out_dir(out_dir)
    for (t in unique(deps$type)) {
      out_file <- file.path(out_dir, paste0(t, ".csv"))
      utils::write.csv(deps[deps$type == t, , drop = FALSE], out_file, row.names = FALSE)
      if (verbose) message("Wrote ", out_file)
    }
  }
  deps
}

# Type -> folder on the Bacen website
branch_folders <- c(
  AGENCIAS = "agencias",
  POSTOS = "postos",
  PAE = "PAE",
  FILIAISCONS = "filiais_adm_consorcios"
)

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
  new_names <- ifelse(is.na(branch_col_map[header]), gsub(" ", "_", header), branch_col_map[header])
  keep <- new_names != "" & !is.na(new_names) & !duplicated(new_names)
  df <- raw[-seq_len(header_row), keep, drop = FALSE]
  names(df) <- unname(new_names[keep])
  df <- df[!is.na(df$cnpj_basico) & grepl("^[0-9]+$", trimws(df$cnpj_basico)), , drop = FALSE]
  df[] <- lapply(df, function(x) trimws(as.character(x)))
  df
}
