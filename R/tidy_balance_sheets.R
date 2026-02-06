#' Process and reshape balances CSV exports from the Brazilian Central Bank (Bacen) downloaded via `get_balance_sheets()`.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `tidy_balance_sheets` reads raw CSV export files produced by the BCB and downloaded via `get_balance_sheets()`, normalizes filenames, locates the
#' header row, imports rows matching a specific document number, cleans column names, pivots account balances so
#' that each account becomes a column and each row corresponds to an
#' institution/date, and optionally writes per-institution-type CSVs.
#' @param path_raw Character scalar. Directory containing the raw CSV files.
#'   Filenames are expected to start with a YYYYMM prefix (e.g. "202012COOPERATIVAS.CSV").
#'   The function will look for files matching "\\.CSV$" (case-insensitive).
#' @param out_dir Character scalar. Directory where output CSVs will be saved
#'   when \code{save = TRUE}. If it does not exist it will be created.
#' @param doc_filter Numeric or integer scalar. The numeric value used to
#'   filter rows in the imported tables by the column DOCUMENTO (e.g. 4010).
#'   Other options are: 4010, 4413, 4423, 4433, 4060, 4016 and 4066.
#'   Check the webpage for more details: [BCB balance page](https://www.bcb.gov.br/estabilidadefinanceira/balancetesbalancospatrimoniais)
#' @param save Logical scalar. If \code{TRUE} (default) the processed data
#'   frames are also written to disk as CSV files (one per institution type). If
#'  \code{FALSE} the function only returns the processed data.frames.
#'
#' @return A named list of data.frames (one element per unique institution
#'   type detected in the filenames). Each data.frame has one row per
#'   unique combination of (data, documento, cnpj, nome_instituicao)
#'   and one column per account (the "conta" values) with balances
#'   populated from the "saldo" column.
#'
#' @details Behavior and implementation notes:
#' - Filenames are first normalized by removing all underscores. If a
#'   target filename after normalization already exists, the function
#'   aborts to avoid overwriting files.
#' - The function extracts year/month and an institution type token from
#'   the file basename by assuming the first six characters are YYYYMM
#'   and the remaining characters (without the ".CSV") represent the type.
#' - For each file the function reads the file as text and attempts to
#'   locate the header row by scanning lines until it finds a row that
#'   contains (case-insensitive) tokens matching:
#'     - "DOCUMENTO" (or "DOCUMENTOS"),
#'     - "CNPJ",
#'     - and "DATA BASE" (variants matched by regex such as "DATA_BASE" or "#DATA_BASE").
#'   The delimiter for that header line is heuristically detected among
#'   semicolon (";"), comma (","), or whitespace. For whitespace-delimited
#'   data the function uses read.table's default behavior.
#' - After locating the header, the table is read with \code{utils::read.table}
#'   using \code{header = TRUE}, the detected separator, latin1 encoding,
#'   and \code{check.names = FALSE}. Rows where the DOCUMENTO column equals
#'   \code{doc_filter} are kept.
#' - Column names are cleaned via \code{janitor::clean_names()} before
#'   the wide pivot. The function expects at least the following cleaned
#'   column names to be present: \code{conta}, \code{saldo}, and
#'   \code{data}. The pivot uses the id columns
#'   (\code{data}, \code{documento}, \code{cnpj},
#'   \code{nome_instituicao}), \code{conta} as names and \code{saldo} as values.
#' - If \code{save = TRUE} each resulting data.frame is written to CSV
#'   using \code{readr::write_csv()} to the path returned by
#'   \code{output_filename(type_inst)} (wrapped with \code{out_dir}).
#' - The function will stop with informative errors in several situations:
#'   missing CSVs in \code{path_raw}, failure to find a header line with
#'   the required tokens, missing required columns after import/cleaning,
#'   or failure to rename files when removing underscores.
#'
#' @note The function prints a message reminding that the numeric values
#'   in the balance sheets are not adjusted for inflation. See:
#'   [https://www.bcb.gov.br/estabilidadefinanceira/balancetesbalancospatrimoniais](https://www.bcb.gov.br/estabilidadefinanceira/balancetesbalancospatrimoniais)
#'
#' @examples
#'
#' # First, download balance sheets
#' get_balance_sheets(
#'   institution = "BANCOS",
#'   months = 12,
#'   first_year = 2023,
#'   final_year = 2023,
#'   out_dir = tempdir(),
#'   overwrite = FALSE
#' )
#'
#' # Now, tidy the files
#' # Do not write outputs
#' tidy_balance_sheets(
#'   path_raw = tempdir(),
#'   out_dir = tempdir(),
#'   doc_filter = 4010,
#'   save = FALSE
#' )
#'\donttest{
#' # Write outputs
#' tidy_balance_sheets(
#'   path_raw = tempdir(),
#'   out_dir = tempdir(),
#'   doc_filter = 4016,
#'   save = TRUE
#' )
#'}
#'
#' @export

tidy_balance_sheets <- function(
  path_raw,
  out_dir,
  doc_filter = 4010,
  save = TRUE
) {
  files <- list.files(
    path_raw,
    pattern = "\\.CSV$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(files) == 0L) {
    stop(glue::glue(
      "No CSV files found in '{path_raw}'. Expected filenames with 'YYYYMM' prefix."
    ))
  }

  # Check filename normalization
  normalized_files <- file.path(
    path_raw,
    gsub("_", "", basename(files))
  )
  if (any(duplicated(normalized_files))) {
    dup_files <- normalized_files[duplicated(normalized_files)]
    stop(glue::glue(
      "Filename normalization would overwrite existing files: {paste(dup_files, collapse = ', ')}"
    ))

    # Rename files to normalized names
  } else if (any(basename(files) != basename(normalized_files))) {
    rename_result <- file.rename(files, normalized_files)
    if (!all(rename_result)) {
      failed_files <- files[!rename_result]
      stop(glue::glue(
        "Failed to rename the following files: {paste(failed_files, collapse = ', ')}"
      ))
    }
    files <- normalized_files
  }

  # Separate files by institution type
  inst_types <- unique(substring(
    basename(files),
    7,
    nchar(basename(files)) - 4
  ))
  result_list <- list()

  # Process each institution type using purrr::map
  result_list <- purrr::map(inst_types, function(type_inst) {
    type_files <- files[grepl(
      paste0("^\\d{6}", type_inst, "\\.CSV$"),
      basename(files),
      ignore.case = TRUE
    )]
    if (length(type_files) == 0L) {
      message(glue::glue(
        "No files found for institution type '{type_inst}'. Skipping."
      ))
      return(NULL)
    }
    # Placeholder for per-type processing; replace with actual import/reshape logic.
    list(files = type_files)
  })

  # For each institution type, import .csv file using data.table and detect header by the row containing "DOCUMENTO", "CNPJ", and "CONTA"
  # Importar cada tipo separadamente
  balance_by_type <- purrr::map(
    result_list,
    ~ import_balance_sheets(.$files)
  )

  # Rename lists
  names(balance_by_type) <- inst_types

  # Filter by doc_filter, for each data.frame in the list

  balance_by_type <- purrr::imap(
    balance_by_type,
    function(df, type_inst) {
      if (is.null(df)) {
        return(NULL)
      }
      df_filtered <- df[df$DOCUMENTO == doc_filter, ]
      if (nrow(df_filtered) == 0L) {
        message(glue::glue(
          "No rows found for Documento = {doc_filter} in institution type '{type_inst}'."
        ))
      }
      message(glue::glue(
        "Documento = {doc_filter} in institution type '{type_inst}' selected."
      ))
      df_filtered
    }
  )

  # Cleaning column names
  balance_by_type <- purrr::map(
    balance_by_type,
    function(df) {
      if (is.null(df)) {
        return(NULL)
      }
      janitor::clean_names(df)
    }
  )

  # Transform to wide format
  balance_by_type <- purrr::map(
    balance_by_type,
    function(df) {
      if (is.null(df) || nrow(df) == 0L) {
        return(NULL)
      }
      required_cols <- c(
        "data",
        "documento",
        "cnpj",
        "nome_instituicao",
        "conta",
        "saldo"
      )
      missing_cols <- setdiff(required_cols, names(df))
      if (length(missing_cols) > 0L) {
        stop(glue::glue(
          "Missing required columns for pivot: {paste(missing_cols, collapse = ', ')}"
        ))
      }
      df_wide <- tidyr::pivot_wider(
        df,
        id_cols = c("data", "documento", "cnpj", "nome_instituicao"),
        names_from = "conta",
        values_from = "saldo"
      )
      df_wide
    }
  )

  # Write a CSV per institution type if save = TRUE
  if (save) {
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
    }

    output_filename <- function(type) {
      file.path(
        out_dir,
        paste0(doc_filter, "_", tolower(type), ".csv")
      )
    }

    purrr::iwalk(balance_by_type, function(df, type_inst) {
      out_path <- output_filename(type_inst)
      data.table::fwrite(df, out_path)
      message(glue::glue("Written: {out_path}"))
    })
  }

  message(glue::glue(
    "Note: \n
    The values in the balancetes are not adjusted for inflation nor modified. \n
    This function is still in development, check your data carefully. \n     
    Check the the page https://www.bcb.gov.br/estabilidadefinanceira/balancetesbalancospatrimoniais "
  ))

  balance_by_type
}
