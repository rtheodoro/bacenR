#' Process and reshape "balancetes" CSV exports from the Brazilian Central Bank (BCB)
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' `treatment_balance_sheets` reads raw CSV export files produced by the BCB,
#' normalizes filenames, locates the header row, imports rows matching a
#' specific document number, cleans column names, pivots account balances
#' so that each account becomes a column and each row corresponds to an
#' institution/date, and optionally writes per-institution-type CSVs.
#' @param path_raw Character scalar. Directory containing the raw CSV files.
#'   Filenames are expected to start with a YYYYMM prefix (e.g. "202012COOPERATIVAS.CSV").
#'   The function will look for files matching "\\.CSV$" (case-insensitive).
#' @param out_dir Character scalar. Directory where output CSVs will be saved
#'   when \code{save = TRUE}. If it does not exist it will be created.
#' @param doc_filter Numeric or integer scalar. The numeric value used to
#'   filter rows in the imported tables by the column DOCUMENTO (e.g. 4010).
#'   Other options are: 4010, 4413, 4423, 4433, 4060, 4016 and 4066.
#'   Check the webpage for more details: https://www.bcb.gov.br/estabilidadefinanceira/balancetesbalancospatrimoniais
#' @param save Logical scalar. If \code{TRUE} (default) the processed data
#'   frames are written to disk as CSV files (one per institution type).
#' @param output_filename NULL, character scalar, or function. Controls the
#'   output file path(s) used when \code{save = TRUE}:
#'   - NULL (default): a function is created that writes files to
#'     \code{file.path(out_dir, paste0(doc_filter, "_", tolower(type_inst), ".csv"))}.
#'   - a single string: that filename (inside \code{out_dir}) is used for all types.
#'   - a function: should accept a single argument \code{type_inst} and return a filename;
#'     the returned path will be wrapped with \code{out_dir}.
#'
#' @return A named list of data.frames (one element per unique institution
#'   type detected in the filenames). Each data.frame has one row per
#'   unique combination of (data_base, documento, cnpj, nome_instituicao)
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
#'   \code{number_data_base}. The pivot uses the id columns
#'   (\code{number_data_base}, \code{documento}, \code{cnpj},
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
#'   in the balancetes are not adjusted for inflation. See:
#'   https://www.bcb.gov.br/estabilidadefinanceira/balancetesbalancospatrimoniais
#'
#' @examples
#' \dontrun{
#' # Basic usage (do not write outputs)
#' treatment_balance_sheets(path_raw = "data_raw", out_dir = "data", doc_filter = 4010, save = FALSE)
#'
#' # Write outputs with a fixed filename for all types
#' treatment_balance_sheets(path_raw = "data_raw", out_dir = "data", doc_filter = 4010,
#'                       save = TRUE, output_filename = "bc_balance_sheets.csv")
#'
#' # Custom naming function
#' treatment_balance_sheets(path_raw = "data_raw", out_dir = "out",
#'                       doc_filter = 4010, save = TRUE,
#'                       output_filename = function(type) paste0("bal_", tolower(type), ".csv"))
#' }
#'
#' @export
treatment_balance_sheets <- function(
  path_raw = "data_raw",
  out_dir = "data",
  doc_filter = 4010,
  save = TRUE,
  output_filename = NULL
) {
  # locate CSVs with leading year+month in the filename (e.g. 202012COOPERATIVAS.CSV)
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

  # normalize filenames by removing underscores and update `files`
  new_paths <- vapply(
    files,
    function(fp) {
      dir <- dirname(fp)
      base_name <- basename(fp)
      new_bn <- gsub("_", "", base_name, fixed = TRUE)
      new_file_path <- file.path(dir, new_bn)

      if (identical(fp, new_file_path)) {
        return(fp)
      }

      if (file.exists(new_file_path)) {
        stop(glue::glue(
          "Target filename already exists: '{new_file_path}'. Aborting to avoid overwrite."
        ))
      }

      rename_succeeded <- file.rename(fp, new_file_path)
      if (!rename_succeeded) {
        stop(glue::glue("Failed to rename '{fp}' -> '{new_file_path}'."))
      }

      new_file_path
    },
    character(1)
  )

  files <- unname(new_paths)

  # Extract the informations: year, month and type of institution.
  info_list <- lapply(
    files,
    function(fp) {
      bn <- basename(fp)
      year <- as.numeric(substr(bn, 1, 4))
      month <- as.numeric(substr(bn, 5, 6))
      type_inst <- gsub(
        "\\d{6}|\\.CSV$",
        "",
        toupper(bn),
        ignore.case = TRUE
      )
      data.frame(
        file_path = fp,
        year = year,
        month = month,
        type_institution = type_inst,
        stringsAsFactors = FALSE
      )
    }
  )

  # Verify if there are more than one type of institution.
  info_df <- do.call(rbind, info_list)
  unique_types <- unique(info_df$type_institution)

  # for each typo of institution, process the files using purrr functions
  # import the csv files starting from the row where the document number is located.
  balancetes_by_type <- purrr::set_names(unique_types) |>
    purrr::map(function(inst_type) {
      inst_files <- info_df$file_path[info_df$type_institution == inst_type]

      balancetes_inst <- purrr::map_dfr(
        inst_files,
        function(fp) {
          # locate the header line containing "DOCUMENTO" or "DOCUMENTOS" (case-insensitive)
          lines <- readLines(fp, encoding = "latin1", warn = FALSE)
          header_idx <- NA_integer_
          sep_used <- ","

          for (i in seq_along(lines)) {
            ln <- lines[i]
            if (nchar(trimws(ln)) == 0L) {
              next
            }

            # detect likely delimiter for this header candidate
            delim <- if (grepl(";", ln)) {
              ";"
            } else if (grepl(",", ln)) {
              ","
            } else {
              "\\s+"
            }
            # when delim is whitespace use a regex split, otherwise split by the delimiter
            split_pattern <- if (identical(delim, "\\s+")) "\\s+" else delim
            tokens <- unlist(strsplit(ln, split_pattern, perl = TRUE))
            tokens <- trimws(gsub('^["\']|["\']$', "", tokens))

            if (length(tokens) == 0L) {
              next
            }

            has_doc <- any(grepl("DOCUMENTO", tokens, ignore.case = TRUE))
            has_cnpj <- any(grepl(
              "^CNPJ$|\\bCNPJ\\b",
              tokens,
              ignore.case = TRUE
            ))
            has_data <- any(grepl(
              "DATA[_ ]?BASE|#DATA_BASE",
              tokens,
              ignore.case = TRUE
            ))

            if (isTRUE(has_doc) && isTRUE(has_cnpj) && isTRUE(has_data)) {
              header_idx <- i
              # map detected delim to a value acceptable by read.table/read.csv:
              # use "" for whitespace (read.table handles any amount of whitespace)
              sep_used <- if (identical(delim, "\\s+")) "" else delim
              break
            }
          }

          if (is.na(header_idx)) {
            stop(glue::glue(
              "Header containing 'DOCUMENTO' not found in file: {fp}"
            ))
          }

          con <- textConnection(lines)
          on.exit(close(con), add = TRUE)

          # read the table explicitly specifying header and separator and avoid using any column as row.names
          df <- utils::read.table(
            con,
            sep = sep_used,
            header = TRUE,
            skip = header_idx - 1L,
            stringsAsFactors = FALSE,
            fileEncoding = "latin1",
            comment.char = "",
            check.names = FALSE,
            row.names = NULL,
            fill = TRUE,
            na.strings = c("", "NA")
          )
          df[df$DOCUMENTO == doc_filter, , drop = FALSE]
        }
      )

      balancetes_inst
    })

  balancetes_by_type

  # janitor::clean_names() to clean the column names
  balancetes_by_type <- purrr::map(
    balancetes_by_type,
    janitor::clean_names
  )

  # Transpose the data.frames so that accounts are rows and dates are columns.
  # The transpose will be by column 'conta' (account).
  # The final data.frame should have the following structure:
  # data_base, documento, cnpj, nome_instituicao, and then all columns for each 'conta' filled with values from 'saldo'

  balancetes_by_type <- purrr::map(
    balancetes_by_type,
    function(df) {
      # ensure 'conta' and 'saldo' columns exist
      if (!all(c("conta", "saldo", "number_data_base") %in% colnames(df))) {
        stop(
          "Columns 'conta', 'saldo', or 'number_data_base' not found in the data frame."
        )
      }

      df_wide <- tidyr::pivot_wider(
        df,
        id_cols = c(
          "number_data_base",
          "documento",
          "cnpj",
          "nome_instituicao"
        ),
        names_from = "conta",
        values_from = "saldo"
      )

      df_wide
    }
  )

  # Separete the balancetes_by_type into different data.frames by type of institution and save them as a .csv files if save = TRUE
  if (isTRUE(save)) {
    # ensure output directory exists
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # normalize output_filename into a function that returns a full path
    if (is.null(output_filename)) {
      output_filename <- function(type_inst) {
        file.path(out_dir, paste0(doc_filter, "_", tolower(type_inst), ".csv"))
      }
    } else if (is.character(output_filename) && length(output_filename) == 1L) {
      fixed_name <- output_filename
      output_filename <- function(type_inst) file.path(out_dir, fixed_name)
    } else if (is.function(output_filename)) {
      orig_fn <- output_filename
      output_filename <- function(type_inst) {
        file.path(out_dir, orig_fn(type_inst))
      }
    } else {
      stop("`output_filename` must be NULL, a single string, or a function.")
    }

    # write each type to CSV
    purrr::iwalk(
      balancetes_by_type,
      function(df, type_inst) {
        out_path <- output_filename(type_inst)
        readr::write_csv(df, out_path)
      }
    )
  }

  balancetes_by_type

  # Print the messages: this values are not updated by the inflation adjustment.
  message(glue::glue(
    "Note: The values in the balancetes are not adjusted for inflation. \n
    Check the the page https://www.bcb.gov.br/estabilidadefinanceira/balancetesbalancospatrimoniais "
  ))
}
