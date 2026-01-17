#' Import balancete CSV files with flexible header detection
#'
#' @description
#' Imports CSV files from BCB by detecting the header line that contains
#' the tokens "DOCUMENTO", "CONTA", and "SALDO" (case-insensitive).
#' Automatically renames columns and removes unnecessary fields.
#'
#' This function is intended for internal use by \code{tidy_balance_sheets()}
#'
#' @param file_paths Character vector of file paths to import.
#'
#' @return A data.frame combining all imported files, or NULL if no valid files.
#'
#' @details
#' For each file:
#' 1. Reads line-by-line searching for a header containing DOCUMENTO, CONTA, and SALDO
#' 2. Auto-detects delimiter (`;`, `,`, or whitespace)
#' 3. Imports using data.table::fread for speed and robustness
#' 4. Renames: DATA_BASE/DATA BASE/#DATA_BASE -> DATA, NOME INSTITUICAO -> nome_instituicao
#' 5. Removes columns: ATRIBUTO, NOME CONTA, AGENCIA, COD_CONGL, NOME_CONGL, TAXONOMIA, NOME_CONTA
#' 6. Returns combined data.frame from all files
#'
#' @keywords internal

import_balance_sheets <- function(file_paths) {
  if (length(file_paths) == 0L) {
    return(NULL)
  }

  all_data <- purrr::map_dfr(
    file_paths,
    function(fp) {
      if (!file.exists(fp)) {
        warning(glue::glue("File not found: {fp}"))
        return(NULL)
      }

      # Read lines to find header
      con <- file(fp, encoding = "latin1")
      lines <- readLines(con, warn = FALSE)
      close(con)

      header_idx <- NA_integer_
      sep_used <- ";"

      for (i in seq_along(lines)) {
        ln <- lines[i]
        if (nchar(trimws(ln)) == 0L) {
          next
        }

        # Detect delimiter
        delim <- if (grepl(";", ln)) {
          ";"
        } else if (grepl(",", ln)) {
          ","
        } else {
          "\\s+"
        }

        # Split and clean tokens
        split_pattern <- if (identical(delim, "\\s+")) "\\s+" else delim
        tokens <- unlist(strsplit(ln, split_pattern, perl = TRUE))
        tokens <- trimws(gsub('^["\']|["\']$', "", tokens))

        if (length(tokens) == 0L) {
          next
        }

        # Check for required columns: DOCUMENTO, CONTA, SALDO
        has_doc <- any(grepl("^DOCUMENTO", tokens, ignore.case = TRUE))
        has_conta <- any(grepl("^CONTA$", tokens, ignore.case = TRUE))
        has_saldo <- any(grepl("^SALDO$", tokens, ignore.case = TRUE))

        if (isTRUE(has_doc) && isTRUE(has_conta) && isTRUE(has_saldo)) {
          header_idx <- i
          sep_used <- if (identical(delim, "\\s+")) "" else delim
          break
        }
      }

      if (is.na(header_idx)) {
        warning(glue::glue(
          "Header with DOCUMENTO, CONTA, and SALDO not found in: {fp}"
        ))
        return(NULL)
      }

      # Import table from header line using data.table::fread
      df <- tryCatch(
        {
          # Create temporary connection with lines from header onwards
          temp_lines <- lines[header_idx:length(lines)]
          temp_file <- tempfile(fileext = ".csv")
          writeLines(temp_lines, temp_file, useBytes = TRUE)

          dt <- data.table::fread(
            file = temp_file,
            sep = if (sep_used == "") "auto" else sep_used,
            header = TRUE,
            encoding = "Latin-1",
            fill = TRUE,
            na.strings = c("", "NA"),
            showProgress = FALSE,
            check.names = FALSE
          )

          unlink(temp_file)
          df <- as.data.frame(dt, stringsAsFactors = FALSE)

          # Rename DATA_BASE / #DATA_BASE / DATA BASE to DATA
          col_names <- names(df)
          data_col_idx <- which(grepl(
            "^(#)?DATA[_ ]?BASE$",
            col_names,
            ignore.case = TRUE
          ))
          if (length(data_col_idx) > 0) {
            names(df)[data_col_idx[1]] <- "DATA"
          }

          # Rename NOME INSTITUICAO to nome_instituicao
          nome_inst_idx <- which(grepl(
            "^NOME[_ ]INSTITUICAO$",
            col_names,
            ignore.case = TRUE
          ))
          if (length(nome_inst_idx) > 0) {
            names(df)[nome_inst_idx[1]] <- "nome_instituicao"
          }

          # Remove unwanted columns
          cols_to_remove <- c(
            "ATRIBUTO",
            "NOME CONTA",
            "AGENCIA",
            "COD_CONGL",
            "NOME_CONGL",
            "TAXONOMIA",
            "NOME_CONTA"
          )
          df <- df[, !names(df) %in% cols_to_remove, drop = FALSE]

          # Convert SALDO to numeric if it exists and is not already numeric
          if ("SALDO" %in% names(df) && !is.numeric(df$SALDO)) {
            df$SALDO <- as.numeric(df$SALDO)
          }

          df
        },
        error = function(e) {
          warning(glue::glue("Error reading {fp}: {conditionMessage(e)}"))
          NULL
        }
      )

      df
    }
  )

  all_data
}
