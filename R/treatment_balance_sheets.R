treatment_balance_sheets <- function(
  path_raw = "data_raw",
  out_dir = "data",
  doc_filter = 4010,
  save = TRUE,
  output_filename = NULL
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

  new_paths <- vapply(
    files,
    function(fp) {
      directory_path <- dirname(fp)
      base_name <- basename(fp)
      new_bn <- gsub("_", "", base_name, fixed = TRUE)
      new_file_path <- file.path(directory_path, new_bn)

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

  info_list <- lapply(
    files,
    function(fp) {
      file_basename <- basename(fp)
      year <- as.numeric(substr(file_basename, 1, 4))
      month <- as.numeric(substr(file_basename, 5, 6))
      type_inst <- gsub(
        "[0-9]{6}|\\.CSV$",
        "",
        toupper(file_basename),
        perl = TRUE
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

  info_df <- do.call(rbind, info_list)
  unique_types <- unique(info_df$type_institution)

  balancetes_by_type <- purrr::set_names(unique_types) |>
    purrr::map(function(inst_type) {
      inst_files <- info_df$file_path[info_df$type_institution == inst_type]

      balancetes_inst <- purrr::map_dfr(
        inst_files,
        function(fp) {
          lines <- readLines(fp, encoding = "latin1", warn = FALSE)
          header_idx <- NA_integer_
          sep_used <- ","

          for (i in seq_along(lines)) {
            current_line <- lines[i]
            if (nchar(trimws(current_line)) == 0L) {
              next
            }

            delim <- if (grepl(";", current_line)) {
              ";"
            } else if (grepl(",", current_line)) {
              ","
            } else {
              "\\s+"
            }
            split_pattern <- if (identical(delim, "\\s+")) "\\s+" else delim
            tokens <- unlist(strsplit(current_line, split_pattern, perl = TRUE))
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
              "^DATA$|DATA[_ ]?BASE|#DATA_BASE",
              tokens,
              ignore.case = TRUE
            ))

            if (isTRUE(has_doc) && isTRUE(has_cnpj) && isTRUE(has_data)) {
              header_idx <- i
              sep_used <- if (identical(delim, "\\s+")) "" else delim
              break
            }
          }

          if (is.na(header_idx)) {
            stop(glue::glue(
              "Header containing 'DOCUMENTO', 'CNPJ', and 'DATA' not found in file: {fp}"
            ))
          }

          text_connection <- textConnection(lines)
          on.exit(close(text_connection), add = TRUE)

          df <- utils::read.table(
            text_connection,
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

          # Normalize date-like header variants to a single "DATA_BASE" name
          nm <- names(df)
          # remove leading '#' for comparison and upper-case
          nm_cmp <- toupper(gsub("^\\s*#", "", nm, perl = TRUE))
          nm[nm_cmp %in% c("DATA", "DATA_BASE")] <- "DATA_BASE"
          # Normalize NOME INSTITUICAO variants to a single consistent header before further processing
          nm[
            nm_cmp %in% c("NOME INSTITUICAO", "NOME_INSTITUICAO")
          ] <- "NOME_INSTITUICAO"
          names(df) <- nm

          # Clean NOME_INSTITUICAO values: remove accents and special characters
          if ("NOME_INSTITUICAO" %in% names(df)) {
            df[["NOME_INSTITUICAO"]] <- as.character(df[["NOME_INSTITUICAO"]])
            df[["NOME_INSTITUICAO"]] <- trimws(df[["NOME_INSTITUICAO"]])
            # transliterate accents -> ASCII
            df[["NOME_INSTITUICAO"]] <- iconv(
              df[["NOME_INSTITUICAO"]],
              from = "latin1",
              to = "ASCII//TRANSLIT"
            )
            # remove any remaining non-alphanumeric characters (preserve space)
            df[["NOME_INSTITUICAO"]] <- gsub(
              "[^A-Za-z0-9 ]+",
              "",
              df[["NOME_INSTITUICAO"]],
              perl = TRUE
            )
            # collapse multiple spaces
            df[["NOME_INSTITUICAO"]] <- gsub(
              "\\s+",
              " ",
              df[["NOME_INSTITUICAO"]],
              perl = TRUE
            )
          }

          # Helper to coerce common numeric formats to numeric
          to_numeric_clean <- function(x) {
            if (is.numeric(x)) {
              return(x)
            }
            x_chr <- as.character(x)
            x_chr[is.na(x_chr)] <- NA_character_
            x_chr <- trimws(x_chr)

            # detect parentheses for negatives
            par_neg <- grepl("^\\(.*\\)$", x_chr)
            x_chr <- gsub("^\\(|\\)$", "", x_chr)

            # vectorized handling
            clean_vec <- x_chr

            # mark valid entries (non-NA, non-empty); others stay NA
            valid <- !is.na(clean_vec) & clean_vec != ""
            clean_vec[!valid] <- NA_character_

            if (any(valid)) {
              xi <- clean_vec[valid]

              # preserve leading sign if present
              has_sign <- grepl("^[+-]", xi)
              sign_chr <- ifelse(has_sign, substr(xi, 1, 1), "")
              xi[has_sign] <- substring(xi[has_sign], 2)

              # Normalize leading zeros:
              # - collapse all-zeros to single "0"
              # - if zeros precede decimal/comma, keep single "0"
              # - if zeros precede non-zero digit, remove them
              xi <- sub("^0+$", "0", xi, perl = TRUE)
              xi <- sub("^0+(?=[\\.,])", "0", xi, perl = TRUE)
              xi <- sub("^0+(?=[1-9])", "", xi, perl = TRUE)

              # count separators
              dot_count <- lengths(regmatches(
                xi,
                gregexpr("\\.", xi, perl = TRUE)
              ))
              comma_count <- lengths(regmatches(
                xi,
                gregexpr(",", xi, perl = TRUE)
              ))

              # both present -> assume dot thousands, comma decimal: remove dots, replace comma->dot
              both <- dot_count > 0 & comma_count > 0
              if (any(both)) {
                xi[both] <- gsub("\\.", "", xi[both], perl = TRUE)
                xi[both] <- gsub(",", ".", xi[both], perl = TRUE)
              }

              # only comma present -> comma is decimal
              only_comma <- comma_count > 0 & dot_count == 0
              if (any(only_comma)) {
                xi[only_comma] <- gsub(",", ".", xi[only_comma], perl = TRUE)
              }

              # multiple dots -> likely thousand separators -> remove all
              multi_dot <- dot_count > 1 & comma_count == 0
              if (any(multi_dot)) {
                xi[multi_dot] <- gsub("\\.", "", xi[multi_dot], perl = TRUE)
              }

              # reattach sign if present
              if (any(has_sign)) {
                xi <- paste0(sign_chr, xi)
              }

              # remove any non-numeric except minus and dot
              xi <- gsub("[^0-9\\.-]", "", xi, perl = TRUE)

              clean_vec[valid] <- xi
            }
            num <- as.numeric(clean_vec)
            num[par_neg] <- -abs(num[par_neg])
            num
          }

          # Coerce 'saldo' or 'valor' columns (case-insensitive, contains 'SALDO' or exact 'VALOR')
          cols_to_clean <- names(df)[
            grepl("SALDO", names(df), ignore.case = TRUE) |
              grepl("^VALOR$", names(df), ignore.case = TRUE)
          ]

          if (length(cols_to_clean) > 0L) {
            for (col in cols_to_clean) {
              df[[col]] <- to_numeric_clean(df[[col]])
            }
          }

          # Filter by DOCUMENTO column in a case-insensitive, safe way
          doc_col <- names(df)[toupper(names(df)) == "DOCUMENTO"]

          if (length(doc_col) == 1L) {
            df[df[[doc_col]] == doc_filter, , drop = FALSE]
          } else {
            # If no matching DOCUMENTO column is found, return an empty data frame
            df[FALSE, , drop = FALSE]
          }
        }
      )

      balancetes_inst
    })

  # Clean column names
  balancetes_by_type <- purrr::map(
    balancetes_by_type,
    function(df) {
      df2 <- janitor::clean_names(df)

      if (
        "number_data_base" %in% colnames(df2) && !("data" %in% colnames(df2))
      ) {
        df2 <- dplyr::rename(df2, data = number_data_base)
      } else if (
        "data_base" %in% colnames(df2) && !("data" %in% colnames(df2))
      ) {
        df2 <- dplyr::rename(df2, data = data_base)
      }

      # ensure compatibility: create number_data_base from data if downstream code expects it
      if (
        !("number_data_base" %in% colnames(df2)) && ("data" %in% colnames(df2))
      ) {
        df2 <- dplyr::mutate(df2, number_data_base = .data$data)
      }

      # Ensure there's a standardized nome_instituicao column after cleaning
      if (!("nome_instituicao" %in% colnames(df2))) {
        match_idx <- which(grepl(
          "^nome.*inst",
          colnames(df2),
          ignore.case = TRUE
        ))
        if (length(match_idx) >= 1L) {
          names(df2)[match_idx[1]] <- "nome_instituicao"
        }
      }

      df2
    }
  )

  # Pivot, detecting whether values live in 'saldo' or 'valor'
  balancetes_by_type <- purrr::map(
    balancetes_by_type,
    function(df) {
      if (!("conta" %in% colnames(df))) {
        stop("Column 'conta' not found in the data frame.")
      }

      # Ensure 'conta' is character, trimmed, and remove empty entries to avoid creating '0' or empty columns after pivot
      df$conta <- as.character(df$conta)
      df$conta <- trimws(df$conta)
      df$conta[df$conta == ""] <- NA_character_
      df <- df[!is.na(df$conta), , drop = FALSE]

      # If number_data_base is absent but data exists, make a consistent id column for pivot
      if (
        !("number_data_base" %in% colnames(df)) && ("data" %in% colnames(df))
      ) {
        df$number_data_base <- df$data
      }

      values_col <- if ("saldo" %in% colnames(df)) {
        "saldo"
      } else if ("valor" %in% colnames(df)) {
        "valor"
      } else {
        stop(
          "Neither 'saldo' nor 'valor' columns were found in the data frame."
        )
      }

      # Coerce the values column to numeric (safely), so non-numeric entries become NA instead of 0
      if (!is.numeric(df[[values_col]])) {
        df[[values_col]] <- suppressWarnings(as.numeric(as.character(df[[
          values_col
        ]])))
      }

      required_id_cols <- c(
        "number_data_base",
        "documento",
        "cnpj",
        "nome_instituicao"
      )
      missing_ids <- setdiff(required_id_cols, colnames(df))
      if (length(missing_ids) > 0L) {
        stop(glue::glue(
          "Missing id columns required for pivot: {paste(missing_ids, collapse = ', ')}"
        ))
      }

      df_wide <- tidyr::pivot_wider(
        df,
        id_cols = required_id_cols,
        names_from = "conta",
        values_from = dplyr::all_of(values_col)
      )

      # Ensure final id column is named 'data' (rename or drop duplicate)
      if (
        "number_data_base" %in%
          colnames(df_wide) &&
          !("data" %in% colnames(df_wide))
      ) {
        df_wide <- dplyr::rename(df_wide, data = number_data_base)
      } else if (
        "number_data_base" %in%
          colnames(df_wide) &&
          "data" %in% colnames(df_wide)
      ) {
        df_wide <- dplyr::select(df_wide, -number_data_base)
      }

      df_wide
    }
  )

  if (isTRUE(save)) {
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

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

    purrr::iwalk(
      balancetes_by_type,
      function(df, type_inst) {
        out_path <- output_filename(type_inst)

        # Ensure balance columns are numeric (they were converted earlier by to_numeric_clean).
        # For writing, format numeric columns to avoid scientific notation while preserving numeric semantics internally.
        df_out <- df

        # Identify numeric columns that are account values (exclude key id columns)
        id_cols <- c(
          "documento",
          "cnpj",
          "nome_instituicao",
          "data",
          "number_data_base"
        )
        numeric_cols <- names(df_out)[vapply(df_out, is.numeric, logical(1))]
        numeric_value_cols <- setdiff(
          numeric_cols,
          intersect(numeric_cols, id_cols)
        )

        if (length(numeric_value_cols) > 0L) {
          # format numeric columns as non-scientific strings for stable CSV representation
          for (col in numeric_value_cols) {
            # format preserves decimal places as needed and disables scientific notation
            df_out[[col]] <- format(
              df_out[[col]],
              scientific = FALSE,
              trim = TRUE
            )
          }
        }

        readr::write_csv(df_out, out_path)
      }
    )
  }

  message(glue::glue(
    "Note: The values in the balance sheets are not adjusted for inflation or modified. \n
     This function still in experimental stage, please use with caution. Check the results. \n
     Long time data may have inconsistencies or format changes. \n
    Check the page https://www.bcb.gov.br/estabilidadefinanceira/balancetesbalancospatrimoniais "
  ))

  balancetes_by_type
}
