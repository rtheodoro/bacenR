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
            ln <- lines[i]
            if (nchar(trimws(ln)) == 0L) {
              next
            }

            delim <- if (grepl(";", ln)) {
              ";"
            } else if (grepl(",", ln)) {
              ","
            } else {
              "\\s+"
            }
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
              "Header containing 'DOCUMENTO' not found in file: {fp}"
            ))
          }

          con <- textConnection(lines)
          on.exit(close(con), add = TRUE)

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

  # Clean column names
  balancetes_by_type <- purrr::map(
    balancetes_by_type,
    janitor::clean_names
  )

  # Pivot, detecting whether values live in 'saldo' or 'valor'
  balancetes_by_type <- purrr::map(
    balancetes_by_type,
    function(df) {
      if (!("conta" %in% colnames(df))) {
        stop("Column 'conta' not found in the data frame.")
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

      required_id_cols <- c(
        "data",
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
        readr::write_csv(df, out_path)
      }
    )
  }

  message(glue::glue(
    "Note: The values in the balancetes are not adjusted for inflation. \n
    Check the the page https://www.bcb.gov.br/estabilidadefinanceira/balancetesbalancospatrimoniais "
  ))

  balancetes_by_type
}
