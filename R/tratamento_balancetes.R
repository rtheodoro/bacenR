tratamento_balancetes <- function(
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
}

# call with defaults
tratamento_balancetes()
