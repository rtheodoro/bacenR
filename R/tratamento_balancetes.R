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
      normalized_basename <- gsub("_", "", bn, fixed = TRUE)
      new_file_path <- file.path(dir, new_bn)

      if (identical(fp, new_file_path)) {
        return(fp)
      }

      if (file.exists(new_file_path)) {
        stop(glue::glue(
          "Target filename already exists: '{new_file_path}'. Aborting to avoid overwrite."
        ))
      }

      rename_succeeded <- file.rename(fp, new_fp)
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
          df <- utils::read.csv(
            fp,
            {
              # locate the header line containing "DOCUMENTO" or "DOCUMENTOS" (case-insensitive)
              lines <- readLines(fp, encoding = "latin1", warn = FALSE)
              header_idx <- grep("DOCUMENTO(S)?", lines, ignore.case = TRUE)[1L]
              if (is.na(header_idx)) {
                stop(glue::glue(
                  "Header containing 'DOCUMENTO' not found in file: {fp}"
                ))
              }
              header_idx - 1L
            },
            stringsAsFactors = FALSE,
            fileEncoding = "latin1"
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
