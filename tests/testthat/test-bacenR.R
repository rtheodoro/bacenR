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
    pattern = "\\d{6}.*\\.CSV$",
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
      bn <- basename(fp)
      new_bn <- gsub("_", "", bn, fixed = TRUE)
      new_fp <- file.path(dir, new_bn)

      if (identical(fp, new_fp)) {
        return(fp)
      }

      if (file.exists(new_fp)) {
        stop(glue::glue(
          "Target filename already exists: '{new_fp}'. Aborting to avoid overwrite."
        ))
      }

      ok <- file.rename(fp, new_fp)
      if (!ok) {
        stop(glue::glue("Failed to rename '{fp}' -> '{new_fp}'."))
      }

      new_fp
    },
    character(1)
  )

  files <- unname(new_paths)

  # extract years present
  years <- unique(na.omit(as.numeric(substr(basename(files), 1, 4))))
  if (length(years) == 0L) {
    stop(
      "Unable to extract years from filenames. Filenames must start with YYYYMM."
    )
  }

  first_year <- min(years)
  most_recent_year <- max(years)

  # helper to detect header line, skip and column names for a given file
  inspect_file <- function(file, n = 200L) {
    lines <- readLines(file, n = n, warn = FALSE)
    # find a header line that contains familiar column names
    header_idx <- which(
      grepl("\\bDOCUMENTO\\b", lines, ignore.case = TRUE) |
        grepl("\\bCNPJ\\b", lines, ignore.case = TRUE) |
        grepl("\\bCONTA\\b", lines, ignore.case = TRUE)
    )
    if (length(header_idx) == 0L) {
      # fallback: try any line that contains a semicolon and likely column names
      header_idx <- which(grepl(";", lines) & nchar(lines) < 200)
    }
    header_line <- if (length(header_idx) > 0L) min(header_idx) else 1L
    header_parts <- strsplit(lines[header_line], ";", fixed = TRUE)[[1]] |>
      trimws()

    # detect date-like and nome-like column names
    data_col <- header_parts[which(
      grepl("DATA_BASE", header_parts, ignore.case = TRUE) |
        grepl("#DATA", header_parts, ignore.case = TRUE) |
        grepl("#DATA_BASE", header_parts, ignore.case = TRUE)
    )]
    data_col_name <- if (length(data_col) >= 1L) data_col[1L] else NA_character_

    nome_col <- header_parts[which(
      grepl("NOME", header_parts, ignore.case = TRUE) |
        grepl("NOME_INSTITUICAO", header_parts, ignore.case = TRUE) |
        grepl("INSTITUICAO", header_parts, ignore.case = TRUE)
    )]
    nome_col_name <- if (length(nome_col) >= 1L) nome_col[1L] else NA_character_

    list(
      skip = header_line - 1L,
      data_col_name = data_col_name,
      nome_col_name = nome_col_name
    )
  }

  # build a per-year configuration (one row per year) by inspecting the first file found for that year
  balancete_rows <- lapply(years, function(y) {
    file_y <- files[substr(basename(files), 1, 4) == sprintf("%04d", y)][1L]
    institution_type_y <- sub(
      "^\\d{6}(.*?)\\.CSV$",
      "\\1",
      basename(file_y),
      ignore.case = TRUE
    ) |>
      trimws()
    info <- inspect_file(file_y)
    data.frame(
      year = y,
      institution_type = institution_type_y,
      skip = info$skip,
      data_col_name = ifelse(
        is.na(info$data_col_name),
        "DATA",
        info$data_col_name
      ),
      nome_col_name = ifelse(
        is.na(info$nome_col_name),
        "NOME_INSTITUICAO",
        info$nome_col_name
      ),
      stringsAsFactors = FALSE
    )
  })

  balancete_config <- do.call(rbind, balancete_rows)

  get_balancete_params <- function(year, config_df) {
    inst_types <- unique(config_df$institution_type)
    if (length(inst_types) > 1L) {
      stop(glue::glue(
        "Multiple institution types found: {paste(inst_types, collapse = ', ')}. ",
        "Please split files by institution type or run the function per type."
      ))
    }

    row <- config_df[config_df$year == year, , drop = FALSE]
    if (nrow(row) != 1L) {
      stop(glue::glue(
        "No unique balancete configuration found for year {year}."
      ))
    }

    list(
      skip = row$skip,
      data_col_name = row$data_col_name,
      nome_col_name = row$nome_col_name
    )
  }

  load_and_treat_balancete <- function(
    year,
    skip,
    data_col_name,
    nome_col_name
  ) {
    # look for any YYYYMM-prefixed files for the given year (any month)
    year_prefix <- sprintf("%04d", year)
    candidates <- list.files(
      path_raw,
      pattern = paste0("^", year_prefix, "\\d{2}.*\\.CSV$"),
      full.names = TRUE,
      ignore.case = TRUE
    )

    if (length(candidates) == 0L) {
      stop(glue::glue(
        "No CSV found for year '{year}' (prefix '{year_prefix}') in '{path_raw}'."
      ))
    }

    # if multiple files found for the year, read each (respecting skip) and combine into a single temp CSV
    if (length(candidates) > 1L) {
      warning(glue::glue(
        "Multiple files found for year {year}: {paste(basename(candidates), collapse = ', ')}. Processing each and combining results."
      ))

      dfs <- lapply(candidates, function(fp) {
        data.table::fread(
          fp,
          sep = ";",
          skip = skip,
          showProgress = FALSE,
          data.table = FALSE
        )
      })

      combined <- do.call(rbind, dfs)

      tmp <- tempfile(fileext = ".csv")
      data.table::fwrite(combined, file = tmp, sep = ";")
      file_path <- tmp
    } else {
      # single candidate: proceed as before
      file_path <- candidates[1L]
    }

    df <- data.table::fread(
      file_path,
      sep = ";",
      skip = skip,
      showProgress = FALSE,
      data.table = FALSE
    )

    cols_up <- toupper(trimws(names(df)))
    find_col <- function(target) {
      tgt <- toupper(target)
      idx <- which(cols_up == tgt)
      if (length(idx) == 0L) {
        idx <- grep(tgt, cols_up, fixed = TRUE)
      }
      if (length(idx) == 0L) {
        idx <- grep(gsub("_", "", tgt), gsub("_", "", cols_up), fixed = TRUE)
      }
      if (length(idx) == 0L) NA_character_ else names(df)[idx[1]]
    }

    cnpj_col <- find_col("CNPJ")
    doc_col <- find_col("DOCUMENTO")
    data_col_actual <- find_col(data_col_name)
    nome_col_actual <- find_col(nome_col_name)
    conta_col <- find_col("CONTA")
    saldo_col <- find_col("SALDO")

    missing_cols <- c(
      CNPJ = cnpj_col,
      DOCUMENTO = doc_col,
      DATA = data_col_actual,
      NOME = nome_col_actual,
      CONTA = conta_col,
      SALDO = saldo_col
    )
    missing_cols <- names(missing_cols)[is.na(unname(missing_cols))]
    if (length(missing_cols) > 0L) {
      stop(glue::glue(
        "Missing required columns in '{basename(file_path)}': {paste(missing_cols, collapse = ', ')}"
      ))
    }

    df |>
      dplyr::filter(.data[[doc_col]] == doc_filter) |>
      dplyr::select(dplyr::all_of(c(
        cnpj_col,
        data_col_actual,
        nome_col_actual,
        conta_col,
        saldo_col
      )))
  }

  resultado <- tibble::tibble() # acumulador

  for (year in years) {
    params <- get_balancete_params(year, balancete_config)

    csv_i <- load_and_treat_balancete(
      year = year,
      data_col_name = params$data_col_name,
      nome_col_name = params$nome_col_name,
      skip = params$skip
    )

    names(csv_i)[1:3] <- c("cnpj", "ano", "razao_social")

    csv_i <- csv_i |>
      tidyr::pivot_wider(
        id_cols = dplyr::all_of(c("cnpj", "ano", "razao_social")),
        names_from = dplyr::all_of("CONTA"),
        values_from = dplyr::all_of("SALDO")
      )

    resultado <- dplyr::bind_rows(resultado, csv_i)
    rm(csv_i)
  }

  resultado <- resultado |>
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("[0-9]"),
        ~ as.numeric(gsub(",", ".", .))
      )
    )

  if (isTRUE(save)) {
    if (is.null(output_filename)) {
      output_filename <- glue::glue(
        "{candidates}_{first_year}a{most_recent_year}_{doc_filter}.csv"
      )
    }
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(
      resultado,
      file = file.path(out_dir, output_filename),
      row.names = FALSE
    )
  }

  resultado
}

# call with defaults
tratamento_balancetes()
