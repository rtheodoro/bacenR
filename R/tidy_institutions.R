#' Process and unify institution CSV exports from the Brazilian Central Bank (Bacen) downloaded via `get_institutions()`.

#' @description
#'  `r lifecycle::badge("stable")`
#'
#' This function reads and processes [financial institutions data from the Brazilian Central Bank (Bacen)](https://www.bcb.gov.br/estabilidadefinanceira/relacao_instituicoes_funcionamento)
#' files that were previously
#' downloaded using the `get_institutions()` function. It takes a directory path
#' containing the institution files, processes them, and optionally saves the
#' output to a specified directory.
#'
#' @param path_dir Character string specifying the path to the directory
#'   containing institution files to be processed.
#' @param out_dir Character string specifying the path to the directory where
#'   processed output files should be saved.
#' @param verbose Logical value indicating whether to print progress messages
#'   during processing. Default is typically FALSE.
#'
#' @return A list of data.frames, where each element corresponds to a processed
#'   institution file. The names of the list elements typically correspond to
#'   institution identifiers or file names.
#'
#' @details
#' Bacen changes column headers over the years. After `janitor::clean_names()`,
#' columns are renamed to the most recent header so each institution type has
#' a single column per field:
#' \itemize{
#'   \item All types: `nome`, `nomeinstituicao` -> `nome_instituicao`;
#'     `fone` -> `telefone`; `cidade` -> `municipio`; `segmentos` -> `segmento`.
#'   \item E-mail and website columns (`email`, `e_mail`, `endereco_eletronico`,
#'     `sitio_na_internet`, `end_pagina_internet`) are assigned to `e_mail` or
#'     `sitio_na_internet` by their content (share of values containing "@"),
#'     because Bacen's headers for them are inconsistent and sometimes swapped.
#'   \item COOPERATIVAS: `classe_cooper` -> `classe`.
#'   \item CONGLOMERADOS: `nome_conglomerado` -> `nome_do_conglomerado`;
#'     `classe` -> `classe_do_conglomerado`; `nome_instituicao_participante`,
#'     `nome_participante` -> `nome_do_participante`; `dt_inicio` ->
#'     `data_inicio` (converted to `Date`); `estado` -> `uf`.
#' }
#' Columns that are not equivalent across years (e.g. COOPERATIVAS
#' `tipo_cooperativa` until 2015 and `criterio_de_associacao` from 2016) are
#' kept separate.
#'
#' @examples
#'# First, download institution data
#'# Download cooperative credit unions data for 2023
#' get_institutions(
#'   institution = "COOPERATIVAS",
#'   years = 2023,
#'   months = 11:12,
#'   out_dir = tempdir()
#' )
#' # Process institution files from a directory
#' institutions <- tidy_institutions(
#'   path_dir = tempdir(),
#'   out_dir = tempdir(),
#'   verbose = TRUE
#' )
#' @export
tidy_institutions <- function(
  path_dir,
  out_dir,
  verbose = TRUE
) {
  # find all Excel and CSV files in path_dir
  candidate_files <- list.files(
    path_dir,
    # Only raw Bacen files (YYYYMM<INST>.ext); avoids re-reading our own output
    pattern = "^[0-9]{6}[A-Za-z]+\\.(xlsx|xls|csv)$",
    full.names = TRUE,
    recursive = TRUE
  )

  cnpj <- as.numeric()

  if (length(candidate_files) == 0) {
    if (verbose) {
      message("No files found in ", path_dir)
    }
    return(list())
  }

  # parse file names to extract institution type
  file_info <- data.frame(
    file_path = candidate_files,
    file_name = basename(candidate_files),
    stringsAsFactors = FALSE
  )

  # extract institution from filename (remove first 6 chars for YYYYMM, then remove extension)
  file_info$institution <- vapply(
    file_info$file_name,
    function(fname) {
      base_name <- tools::file_path_sans_ext(fname)
      substr(base_name, 7, nchar(base_name))
    },
    character(1)
  )
  # Bacen renamed the file from CONGLOMERADOS to CONGLOMERADO after 2022
  file_info$institution[
    file_info$institution == "CONGLOMERADO"
  ] <- "CONGLOMERADOS"

  # extract year-month from filename
  file_info$data <- vapply(
    file_info$file_name,
    function(fname) {
      base_name <- tools::file_path_sans_ext(fname)
      substr(base_name, 1, 6)
    },
    character(1)
  )

  # Helper function to import a single file
  import_file <- function(file_path, inst) {
    if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
      # Read CSV to find header row
      lines <- readLines(file_path, warn = FALSE)
      header_row <- which(grepl("CNPJ", lines, ignore.case = TRUE))[1]

      if (is.na(header_row)) {
        return(NULL)
      }

      df <- read.csv(file_path, skip = header_row - 1, stringsAsFactors = FALSE)
    } else {
      sheets <- tryCatch(readxl::excel_sheets(file_path), error = function(e) {
        character(0)
      })

      # Some files contain extra sheets: 201212COOPERATIVAS.xls has a small
      # "SCM" sheet before the main one, and 201112 BANCOS/ADMCONSORCIO have
      # "Posição em 31.12" followed by "Posição em 01.12". Use the first sheet
      # with a CNPJ header unless it is much smaller than the largest one.
      sheet_info <- lapply(sheets, function(sheet) {
        temp_df <- tryCatch(
          readxl::read_excel(
            file_path,
            sheet = sheet,
            col_names = FALSE,
            .name_repair = "minimal"
          ),
          error = function(e) NULL
        )
        if (is.null(temp_df) || nrow(temp_df) == 0) {
          return(NULL)
        }
        header_row <- which(apply(temp_df, 1, function(row) {
          any(grepl("CNPJ", row, ignore.case = TRUE))
        }))[1]
        if (is.na(header_row)) {
          return(NULL)
        }
        list(
          sheet = sheet,
          header_row = header_row,
          n_rows = nrow(temp_df) - header_row
        )
      })
      sheet_info <- Filter(Negate(is.null), sheet_info)

      if (length(sheet_info) == 0) {
        return(NULL)
      }

      n_rows <- vapply(sheet_info, `[[`, numeric(1), "n_rows")
      best <- sheet_info[[which(n_rows >= 0.5 * max(n_rows))[1]]]

      # The footer row ("FONTE: ...") sits in the numeric CNPJ column and
      # triggers a coercion warning; it becomes NA and is dropped below.
      df <- withCallingHandlers(
        readxl::read_excel(
          file_path,
          sheet = best$sheet,
          skip = best$header_row - 1
        ),
        warning = function(w) {
          if (grepl("got 'FONTE", conditionMessage(w), ignore.case = TRUE)) {
            invokeRestart("muffleWarning")
          }
        },
        # Empty header cells; those all-NA columns are dropped later
        message = function(m) {
          if (grepl("^New names", conditionMessage(m))) {
            invokeRestart("muffleMessage")
          }
        }
      )
    }

    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }

    df <- janitor::clean_names(as.data.frame(df, stringsAsFactors = FALSE))

    # CONGLOMERADOS files use "CNPJ PARTIC." / "CNPJ PARTICIPANTE"
    cnpj_col <- grep("^cnpj", names(df))[1]
    if (is.na(cnpj_col)) {
      return(NULL)
    }
    names(df)[cnpj_col] <- "cnpj"

    # Drop blank rows and the "FONTE:"/"Fonte:" footer, but keep real rows
    # whose CNPJ is missing (e.g. conglomerate participants without CNPJ).
    is_blank <- rowSums(!is.na(df)) == 0
    is_footer <- apply(df, 1, function(row) {
      any(grepl("^\\s*FONTE", row, ignore.case = TRUE))
    })
    df <- df[!is_blank & !is_footer, , drop = FALSE]

    df$cnpj <- as.numeric(stringr::str_remove_all(df$cnpj, "\\."))

    harmonize_institution_cols(df, inst)
  }

  # Group files by institution first
  file_info_split <- split(file_info, file_info$institution)

  # Process each institution separately
  inst_list <- lapply(names(file_info_split), function(inst_name) {
    inst_files <- file_info_split[[inst_name]]

    if (verbose) {
      message("Processing institution: ", inst_name)
    }

    # Process all files for this institution
    inst_results <- Map(
      function(file_path, file_data, file_name) {
        if (verbose) {
          message("  - ", file_name)
        }
        result <- import_file(file_path, inst_name)
        if (!is.null(result)) {
          result$institution <- inst_name
          result$data <- file_data
          result
        } else {
          NULL
        }
      },
      inst_files$file_path,
      inst_files$data,
      inst_files$file_name
    )

    # Remove NULL results
    inst_results <- inst_results[!sapply(inst_results, is.null)]

    if (length(inst_results) == 0) {
      return(NULL)
    }

    # Combine data frames for this institution only
    all_cols <- unique(unlist(lapply(inst_results, names)))
    inst_data <- do.call(
      rbind,
      lapply(inst_results, function(df) {
        # Add missing columns with NA
        missing_cols <- setdiff(all_cols, names(df))
        if (length(missing_cols) > 0) {
          df[missing_cols] <- NA
        }
        df[all_cols]
      })
    )

    inst_data
  })

  # Name the list and remove NULLs
  names(inst_list) <- names(file_info_split)
  inst_list <- inst_list[!sapply(inst_list, is.null)]

  if (length(inst_list) == 0) {
    if (verbose) {
      message("No valid data found in any files")
    }
    return(list())
  }

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Write CSV files for each institution
  purrr::iwalk(inst_list, function(df, inst_name) {
    # Sanitize institution name before using it in a file path
    safe_inst_name <- gsub("[^A-Za-z0-9._-]", "_", inst_name)

    out_file_path <- file.path(out_dir, paste0(safe_inst_name, ".csv"))
    write.csv(df, out_file_path, row.names = FALSE)
    if (verbose) {
      message(
        "Wrote CSV for ",
        inst_name,
        ": ",
        out_file_path
      )
    }
  })

  invisible(inst_list)
}

# Column renames (old = new) applied after janitor::clean_names(). `.all`
# applies to every institution type; type-specific maps are applied first.
institution_col_map <- list(
  .all = c(
    nome = "nome_instituicao",
    nomeinstituicao = "nome_instituicao",
    fone = "telefone",
    cidade = "municipio",
    segmentos = "segmento"
  ),
  COOPERATIVAS = c(
    classe_cooper = "classe"
  ),
  CONGLOMERADOS = c(
    nome_conglomerado = "nome_do_conglomerado",
    classe = "classe_do_conglomerado",
    nome_instituicao_participante = "nome_do_participante",
    nome_participante = "nome_do_participante",
    dt_inicio = "data_inicio",
    estado = "uf"
  )
)

contact_cols <- c(
  "email",
  "e_mail",
  "endereco_eletronico",
  "sitio_na_internet",
  "end_pagina_internet"
)

# Rename `from` to `to`; if `to` already exists, fill its NAs from `from`.
rename_or_merge <- function(df, from, to) {
  if (from == to || !from %in% names(df)) {
    return(df)
  }
  if (to %in% names(df)) {
    df[[to]] <- ifelse(
      is.na(df[[to]]),
      as.character(df[[from]]),
      as.character(df[[to]])
    )
    df[[from]] <- NULL
  } else {
    names(df)[names(df) == from] <- to
  }
  df
}

# Excel dates arrive as POSIXct, Excel serial numbers, or "dd/mm/yy(yy)" text.
as_bacen_date <- function(x) {
  if (inherits(x, c("POSIXt", "Date"))) {
    return(as.Date(x))
  }
  x <- trimws(as.character(x))
  out <- as.Date(rep(NA_character_, length(x)))

  serial <- grepl("^[0-9]+(\\.0+)?$", x)
  out[serial] <- as.Date(as.numeric(x[serial]), origin = "1899-12-30")

  dmy4 <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", x)
  dmy2 <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2}$", x)

  # When serials are mixed with dd/mm text (CONGLOMERADOS 201912, 202112),
  # Excel had parsed the dd/mm dates with day <= 12 as mm/dd: swap them back.
  if (any(serial) && any(dmy4 | dmy2)) {
    out[serial] <- as.Date(format(out[serial], "%Y-%d-%m"))
  }

  out[dmy4] <- as.Date(x[dmy4], format = "%d/%m/%Y")
  out[dmy2] <- as.Date(x[dmy2], format = "%d/%m/%y")
  # %y maps 00-68 to 20xx; push impossible future dates back a century
  future <- dmy2 & !is.na(out) & out > Sys.Date()
  out[future] <- as.Date(paste0(
    as.integer(format(out[future], "%Y")) - 100L,
    format(out[future], "-%m-%d")
  ))

  out
}

harmonize_institution_cols <- function(df, inst) {
  # Drop all-NA columns created from empty header cells (x2, x15, ...)
  empty_unnamed <- grepl("^x[0-9]*$", names(df)) &
    vapply(df, function(col) all(is.na(col)), logical(1))
  df <- df[, !empty_unnamed, drop = FALSE]

  maps <- c(institution_col_map[[inst]], institution_col_map$.all)
  for (from in names(maps)) {
    df <- rename_or_merge(df, from, maps[[from]])
  }

  # Assign e-mail/website columns by content rather than by header. Classify
  # all of them before renaming, since headers are sometimes swapped.
  present <- intersect(contact_cols, names(df))
  if (length(present) > 0) {
    targets <- vapply(
      present,
      function(col) {
        values <- df[[col]][!is.na(df[[col]])]
        is_email <- if (length(values) > 0) {
          mean(grepl("@", values)) > 0.5
        } else {
          col %in% c("email", "e_mail")
        }
        if (is_email) "e_mail" else "sitio_na_internet"
      },
      character(1)
    )
    sources <- df[present]
    df[present] <- NULL
    for (target in unique(targets)) {
      cols <- sources[names(targets)[targets == target]]
      df[[target]] <- Reduce(
        function(a, b) ifelse(is.na(a), b, a),
        lapply(cols, as.character)
      )
    }
  }

  if ("data_inicio" %in% names(df)) {
    df$data_inicio <- as_bacen_date(df$data_inicio)
  }

  df
}
