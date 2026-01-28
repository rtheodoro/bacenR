#' Process and unify institution CSV exports from the Brazilian Central Bank (Bacen) downloaded via `get_institutions()`.

#' @description
#' `r lifecycle::badge("experimental")`
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
#' @examples
#' \dontrun{
#' # Process institution files from a directory
#' institutions <- tidy_institutions(
#'   path_dir = "data/raw_institutions",
#'   out_dir = "data/processed_institutions",
#'   verbose = TRUE
#' )
#' }
#'
#' @export
tidy_institutions <- function(
  path_dir = "data",
  out_dir = "data",
  verbose = TRUE
) {
  # find all Excel and CSV files in path_dir
  candidate_files <- list.files(
    path_dir,
    pattern = "\\.(xlsx|xls|csv)$",
    full.names = TRUE,
    recursive = TRUE
  )

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
  import_file <- function(file_path) {
    if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
      # Read CSV to find header row
      lines <- readLines(file_path, warn = FALSE)
      header_row <- which(grepl("CNPJ", lines, ignore.case = TRUE))[1]

      if (is.na(header_row)) {
        return(NULL)
      }

      df <- read.csv(file_path, skip = header_row - 1, stringsAsFactors = FALSE)
    } else {
      # Read Excel to find header row
      temp_df <- tryCatch(
        readxl::read_xlsx(
          file_path,
          col_names = FALSE,
          .name_repair = "minimal"
        ),
        error = function(e) {
          readxl::read_xls(
            file_path,
            col_names = FALSE,
            .name_repair = "minimal"
          )
        }
      )

      if (is.null(temp_df)) {
        return(NULL)
      }

      # Find row with CNPJ
      header_row <- which(apply(temp_df, 1, function(row) {
        any(grepl("CNPJ", row, ignore.case = TRUE))
      }))[1]

      if (is.na(header_row)) {
        return(NULL)
      }

      # Re-read with correct skip
      df <- tryCatch(
        readxl::read_xlsx(file_path, skip = header_row - 1),
        error = function(e) {
          readxl::read_xls(file_path, skip = header_row - 1)
        }
      )
    }

    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }

    # Convert to data.frame
    df <- as.data.frame(df, stringsAsFactors = FALSE)

    # Remove any rows where CNPJ starts with FONTE. (based on original values)
    df <- df |>
      janitor::clean_names() |>
      dplyr::filter(stringr::str_detect(cnpj, "^FONTE", negate = TRUE)) |>
      dplyr::mutate(
        cnpj = stringr::str_remove_all(cnpj, "\\."),
        cnpj = as.numeric(cnpj)
      )

    df
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
        result <- import_file(file_path)
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
