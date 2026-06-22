globalVariables(
  c(
    "grupo", "conta", "numero_relatorio", "nome_relatorio",
    "tipo_instituicao", "descricao_coluna", "cod_inst",
    "nome_coluna", "saldo"
  )
)

#' Tidy ifdata Reports Data
#'
#' @description
#'  `r lifecycle::badge("stable")`
#' 
#' Transforms raw ifdata reports downloaded with `get_ifdata_reports()` into a clean, wide-format dataset suitable for
#' analysis. This function handles data input from multiple sources (direct
#' data.frame, CSV, or Parquet files), removes unnecessary columns, renames
#' identifiers, and pivots the data from long to wide format.
#'
#' @param data Optional. A data.frame/tibble or file path (CSV or Parquet).
#'   The data must be downloaded via \code{\link{get_ifdata_reports}}.
#'   If a character string, must be a valid path to a .csv or .parquet file.
#'   If a data.frame, it should contain the raw ifdata reports structure with
#'   columns: \code{cod_inst}, \code{grupo}, \code{conta}, \code{numero_relatorio},
#'   \code{nome_relatorio}, \code{tipo_instituicao}, \code{descricao_coluna},
#'   \code{nome_coluna}, and \code{saldo}.
#'
#' @return A tibble with the following transformations applied:
#'   \itemize{
#'     \item Columns removed: grupo, conta, numero_relatorio, nome_relatorio,
#'           tipo_instituicao, descricao_coluna
#'     \item Column renamed: cod_inst → cnpj
#'     \item Structure pivoted: Long format (multiple rows per institution) →
#'           Wide format (one row per institution with columns for each report item)
#'     \item Column names: Cleaned to lowercase with underscores via
#'           \code{\link[janitor]{clean_names}}
#'   }
#'
#' @details
#' The function performs the following operations in sequence:
#'
#' 1. **Data Input Handling**: Accepts data in two forms:
#'    - Direct tibble/data.frame (preferred for in-memory operations)
#'    - File path to CSV or Parquet (for disk-based data)
#'
#' 2. **Column Removal**: Drops metadata columns that are redundant after
#'    pivoting (grupo, conta, numero_relatorio, etc.)
#'
#' 3. **Identifier Rename**: Renames \code{cod_inst} to \code{cnpj} for clarity
#'    that rows represent financial institutions by their CNPJ identifier
#'
#' 4. **Pivot Operation**: Transforms from long format (each report metric in
#'    a separate row) to wide format (each metric becomes a column). The pivot
#'    uses \code{nome_coluna} for column names and \code{saldo} for cell values.
#'
#' 5. **Name Cleaning**: Applies janitor's name standardization to ensure
#'    consistent, R-friendly column naming
#'
#' @examples
#' \dontrun{
#' # Example 1: Tidy data from a CSV file
#' tidy_data <- tidy_ifdata_reports(data = "reports.csv")
#'
#' # Example 2: Tidy data from a Parquet file
#' tidy_data <- tidy_ifdata_reports(data = "reports.parquet")
#'}
#' \donttest{
#' # Example 3: Tidy an existing data.frame
#' raw_data <- get_ifdata_reports(year = 2024, month = 12, 
#'                                 type_institution = 2, report = 4)
#' tidy_data <- tidy_ifdata_reports(data = raw_data)
#' }
#'
#' @seealso
#' \code{\link{get_ifdata_reports}} for downloading raw data
#'
#' @importFrom dplyr select rename
#' @importFrom tidyr pivot_wider
#' @importFrom janitor clean_names
#' @importFrom utils read.csv
#' @importFrom arrow read_parquet
#' @keywords internal
#' @export
#'
tidy_ifdata_reports <- function(data) {
  if (inherits(data, "data.frame")) {
    df <- data
  } else if (is.character(data) && length(data) == 1) {
    ext <- tolower(tools::file_ext(data))

    df <- switch(
      ext,
      csv = utils::read.csv(data, stringsAsFactors = FALSE),
      parquet = arrow::read_parquet(data),
      stop("`data` must be a data.frame/tibble or a path to a .csv or .parquet file.")
    )
  } else {
    stop("`data` must be a data.frame/tibble or a path to a .csv or .parquet file.")
  }

  df |>
    dplyr::select(
      -dplyr::any_of(c(
        "grupo", "conta", "numero_relatorio", "nome_relatorio",
        "tipo_instituicao", "descricao_coluna"
      ))
    ) |>
    dplyr::rename(cnpj = cod_inst) |>
    tidyr::pivot_wider(
      names_from = nome_coluna,
      values_from = saldo
    ) |>
    janitor::clean_names()
}

