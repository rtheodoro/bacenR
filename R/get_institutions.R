#' get_institutions
#'
#' Download, extract and parse monthly "cooperativas" institution reports from the
#' Banco Central do Brasil (Bacen) and return a cleaned tibble of institution data.
#'
#' @param start_date Character. Start of the monthly sequence (inclusive). Expected in "YYYY-MM" format (default: "2007-09").
#' @param end_date Character. End of the monthly sequence (inclusive). Expected in "YYYY-MM" format (default: "2024-09").
#' @param out_dir Character. Directory used to store downloaded and extracted files (default: "download"). Created if it does not exist.
#' @param output_csv Character or NULL. Path to write the final combined CSV. If NULL or empty string, no CSV is written (default: "coop_em_funcionamento_bacen.csv").
#' @param download_files Logical. If TRUE the function will attempt to download monthly ZIP files from the BCB; if FALSE it will only process files already present in out_dir (default: TRUE).
#' @param cleanup_zip Logical. If TRUE temporarily downloaded ZIP files are removed after extraction (default: TRUE).
#' @param verbose Logical. Currently reserved for future use; no verbose output is produced by the function as implemented (default: FALSE).
#'
#' @details
#' The function:
#' - Builds a sequence of months between start_date and end_date (formatted as YYYYMM).
#' - For each month, it constructs a URL to download a ZIP file containing the cooperatives report and attempts to download it using httr::GET.
#' - Extracts any found ZIP archives into out_dir and (optionally) deletes the ZIPs.
#' - Lists extracted files whose filenames contain "COOPERATIVAS" (case-insensitive).
#' - Reads each file attempting readxl::read_xlsx first and falling back to readxl::read_xls, using skip = 9 and sheet = "Plan1".
#' - Cleans and normalizes column names with janitor::clean_names(), filters out header/footer artefacts, renames several common columns
#'   (e.g. endereco -> logradouro, nome* -> nome_instituicao, mail -> e_mail, fone -> telefone), and coerces common columns to character.
#' - Extracts the report date (ano, mes) from the filename and appends them as integer columns.
#' - Binds all monthly tables into a single tibble, replaces NA values in character/numeric/logical columns with sensible defaults, and
#'   reorders columns placing core identifier columns (cnpj, nome_instituicao, tipo, classe, categoria, filiacao, criterio_de_associacao)
#'   first followed by the remaining columns and the ano/mes columns at the end.
#' - Optionally writes the final result to output_csv using readr::write_csv.
#'
#' Note: The function depends on several packages (httr, utils, readxl, janitor, dplyr, tidyr, stringr, purrr, readr). The exact column
#' names produced depend on the input Excel files and the renaming heuristics implemented in the function.
#'
#' @return A tibble (data.frame) containing the combined and cleaned cooperatives data for the requested period. If no files are found
#' or none can be parsed, an empty tibble is returned.
#'
#' @examples
#' \dontrun{
#' # Download and parse default range, write CSV to working directory
#' df <- get_institutions()
#'
#' # Only parse files already present in "download" and don't write CSV
#' df <- get_institutions(download_files = FALSE, output_csv = "")
#' }
#'
#' @export
get_institutions <- function(
  start_date = "2007-09",
  end_date = "2024-09",
  out_dir = "data",
  output_csv = "coop_em_funcionamento_bacen.csv",
  download_files = TRUE,
  cleanup_zip = TRUE,
  verbose = FALSE
) {
  ##### POR ENQUANTO BAIXA APENAS COOPRERATIVAS EM FUNCIONAMENTO #####

  # build YYYYMM sequence
  datas <- format(
    seq(as.Date(start_date), as.Date(end_date), by = "month"),
    "%Y%m"
  )

  # ensure output dir exists
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # helper: download & unzip one month
  baixar_arquivos <- function(year_month) {
    url <- paste0(
      "https://www3.bcb.gov.br/informes/rest/instituicoesFuncionamento/cooperativa/",
      year_month,
      "COOPERATIVAS"
    )
    arquivo_zip <- file.path(
      out_dir,
      paste0("cooperativas_", year_month, ".zip")
    )

    resp <- httr::GET(url)
    if (httr::status_code(resp) == 200) {
      writeBin(httr::content(resp, "raw"), arquivo_zip)
      utils::unzip(arquivo_zip, exdir = out_dir)
      if (cleanup_zip) {
        unlink(arquivo_zip)
      }

      TRUE
    } else {
      FALSE
    }
  }

  if (download_files) {
    purrr::walk(datas, baixar_arquivos)
  }

  # list extracted files that contain "COOPERATIVAS" in the filename (case-insensitive)
  arquivos <- list.files(
    out_dir,
    pattern = "(?i)COOPERATIVAS",
    full.names = TRUE
  )

  importar_arquivo <- function(arquivo) {
    data_arquivo <- substr(basename(arquivo), 1, 6)

    # try reading xlsx first, then xls; keep skip = 9 and sheet = "Plan1" as in original
    df <- tryCatch(
      readxl::read_xlsx(arquivo, skip = 9, sheet = "Plan1"),
      error = function(e1) {
        tryCatch(
          readxl::read_xls(arquivo, skip = 9, sheet = "Plan1"),
          error = function(e2) NULL
        )
      }
    )

    if (is.null(df)) {
      return(NULL)
    }

    df <- df |>
      janitor::clean_names() |>
      dplyr::filter(
        !is.na(cnpj) &
          cnpj !=
            "FONTE: INSTITUIÇÕES FINANCEIRAS (O TEOR DAS INFORMAÇÕES É DE RESPONSABILIDADE DA RESPECTIVA INSTITUIÇÃO/EMPRESA, DE ACORDO COM A REGULAMENTAÇÃO EM VIGOR)"
      ) |>
      dplyr::rename_with(
        ~ dplyr::case_when(
          grepl("^endereco$", .) ~ "logradouro",
          grepl("^nome", .) ~ "nome_instituicao",
          grepl("mail", .) ~ "e_mail",
          grepl("endereco_eletronico", .) ~ "e_mail",
          grepl("enderecoeletronico", .) ~ "site",
          grepl("sitio", .) ~ "site",
          grepl("fone", .) ~ "telefone",
          grepl("^tipo", .) ~ "tipo",
          grepl("^classe", .) ~ "classe",
          grepl("^categ", .) ~ "categoria",
          grepl("assoc", .) ~ "criterio_de_associacao",
          TRUE ~ .
        )
      ) |>
      dplyr::mutate(
        cnpj = stringr::str_remove_all(cnpj, "\\."),
        cnpj = as.character(cnpj),
        cep = as.character(cep),
        telefone = as.character(telefone),
        data_base = data_arquivo
      ) |>
      tidyr::separate(
        data_base,
        into = c("ano", "mes"),
        sep = 4,
        convert = TRUE
      )

    df
  }

  cooperativas_list <- purrr::map(arquivos, importar_arquivo)
  cooperativas_final <- dplyr::bind_rows(cooperativas_list)

  # if no files were read, return empty tibble
  if (nrow(cooperativas_final) == 0) {
    return(cooperativas_final)
  }

  cooperativas_final <- cooperativas_final |>
    dplyr::select(
      cnpj,
      nome_instituicao,
      tipo,
      classe,
      categoria,
      filiacao,
      criterio_de_associacao,
      dplyr::everything(),
      ano,
      mes
    ) |>
    dplyr::mutate(
      dplyr::across(where(is.character), ~ dplyr::if_else(is.na(.), "", .)),
      dplyr::across(where(is.numeric), ~ dplyr::if_else(is.na(.), 0, .)),
      dplyr::across(where(is.logical), ~ dplyr::if_else(is.na(.), FALSE, .))
    )

  if (!is.null(output_csv) && nzchar(output_csv)) {
    readr::write_csv(cooperativas_final, output_csv)
  }

  cooperativas_final
}
