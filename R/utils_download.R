# Internal helpers shared by get_estban() and get_bank_branches()

check_years_months <- function(years, months) {
  if (!is.numeric(years) || anyNA(years)) {
    stop("years must be a numeric vector, e.g. c(2020:2023).", call. = FALSE)
  }
  if (!is.numeric(months) || anyNA(months) || any(months < 1 | months > 12)) {
    stop("months must be a numeric vector with values between 1 and 12, e.g. c(1:12).", call. = FALSE)
  }
  invisible(TRUE)
}

check_out_dir <- function(out_dir) {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(out_dir) || file.access(out_dir, mode = 2) != 0) {
    stop(
      "Cannot write to out_dir: ", normalizePath(out_dir, mustWork = FALSE),
      ". Check the path and its permissions.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

bacen_user_agent <- function() {
  httr::user_agent("Mozilla/5.0 (compatible; bacenR; +https://github.com/rtheodoro/bacenR)")
}

# Download url to dest; TRUE when the request returned status 200
download_bacen <- function(url, dest) {
  resp <- tryCatch(
    httr::GET(url, bacen_user_agent(), httr::write_disk(dest, overwrite = TRUE)),
    error = function(e) NULL
  )
  !is.null(resp) && httr::status_code(resp) == 200 && file.exists(dest) && file.size(dest) > 0
}

is_zip <- function(path) {
  file.exists(path) && identical(readBin(path, "raw", 4L), as.raw(c(0x50, 0x4b, 0x03, 0x04)))
}

# The Bacen website serves error pages with status 200; keep only real ESTBAN files
is_estban_csv <- function(path) {
  file.exists(path) && file.size(path) > 0 &&
    any(grepl("DATA_BASE", readLines(path, n = 5L, warn = FALSE), useBytes = TRUE))
}

# Documents listed by the Bacen website API for a folder (e.g. "/agencias")
list_bacen_documents <- function(folder) {
  resp <- tryCatch(
    httr::GET(
      "https://www.bcb.gov.br/api/servico/sitebcb/Documentos/byListGuid",
      query = list(
        tronco = "estabilidadefinanceira",
        guidLista = "50f14992-f03c-4c47-8576-b96a65565485",
        ordem = "DataDocumento desc",
        pasta = folder
      ),
      bacen_user_agent()
    ),
    error = function(e) NULL
  )
  empty <- data.frame(year_month = character(), title = character(), url = character())
  if (is.null(resp) || httr::status_code(resp) != 200) {
    return(empty)
  }
  content <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))$conteudo
  if (is.null(content) || NROW(content) == 0) {
    return(empty)
  }
  data.frame(
    year_month = document_year_month(content$Titulo),
    title = content$Titulo,
    url = paste0("https://www.bcb.gov.br", content$Url),
    stringsAsFactors = FALSE
  )
}

# "12/2024", "Dezembro/2024" or "Dezembro de 2024" -> "202412"
document_year_month <- function(title) {
  year <- stringr::str_extract(title, "[0-9]{4}")
  month <- suppressWarnings(as.integer(stringr::str_match(title, "([0-9]{1,2})/[0-9]{4}")[, 2]))
  month_names <- c(
    "janeiro", "fevereiro", "marco", "abril", "maio", "junho",
    "julho", "agosto", "setembro", "outubro", "novembro", "dezembro"
  )
  word <- stringr::str_extract(tolower(iconv(title, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")), "[a-z]+")
  month <- ifelse(is.na(month), match(word, month_names), month)
  ifelse(is.na(year) | is.na(month), NA_character_, sprintf("%s%02d", year, as.integer(month)))
}
