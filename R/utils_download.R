# Internal helpers shared by get_estban() and get_branches()

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
