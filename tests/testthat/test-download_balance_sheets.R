test_that("create directory, download files and extract them, for a range of years", {
  download_balance_sheets(
    instituicao = "COOPERATIVAS",
    meses = 12,
    first_year = 2022,
    final_year = as.numeric(format(Sys.time(), "%Y")) - 2,
    out_dir = "data_raw",
    overwrite = FALSE
  )
  expect_true(fs::file_exists("data_raw/202312COOPERATIVAS.CSV"))
})


test_that("Check if directory exists, download files and extract them, for multiple institutions", {
  download_balance_sheets(
    instituicao = c("COOPERATIVAS", "BANCOS"),
    meses = 12,
    first_year = 2022,
    final_year = as.numeric(format(Sys.time(), "%Y")) - 2,
    out_dir = "data_raw",
    overwrite = FALSE
  )
  expect_true(
    fs::file_exists("data_raw/202312COOPERATIVAS.CSV") &
      fs::file_exists("data_raw/202312BANCOS.CSV")
  )
})


test_that("Check if overwrite files and download multiple months", {
  download_balance_sheets(
    instituicao = c("COOPERATIVAS", "BANCOS"),
    meses = c(6, 12),
    first_year = 2022,
    final_year = as.numeric(format(Sys.time(), "%Y")) - 2,
    out_dir = "data_raw",
    overwrite = TRUE
  )
  expect_true(
    fs::file_exists("data_raw/202312COOPERATIVAS.CSV") &
      fs::file_exists("data_raw/202306BANCOS.CSV")
  )
})

# Delete directory after tests
fs::dir_delete("data_raw")
