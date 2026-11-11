test_that("document titles are converted to year-month", {
  expect_equal(
    bacenR:::document_year_month(c("12/2024", "Dezembro/2010", "Setembro de 2007", "sem data")),
    c("202412", "201012", "200709", NA)
  )
})

test_that("tidy_bank_branches returns an empty tibble when there are no folders", {
  dir <- withr::local_tempdir()
  expect_message(out <- tidy_bank_branches(dir), "No branch folders")
  expect_equal(nrow(out), 0)
})

test_that("get_bank_branches validates its arguments", {
  expect_error(get_bank_branches(years = 2023, months = 0, out_dir = tempdir()), "months")
})

test_that("get_bank_branches downloads and tidy_bank_branches reads a month", {
  skip_on_cran()
  skip_if_offline("www.bcb.gov.br")
  dir <- withr::local_tempdir()
  docs <- get_bank_branches(years = 2023, months = 12, out_dir = dir, verbose = FALSE)
  expect_true(any(!is.na(docs$path)))
  branches <- tidy_bank_branches(dir, verbose = FALSE)
  expect_gt(nrow(branches), 10000)
  expect_true(all(nchar(branches$cnpj) == 14))
})
