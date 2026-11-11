test_that("get_branches validates its arguments", {
  expect_error(get_branches(type = "XYZ", years = 2023, months = 12, out_dir = tempdir()) |> suppressWarnings(), "No valid type")
  expect_error(get_branches(years = 2023, months = 0, out_dir = tempdir()), "months")
})

test_that("tidy_branches returns an empty tibble when there are no folders", {
  dir <- withr::local_tempdir()
  expect_message(out <- tidy_branches(dir), "No branch folders")
  expect_equal(nrow(out), 0)
})

test_that("get_branches downloads and tidy_branches reads branches and posts", {
  skip_on_cran()
  skip_if_offline("www.bcb.gov.br")
  dir <- withr::local_tempdir()
  files <- get_branches(type = c("AGENCIAS", "POSTOS"), years = 2023, months = 12, out_dir = dir, verbose = FALSE)
  expect_true(all(!is.na(files$path)))
  deps <- tidy_branches(dir, verbose = FALSE)
  expect_setequal(unique(deps$type), c("AGENCIAS", "POSTOS"))
  expect_gt(sum(deps$type == "AGENCIAS"), 5000)
  expect_true(all(nchar(deps$cnpj) == 14))
})
