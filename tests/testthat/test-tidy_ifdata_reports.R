test_that("empty or NULL input returns an empty tibble with a message", {
  expect_message(
    res <- tidy_ifdata_reports(tibble::tibble()),
    "No data to tidy"
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0)

  expect_message(res_null <- tidy_ifdata_reports(NULL), "No data to tidy")
  expect_equal(nrow(res_null), 0)
})

test_that("missing required columns give an informative error", {
  bad <- data.frame(cod_inst = 1, saldo = 10)
  expect_error(
    tidy_ifdata_reports(bad),
    "missing required column\\(s\\): nome_coluna"
  )
})

test_that("valid input is pivoted to one row per institution", {
  raw <- data.frame(
    cod_inst = c(1, 1, 2, 2),
    grupo = "g",
    conta = "c",
    nome_coluna = c(
      "Ativo Total",
      "Lucro Liquido",
      "Ativo Total",
      "Lucro Liquido"
    ),
    saldo = c(100, 10, 200, 20)
  )
  res <- tidy_ifdata_reports(raw)
  expect_equal(nrow(res), 2)
  expect_named(res, c("cnpj", "ativo_total", "lucro_liquido"))
})
