test_that("tidy_estban reads ESTBAN files with the #DATA_BASE header", {
  dir <- withr::local_tempdir()
  lines <- c(
    "ESTBAN - Estatistica Bancaria Mensal por Municipio",
    "#DATA_BASE;UF;CODMUN_IBGE;MUNICIPIO;CNPJ;NOME_INSTITUICAO;AGEN_ESPERADAS;AGEN_PROCESSADAS;VERBETE_160_OPERACOES_DE_CREDITO",
    "202312;SP;3550308;SAO PAULO;00000000;BCO DO BRASIL S.A.;10;10;1.234,50",
    "202312;SP;3550308;SAO PAULO;00360305;CAIXA ECONOMICA FEDERAL;5;4;100,00"
  )
  writeLines(iconv(lines, to = "latin1"), file.path(dir, "202312_ESTBAN.csv"), useBytes = TRUE)

  out <- tidy_estban(dir, verbose = FALSE)
  expect_equal(nrow(out), 2)
  expect_equal(out$agencias, c(10, 4))
  expect_equal(out$verbete_160_operacoes_de_credito, c(1234.5, 100))
  expect_equal(unique(out$year_month), "202312")
  expect_true("data_base" %in% names(out))
})

test_that("tidy_estban returns an empty tibble when there are no files", {
  dir <- withr::local_tempdir()
  expect_message(out <- tidy_estban(dir), "No ESTBAN files")
  expect_equal(nrow(out), 0)
})

test_that("get_estban validates its arguments", {
  expect_error(get_estban(years = "2023", months = 12, out_dir = tempdir()), "years")
  expect_error(get_estban(years = 2023, months = 13, out_dir = tempdir()), "months")
})

test_that("get_estban downloads a file", {
  skip_on_cran()
  skip_if_offline("www.bcb.gov.br")
  dir <- withr::local_tempdir()
  paths <- get_estban(years = 2023, months = 12, out_dir = dir, verbose = FALSE)
  expect_length(paths, 1)
  expect_gt(nrow(tidy_estban(dir, verbose = FALSE)), 1000)
})
