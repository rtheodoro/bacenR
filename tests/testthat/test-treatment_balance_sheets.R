# test_that("treat balance sheet of two types of institution", {
#   local({
#     download_balance_sheets(
#       instituicao = c("COOPERATIVAS", "BANCOS"),
#       meses = c(6, 12),
#       first_year = 2022,
#       final_year = as.numeric(format(Sys.time(), "%Y")) - 2,
#       overwrite = TRUE
#     )
#     treatment_balance_sheets(
#       path_raw = "data_raw",
#       out_dir = "data",
#       doc_filter = 4010,
#       save = TRUE
#     )
#   })
#   expect_true(
#     fs::file_exists("data/4010_cooperativas.csv") &
#       fs::file_exists("data/4010_bancos.csv")
#   )
# })

# # Delete directory after tests
# fs::dir_delete("data_raw")
# fs::dir_delete("data")

# test_that("treat balance sheet of two types of institutions for 2 months", {
#   local({
#     download_balance_sheets(
#       instituicao = c("COOPERATIVAS", "BANCOS"),
#       meses = c(6, 12),
#       first_year = 2022,
#       final_year = as.numeric(format(Sys.time(), "%Y")) - 2,
#       overwrite = TRUE
#     )
#     treatment_balance_sheets(
#       path_raw = "data_raw",
#       out_dir = "data",
#       doc_filter = 4060,
#       output_filename = function(type) paste0("bal_", tolower(type), ".csv"),
#       save = TRUE
#     )
#   })
#   expect_true(
#     fs::file_exists("data/bal_cooperativas.csv") &
#       fs::file_exists("data/bal_bancos.csv")
#   )
# })

# # Delete directory after tests
# fs::dir_delete("data_raw")
# fs::dir_delete("data")

download_balance_sheets(
  instituicao = c("BANCOS", "COOPERATIVAS"),
  meses = 12,
  first_year = 2003,
  final_year = 2023,
  out_dir = "data_raw",
  overwrite = FALSE
)


treatment_balance_sheets(
  path_raw = "data_raw",
  out_dir = "data",
  doc_filter = 4010,
  save = TRUE,
  output_filename = NULL
)
