primeiroano <- 1993
anomaisrecente <- as.numeric(format(Sys.time(), "%Y")) - 1

csv_coop_completo_1993a2022 <- data.frame()

load_and_treat_balanco <- function(
  year,
  month = "12",
  skip,
  data_col_name,
  nome_col_name
) {
  if (!grepl("^(0[1-9]|1[0-2])$", month)) {
    stop(sprintf(
      "Invalid month '%s'. Expected two-digit month between '01' and '12'.",
      month
    ))
  }

  csv_i <-
    data.table::fread(
      glue::glue('data_raw/{year}{month}COOPERATIVAS.CSV'),
      sep = ";",
      skip = skip
    )

  csv_i |>
    dplyr::filter(DOCUMENTO == 4010) |>
    dplyr::select(
      tidyselect::all_of(
        c("CNPJ", data_col_name, nome_col_name, "CONTA", "SALDO")
      )
    )
}

# Configuração dos parâmetros do balanço por faixa de ano
balanco_config <- data.frame(
  start_year = c(1993, 1994, 2010),
  end_year = c(1993, 2009, Inf),
  skip = c(4, 3, 3),
  data_col_name = c("#DATA_BASE", "DATA", "#DATA_BASE"),
  nome_col_name = c("NOME_INSTITUICAO", "NOME INSTITUICAO", "NOME_INSTITUICAO"),
  stringsAsFactors = FALSE
)

get_balanco_params <- function(year, config_df = balanco_config) {
  idx <- which(year >= config_df$start_year & year <= config_df$end_year)
  if (length(idx) != 1L) {
    stop(sprintf("No unique balanço configuration found for year %s.", year))
  }
  row <- config_df[idx, , drop = FALSE]
  list(
    skip = row$skip,
    data_col_name = row$data_col_name,
    nome_col_name = row$nome_col_name
  )
}

for (i in primeiroano:anomaisrecente) {
  print(glue::glue("Carregando Balanço do ano {i}"))

  params <- get_balanco_params(i)
  csv_i <- load_and_treat_balanco(
    year = i,
    skip = params$skip,
    data_col_name = params$data_col_name,
    nome_col_name = params$nome_col_name
  )

  print(glue::glue("Tratando Balanço do ano {i}"))
  names(csv_i)[1:3] <- c('cnpj', 'ano', 'razao_social')

  csv_i <- csv_i |>
    tidyr::pivot_wider(
      id_cols = c(cnpj, ano, razao_social),
      names_from = CONTA,
      values_from = SALDO
    )

  print(glue::glue("Unificando Balanço do ano {i}"))
  csv_coop_completo_1993a2022 <-
    merge(csv_coop_completo_1993a2022, csv_i, all = TRUE)

  rm(csv_i)
}


# Converte colunas numéricas de csv_coop_completo_1993a2022 de vírgula para ponto como separador decimal
csv_coop_completo_1993a2022 <- csv_coop_completo_1993a2022 |>
  dplyr::mutate_at(
    dplyr::vars(dplyr::matches("[0-9]")),
    ~ as.numeric(gsub(",", ".", .))
  )


write.csv(
  csv_coop_completo_1993a2022,
  file = glue::glue(
    "data/balanco_coop_cred_{primeiroano}a{anomaisrecente}_4010.csv"
  ),
  row.names = FALSE
)
