download_texto_normativo <- function(normative_data) {
  all_data <- data.frame()

  # Iterar sobre cada linha do data frame
  for (i in seq_len(nrow(normative_data))) {
    normativo_tipo <- normative_data$TipodoNormativoOWSCHCS[i]
    normativo_num <- normative_data$NumeroOWSNMBR[i]
    print(i)
    # Construir a URL com base no tipo de normativo
    if (
      normativo_tipo %in% c("Comunicado", "Ato de Diretor", "Ato do Presidente")
    ) {
      normativo_tipo_encoded <- URLencode(normativo_tipo, reserved = TRUE)
      site <- glue::glue(
        "https://www.bcb.gov.br/api/conteudo/app/normativos/exibeoutrasnormas?p1={normativo_tipo_encoded}&p2={normativo_num}"
      )
    } else {
      # Codificar o tipo de normativo para URL
      normativo_tipo_encoded <- URLencode(normativo_tipo, reserved = TRUE)
      site <- glue::glue(
        "https://www.bcb.gov.br/api/conteudo/app/normativos/exibenormativo?p1={normativo_tipo_encoded}&p2={normativo_num}"
      )
    }

    # Fazer a requisição e obter o conteúdo JSON
    json_file <- httr::GET(site) |>
      httr::content(as = "text") |>
      jsonlite::fromJSON() |>
      purrr::pluck("conteudo") |>
      as.data.frame() |>
      dplyr::mutate(
        Assunto = xml2::xml_text(xml2::read_html(paste0(
          "<x>",
          Assunto,
          "</x>"
        ))),
        Assunto = stringr::str_replace_all(Assunto, "\n", " "),
        Assunto = stringr::str_squish(Assunto),
        #Texto = stringr::str_replace_all(Texto, "<[^>]+>", ""),
        Texto = xml2::xml_text(xml2::read_html(paste0("<x>", Texto, "</x>"))),
        Texto = stringr::str_replace_all(Texto, "\n", " "),
        Texto = stringr::str_squish(Texto)
      )

    all_data <- dplyr::bind_rows(all_data, json_file)
  }

  return(all_data)
}


normative_txt <- download_texto_normativo(normative_data)
