#' Baixa texto de normativos do Banco Central (BCB) a partir de um data.frame de referências
#'
#' Para cada linha do data.frame de entrada, monta a URL apropriada à API de normativos
#' do BCB, realiza a requisição, extrai o conteúdo retornado e faz limpeza básica dos
#' campos de texto (Assunto e Texto). Retorna um data.frame com todos os conteúdos agregados.
#'
#' @param normative_data data.frame. Data.frame com pelo menos as colunas:
#'   - TipodoNormativoOWSCHCS: tipo do normativo (ex.: "Comunicado", "Ato de Diretor", etc.)
#'   - NumeroOWSNMBR: número/identificador do normativo a ser consultado.
#'   - É o resultado de uma consulta prévia (p.ex., via \code{download_legislacao}).
#'
#' @details
#' A função realiza os seguintes passos:
#' 1. Itera por cada linha de \code{normative_data} (imprime o índice da iteração para acompanhamento).
#' 2. Codifica o tipo do normativo para uso em URL (usando \code{URLencode(..., reserved = TRUE)}).
#' 3. Seleciona a rota da API conforme o tipo:
#'    - para "Comunicado", "Ato de Diretor" e "Ato do Presidente" usa o endpoint de
#'      outras normas (\code{exibeoutrasnormas});
#'    - para os demais, usa o endpoint de normativo padrão (\code{exibenormativo}).
#' 4. Faz a requisição HTTP (via \code{httr::GET}) e converte o JSON retornado em data.frame.
#' 5. Extrai e limpa os campos textuais:
#'    - converte HTML em texto plano para os campos \code{Assunto} e \code{Texto} (usando \code{xml2::read_html} + \code{xml2::xml_text}),
#'    - remove quebras de linha e excesso de espaços (\code{stringr::str_replace_all}, \code{stringr::str_squish}).
#' 6. Agrega cada resultado em um data.frame final retornado pela função.
#'
#' @return data.frame com os registros retornados pela API para cada referência em \code{normative_data},
#'   incluindo os campos extraídos e pós-processados (p.ex. \code{Assunto}, \code{Texto}).
#'
#' @note
#' - A função pressupõe que a API do BCB retorna um campo \code{conteudo} com os campos esperados.
#' - Erros de requisição ou alterações na estrutura da API podem causar falhas; trate possíveis exceções
#'   ao redor da chamada se necessário.
#' - Pacotes usados internamente: \code{httr}, \code{jsonlite}, \code{xml2}, \code{stringr}, \code{dplyr}, \code{glue}, \code{purrr}.
#'
#' @examples
#' \dontrun{
#' # Exemplo mínimo
#' normative_data <- data.frame(
#'   TipodoNormativoOWSCHCS = c("Comunicado", "Circular"),
#'   NumeroOWSNMBR = c("123", "456"),
#'   stringsAsFactors = FALSE
#' )
#' normative_txt <- download_texto_normativo(normative_data)
#' }
#'
#' @export
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
