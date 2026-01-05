#' Download normative texts from the Brazil Central Bank (BCB) from a reference data.frame
#'
#' For each row of the input data.frame, constructs the appropriate URL for the BCB
#' normatives API, performs the request, extracts the returned content and applies
#' basic cleaning to the text fields (Assunto and Texto). Returns a data.frame
#' with all aggregated contents.
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' @param normative_data data.frame. A data.frame with at least the columns:
#'   - TipodoNormativoOWSCHCS: type of the normative (e.g., "Comunicado", "Ato de Diretor", etc.)
#'   - NumeroOWSNMBR: number/identifier of the normative to be queried.
#'   - Typically the result of a prior query (e.g., via \code{download_normative_data}).
#'
#' @details
#' The function performs the following steps:
#' 1. Iterates over each row of \code{normative_data} (prints the iteration index for monitoring).
#' 2. Encodes the normative type for URL usage (using \code{URLencode(..., reserved = TRUE)}).
#' 3. Chooses the API route depending on the type:
#'    - for "Comunicado", "Ato de Diretor" and "Ato do Presidente" it uses the other-norms endpoint (\code{exibeoutrasnormas});
#'    - for the rest, it uses the standard normative endpoint (\code{exibenormativo}).
#' 4. Makes the HTTP request (via \code{httr::GET}) and converts the returned JSON into a data.frame.
#' 5. Extracts and cleans textual fields:
#'    - converts HTML to plain text for the fields \code{Assunto} and \code{Texto} (using \code{xml2::read_html} + \code{xml2::xml_text}),
#'    - removes line breaks and extra spaces (\code{stringr::str_replace_all}, \code{stringr::str_squish}).
#' 6. Aggregates each result into a final data.frame returned by the function.
#'
#' @return data.frame with the records returned by the API for each reference in \code{normative_data},
#'   including the extracted and post-processed fields (e.g., \code{Assunto}, \code{Texto}).
#'
#' @note
#' - The function assumes the BCB API returns a \code{conteudo} field with the expected fields.
#' - Request errors or changes in the API structure may cause failures; consider handling exceptions around the call if needed.
#' - Packages used internally: \code{httr}, \code{jsonlite}, \code{xml2}, \code{stringr}, \code{dplyr}, \code{glue}, \code{purrr}.
#'
#' @examples
#' \dontrun{
#' # Minimal example
#' normative_data <- data.frame(
#'   TipodoNormativoOWSCHCS = c("Comunicado", "Circular"),
#'   NumeroOWSNMBR = c("123", "456"),
#'   stringsAsFactors = FALSE
#' )
#' normative_txt <- download_normative_txt(normative_data)
#' }
#'
#' @export

download_normative_txt <- function(normative_data) {
  all_data <- data.frame()

  # Iterate over each row of the input data.frame
  all_data <- purrr::pmap_dfr(
    normative_data,
    function(TipodoNormativoOWSCHCS, NumeroOWSNMBR, ...) {
      normativo_tipo <- TipodoNormativoOWSCHCS
      normativo_num <- NumeroOWSNMBR

      normativo_tipo_encoded <- URLencode(normativo_tipo, reserved = TRUE)
      site <- if (
        normativo_tipo %in%
          c("Comunicado", "Ato de Diretor", "Ato do Presidente")
      ) {
        glue::glue(
          "https://www.bcb.gov.br/api/conteudo/app/normativos/exibeoutrasnormas?p1={normativo_tipo_encoded}&p2={normativo_num}"
        )
      } else {
        glue::glue(
          "https://www.bcb.gov.br/api/conteudo/app/normativos/exibenormativo?p1={normativo_tipo_encoded}&p2={normativo_num}"
        )
      }

      httr::GET(site) |>
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
          Texto = xml2::xml_text(xml2::read_html(paste0("<x>", Texto, "</x>"))),
          Texto = stringr::str_replace_all(Texto, "\n", " "),
          Texto = stringr::str_squish(Texto)
        )
    }
  )

  return(all_data)
}
