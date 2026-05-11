#' Preprocess text data for ADGA
#'
#' Annotates a text column using UDPipe (lemmatization, POS-tagging,
#' tokenization), filters by part-of-speech tags, and merges metadata
#' back onto the token table.
#'
#' @param data A data frame containing the text to annotate.
#' @param text_col Name of the column containing the text (as string).
#' @param id_col Name of the column containing a unique response ID (as string).
#' @param group_col Name of the column to use as the ADGA group variable (as string).
#' @param language UDPipe language model string, e.g. `"norwegian-nynorsk"`.
#' @param pos_filter Character vector of UPOS tags to keep.
#'   Defaults to `c("NOUN", "ADJ", "ADV", "VERB", "PROPN")`.
#' @param carry_cols Character vector of additional columns from `data` to
#'   carry over to the token table (e.g. sociodemographics). Defaults to `NULL`.
#'
#' @return A tibble of annotated tokens with lemmas, POS tags, and metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' annotated <- adga_preprocess(
#'   data       = my_data,
#'   text_col   = "openanswer",
#'   id_col     = "responseid",
#'   group_col  = "humanmade",
#'   language   = "norwegian-nynorsk"
#' )
#' }
adga_preprocess <- function(data,
                            text_col,
                            id_col,
                            group_col,
                            language,
                            pos_filter = c("NOUN", "ADJ", "ADV", "VERB", "PROPN"),
                            carry_cols = NULL) {

  # download and load udpipe model
  lang_model <- udpipe::udpipe_download_model(language = language)
  udmodel    <- udpipe::udpipe_load_model(file = lang_model$file_model)

  # annotate
  x <- udpipe::udpipe_annotate(udmodel, x = data[[text_col]])
  x <- tibble::as_tibble(as.data.frame(x))

  # unique doc ids -> row crosswalk
  uids <- unique(x$doc_id)

  if (length(uids) != nrow(data)) {
    warning("Number of unique doc_ids does not match nrow(data). ",
            "Mapping by row order — please verify.")
  }

  crosswalk <- tibble::tibble(doc_id = uids, .row = seq_along(uids))

  meta <- data |>
    dplyr::mutate(.row = dplyr::row_number()) |>
    dplyr::select(.row, dplyr::all_of(c(id_col, group_col, carry_cols))) |>
    dplyr::rename(id = dplyr::all_of(id_col))

  lookup <- dplyr::left_join(crosswalk, meta, by = ".row") |>
    dplyr::select(-.row)

  annotated <- dplyr::left_join(x, lookup, by = "doc_id", multiple = "all")

  # rename group column
  annotated <- annotated |>
    dplyr::rename(group = dplyr::all_of(group_col))

  # pos filter + alphabetic filter
  annotated <- annotated |>
    dplyr::filter(
      upos %in% pos_filter,
      stringr::str_detect(lemma, "^[[:alpha:]]")
    ) |>
    dplyr::mutate(lemma = stringr::str_trim(stringr::str_to_lower(lemma)))

  annotated
}




