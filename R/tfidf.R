#' Compute tf-idf scores for ADGA
#'
#' For each group-lemma combination, computes a modified tf-idf score that
#' measures how indicative a term is for a given group relative to the rest
#' of the corpus.
#'
#' @param annotated A tibble returned by [adga_preprocess()].
#'
#' @return A tibble with columns `group`, `lemma`, `n`, `tf`, `idf`,
#'   `tf_idf`, and `tf_idf_rank` (rank within each group, 1 = most indicative).
#' @export
#'
#' @examples
#' \dontrun{
#' tfidf <- adga_tfidf(annotated)
#' }
adga_tfidf <- function(annotated) {

  annotated |>
    dplyr::count(group, lemma, name = "n") |>
    tidytext::bind_tf_idf(lemma, group, n) |>
    dplyr::group_by(group) |>
    dplyr::arrange(dplyr::desc(tf_idf), .by_group = TRUE) |>
    dplyr::mutate(tf_idf_rank = dplyr::row_number()) |>
    dplyr::ungroup()
}
