#' Compute ADGA diagnostics
#'
#' Returns two diagnostic summaries:
#' - **K/URW ratio**: keywords selected relative to unique reference words
#'   available per group. A high ratio indicates elevated noise risk.
#' - **Measure contribution**: the share of keywords for which each of the
#'   three measures (tf-idf, chi-square, wordscore) contributed a vote.
#'
#' @param vote_result A tibble returned by [adga_vote()].
#' @param annotated A tibble returned by [adga_preprocess()].
#'
#' @return A tibble with one row per group containing `group`, `n_keywords`,
#'   `unique_ref_words`, `k_urw`, `pct_tfidf`, `pct_chi`, and `pct_wordscore`.
#' @export
#'
#' @examples
#' \dontrun{
#' diag <- adga_diagnostics(vote_result, annotated)
#' }
adga_diagnostics <- function(vote_result, annotated) {

  # unique reference words per group
  urw <- annotated |>
    dplyr::group_by(group) |>
    dplyr::summarise(unique_ref_words = dplyr::n_distinct(lemma), .groups = "drop") |>
    dplyr::mutate(group = as.character(group))

  # measure contribution per group
  contribution <- purrr::map_dfr(seq_len(nrow(vote_result)), function(r) {
    kw <- vote_result$keywords[[r]]
    tibble::tibble(
      group         = as.character(vote_result$group[r]),
      n_keywords    = nrow(kw),
      pct_tfidf     = mean(kw$in_tfidf)     * 100,
      pct_chi       = mean(kw$in_chi)       * 100,
      pct_wordscore = mean(kw$in_wordscore) * 100
    )
  })

  # join and compute K/URW
  contribution |>
    dplyr::left_join(urw, by = "group") |>
    dplyr::mutate(k_urw = n_keywords / unique_ref_words) |>
    dplyr::select(group, n_keywords, unique_ref_words, k_urw,
                  pct_tfidf, pct_chi, pct_wordscore)
}
