#' Compute wordscore values for ADGA
#'
#' For each group-lemma combination, computes a wordscore based on conditional
#' probabilities: how exclusively a term belongs to a group versus the rest of
#' the corpus. Scores range from -1 (exclusively outside the group) to +1
#' (exclusively inside the group). Laplace smoothing is applied to avoid
#' division by zero.
#'
#' @param annotated A tibble returned by [adga_preprocess()].
#' @param smoothing Numeric. Laplace smoothing constant added to all counts.
#'   Defaults to `0.5`.
#'
#' @return A tibble with columns `group`, `lemma`, `wordscore`, and
#'   `wordscore_rank` (rank within each group, 1 = most indicative).
#' @export
#'
#' @examples
#' \dontrun{
#' ws <- adga_wordscore(annotated)
#' }
adga_wordscore <- function(annotated, smoothing = 0.5) {

  counts <- annotated |>
    dplyr::count(group, lemma, name = "n") |>
    dplyr::mutate(n = n + smoothing)

  totals_by_group <- counts |>
    dplyr::group_by(group) |>
    dplyr::summarise(total_group = sum(n), .groups = "drop")

  totals_by_lemma <- counts |>
    dplyr::group_by(lemma) |>
    dplyr::summarise(total_lemma = sum(n), .groups = "drop")

  grand_total <- sum(totals_by_group$total_group)

  counts |>
    dplyr::left_join(totals_by_group, by = "group") |>
    dplyr::left_join(totals_by_lemma, by = "lemma") |>
    dplyr::mutate(
      no_x_count         = total_lemma - n,
      total_other_tokens = grand_total - total_group,
      has_x_Fwr          = n / total_group,
      no_x_Fwr           = no_x_count / total_other_tokens,
      wordscore          = (has_x_Fwr - no_x_Fwr) / (has_x_Fwr + no_x_Fwr)
    ) |>
    dplyr::group_by(group) |>
    dplyr::arrange(dplyr::desc(wordscore), .by_group = TRUE) |>
    dplyr::mutate(wordscore_rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::select(group, lemma, wordscore, wordscore_rank)
}
