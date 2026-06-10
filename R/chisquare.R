#' Compute chi-square scores for ADGA
#'
#' For each group-lemma combination, computes standardised residuals from a
#' contingency table comparing token frequencies inside versus outside the
#' group. Terms with high positive residuals are overrepresented in the group.
#'
#' @param annotated A tibble returned by [adga_preprocess()].
#'
#' @return A tibble with columns `group`, `lemma`, `class_n`, `expected`,
#'   `std_res`, and `chi_rank` (rank within each group, 1 = most indicative).
#' @export
#'
#' @examples
#' \dontrun{
#' chisq <- adga_chisquare(annotated)
#' }
adga_chisquare <- function(annotated) {

  word_counts <- annotated |>
    dplyr::count(group, lemma, name = "class_n")

  word_counts |>
    dplyr::group_by(group) |>
    dplyr::mutate(row_total = sum(class_n)) |>
    dplyr::ungroup() |>
    dplyr::group_by(lemma) |>
    dplyr::mutate(col_total = sum(class_n)) |>
    dplyr::ungroup() |>
    dplyr::mutate(grand_total = sum(class_n)) |>
    dplyr::mutate(
      expected = (row_total * col_total) / grand_total,
      std_res  = (class_n - expected) / sqrt(expected)
    ) |>
    dplyr::group_by(group) |>
    dplyr::arrange(dplyr::desc(std_res), .by_group = TRUE) |>
    dplyr::mutate(chi_rank = dplyr::row_number()) |>
    dplyr::ungroup()
}
