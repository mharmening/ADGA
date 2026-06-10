#' Run the ADGA voting model to select keywords
#'
#' Combines tf-idf, chi-square, and wordscore rankings through an iterative
#' voting procedure. Starting from the top `i_start` words per measure, a term
#' is added to the dictionary if it appears in at least two of the three
#' ranked lists. The lists are expanded by one each iteration until the target
#' dictionary size is reached.
#'
#' @param tfidf A tibble returned by [adga_tfidf()].
#' @param chisquare A tibble returned by [adga_chisquare()].
#' @param wordscore A tibble returned by [adga_wordscore()].
#' @param goal Integer. Target number of keywords per group. Defaults to `1000`.
#' @param i_start Integer. Starting size of each ranked input list. Defaults to `50`.
#' @param i_stop Integer. Maximum input list size before stopping. Defaults to `2500`.
#'
#' @return A tibble with one row per group, containing columns `group`,
#'   `i_final` (list size at which goal was reached), `n_keywords`, and
#'   `keywords` (a list-column of tibbles with lemma, votes, and measure flags).
#' @export
#'
#' @examples
#' \dontrun{
#' results <- adga_vote(tfidf, chisquare, wordscore, goal = 100)
#' }
adga_vote <- function(tfidf,
                      chisquare,
                      wordscore,
                      goal    = 1000,
                      i_start = 50,
                      i_stop  = 2500) {

  groups  <- sort(unique(tfidf$group))
  results <- vector("list", length(groups))

  for (g_idx in seq_along(groups)) {
    g <- groups[g_idx]
    message("Processing group: ", g)

    tfidf_g <- tfidf |>
      dplyr::filter(group == g) |>
      dplyr::select(lemma, tf_idf_rank)

    chi_g <- chisquare |>
      dplyr::filter(group == g) |>
      dplyr::select(lemma, chi_rank)

    ws_g <- wordscore |>
      dplyr::filter(group == g) |>
      dplyr::select(lemma, wordscore_rank)

    combo <- tfidf_g |>
      dplyr::full_join(chi_g, by = "lemma") |>
      dplyr::full_join(ws_g,  by = "lemma") |>
      dplyr::mutate(
        dplyr::across(dplyr::ends_with("_rank"),
                      \(x) tidyr::replace_na(x, Inf))
      )

    accepted <- tibble::tibble(
      lemma        = character(),
      in_tfidf     = integer(),
      in_chi       = integer(),
      in_wordscore = integer(),
      vote         = integer(),
      i_entered    = integer()
    )

    for (i in i_start:i_stop) {
      newly_voted <- combo |>
        dplyr::filter(!lemma %in% accepted$lemma) |>
        dplyr::mutate(
          in_tfidf     = as.integer(tf_idf_rank    <= i),
          in_chi       = as.integer(chi_rank        <= i),
          in_wordscore = as.integer(wordscore_rank  <= i),
          vote         = in_tfidf + in_chi + in_wordscore
        ) |>
        dplyr::filter(vote > 1) |>
        dplyr::mutate(i_entered = i)

      if (nrow(newly_voted) > 0) {
        accepted <- dplyr::bind_rows(
          accepted,
          newly_voted |>
            dplyr::select(lemma, in_tfidf, in_chi, in_wordscore, vote, i_entered)
        )
      }

      if (nrow(accepted) >= goal || nrow(accepted) == nrow(combo)) {
        message("  Reached ", nrow(accepted), " keywords at i = ", i)
        break
      }
    }

    results[[g_idx]] <- tibble::tibble(
      group      = g,
      i_final    = i,
      n_keywords = nrow(accepted),
      keywords   = list(accepted)
    )
  }

  dplyr::bind_rows(results)
}
