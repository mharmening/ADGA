#' Remove stopwords from ADGA metric tables
#'
#' Removes stopwords from the output of [adga_tfidf()], [adga_chisquare()],
#' and [adga_wordscore()]. Stopwords are loaded via the `stopwords` package.
#'
#' @param tfidf A tibble returned by [adga_tfidf()].
#' @param chisquare A tibble returned by [adga_chisquare()].
#' @param wordscore A tibble returned by [adga_wordscore()].
#' @param language Language code passed to [stopwords::stopwords()],
#'   e.g. `"no"` for Norwegian. Defaults to `"no"`.
#' @param source Stopword source passed to [stopwords::stopwords()].
#'   Defaults to `"snowball"`.
#'
#' @return A named list with elements `tfidf`, `chisquare`, and `wordscore`,
#'   each with stopwords removed.
#' @export
#'
#' @examples
#' \dontrun{
#' cleaned <- adga_remove_stopwords(tfidf, chisquare, wordscore, language = "no")
#' tfidf     <- cleaned$tfidf
#' chisquare <- cleaned$chisquare
#' wordscore <- cleaned$wordscore
#' }
adga_remove_stopwords <- function(tfidf,
                                  chisquare,
                                  wordscore,
                                  language = "no",
                                  source   = "snowball") {

  sw <- tibble::tibble(
    lemma = stopwords::stopwords(language = language, source = source)
  )

  list(
    tfidf     = dplyr::anti_join(tfidf,     sw, by = "lemma"),
    chisquare = dplyr::anti_join(chisquare, sw, by = "lemma"),
    wordscore = dplyr::anti_join(wordscore, sw, by = "lemma")
  )
}
