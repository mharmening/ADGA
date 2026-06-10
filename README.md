# ADGA

The **A**utomatic **D**ictionary **G**eneration **A**pproach is an R package
for transparent, automated keyword selection from labeled reference data.
Given a corpus of texts where concepts of interest are already labeled, ADGA
identifies the most distinctive terms for each label by combining three
established measures of word relevance — tf-idf, chi-square, and wordscores —
in a simple voting model.

ADGA is introduced in:

> Block, S., Harmening, M., & Nyhuis, D. (*forthcoming*). Automatic Dictionary
> Generation for Social Science Text Analysis: Introducing a Versatile and
> Transparent Approach.

## Installation

```r
# install.packages("remotes")
remotes::install_github("mharmening/ADGA")
```

## Usage

```r
library(ADGA)

# 1. Preprocess
annotated <- adga_preprocess(
  data      = my_data,
  text_col  = "text",
  id_col    = "doc_id",
  group_col = "label",
  language  = "english"
)

# 2. Compute relevance measures
tfidf     <- adga_tfidf(annotated)
chisquare <- adga_chisquare(annotated)
wordscore <- adga_wordscore(annotated)

# 3. (Optional) Remove stopwords
cleaned   <- adga_remove_stopwords(tfidf, chisquare, wordscore, language = "en")
tfidf     <- cleaned$tfidf
chisquare <- cleaned$chisquare
wordscore <- cleaned$wordscore

# 4. Select keywords via voting
results   <- adga_vote(tfidf, chisquare, wordscore, goal = 100)

# 5. Diagnostics
adga_diagnostics(results, annotated)
```

## License

MIT
