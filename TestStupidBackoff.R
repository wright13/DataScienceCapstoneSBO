# Load packages
library(data.table)
library(tidyverse)
library(quanteda)
library(readr)
library(R.utils)
library(readtext)
library(sqldf)
source("StupidBackoff.R")

# Read texts into invididual corpus. readtext() works for twitter and blogs, and read_file() works for news.
test.path <- paste0(getwd(), "/test.txt")
corp <- corpus(readtext(test.path))
toks <- tokens(corp, what = "word", remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE, remove_twitter = TRUE, ngrams = 5) %>%
    tokens_tolower() %>%
    unlist(use.names = FALSE) %>%
    as.data.table()
names(toks) <- "n.gram"
all.tokens <- unique(toks)
all.tokens <- all.tokens[, list(prefix = sub("_[^_]+$", "", n.gram), word = sub("^([^_]+_)+", "", n.gram))]

fun <- function(x) {
    r <- SBO(x)
    r <- setorder(r, -prob)
    w <- r[1, c("word")]
    return(w)
}

test <- all.tokens[, list(prefix, word, prediction = sapply(prefix, fun), prob)]
