# Load packages
library(data.table)
library(tidyverse)
library(quanteda)
library(readr)
library(R.utils)
library(readtext)
library(sqldf)
source(paste0(getwd(),"/StupidBackoff/StupidBackoff.R"))

# Read texts into invididual corpus. readtext() works for twitter and blogs, and read_file() works for news.
test.path <- paste0(getwd(), "/StupidBackoff/test.txt")
corp <- corpus(readtext(test.path))
tokns <- tokens(corp, what = "word", remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE, remove_twitter = TRUE, ngrams = 5) %>%
    tokens_tolower() %>%
    unlist(use.names = FALSE) %>%
    as.data.table()
names(tokns) <- "n.gram"
#all.tokens <- unique(toks)
tokns <- tokns[, list(prefix = sub("_[^_]+$", "", n.gram), word = sub("^([^_]+_)+", "", n.gram))]
set.seed(1302018)
sample.tokens <- tokns[rbinom(nrow(tokns), 1, 5000/nrow(tokns)) == 1]

topPred <- function(x) {
    r <- SBO(x)
    r <- setorder(r, -prob)
    w <- r[1, c("word", "prob")]
    return(w)
}

top3Pred <- function(x) {
    r <- SBO(x)
    r <- r[, c("word", "prob")]
    return(r)
}

matches.top3 <- 0
for (i in 1:nrow(sample.tokens)) {
    pred <- topPred(sample.tokens[i, prefix])
    pred.top3 <- top3Pred(sample.tokens[i, prefix])
    
    result <- sample.tokens[i, prediction := pred[[1]]]
    
    if (nrow(pred.top3) > 0) matches.top3 <- matches.top3 + any(pred.top3$word == sample.tokens[i, word])
}

accuracy <- nrow(sample.tokens[word == prediction]) / nrow(sample.tokens)
accuracy.top3 <- matches.top3 / nrow(sample.tokens)
