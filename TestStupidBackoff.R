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
tokns <- tokens(corp, what = "word", remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE, remove_twitter = TRUE, ngrams = 5) %>%
    tokens_tolower() %>%
    unlist(use.names = FALSE) %>%
    as.data.table()
names(tokns) <- "n.gram"
#all.tokens <- unique(toks)
tokns <- tokns[, list(prefix = sub("_[^_]+$", "", n.gram), word = sub("^([^_]+_)+", "", n.gram))]
set.seed(1302018)
sample.tokens <- tokns[rbinom(nrow(tokns), 1, 5000/nrow(tokns)) == 1]

fun <- function(x) {
    r <- SBO(x)
    r <- setorder(r, -prob)
    w <- r[1, c("word", "prob")]
    return(w)
}

fun2 <- function(x) {
    r <- SBO(x)
    r <- r[,1:2]
    return(r)
}

xentropy <- 0
matches.top3 <- 0
for (i in 1:nrow(sample.tokens)) {
    pred <- fun(sample.tokens[i, prefix])
    pred.top3 <- fun2(sample.tokens[i, prefix])
    
    if (any(is.na(pred))) pred <- c("UNK", "0") 
    result <- sample.tokens[i, prediction := pred[[1]]]
    if (pred[[2]] > 0) xentropy <- xentropy + log(pred[[2]])
    
    if (nrow(pred.top3) > 0) matches.top3 <- matches.top3 + any(pred.top3$word == sample.tokens[i, word])
}

avg.x.entropy <- -xentropy/nrow(sample.tokens)
accuracy <- nrow(sample.tokens[word == prediction]) / nrow(sample.tokens)
accuracy.top3 <- matches.top3 / nrow(sample.tokens)


# test <- sample.tokens[, list(prefix, word, prediction = sapply(prefix, fun)[1])]
