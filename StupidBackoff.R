library(tidyverse)
library(quanteda)
library(data.table)
library(plotly)
library(readtext)

n.grams <- fread("n_grams.txt", select = c("token", "count", "n"), data.table = TRUE, stringsAsFactors = FALSE, colClasses = c("character", "integer", "integer"))
unigram.count <- sum(n.grams[, count])
lambda <- 0.4

# Return TRUE if ngram exists in the list of n-grams, FALSE otherwise
ngramExists <- function(ngram) {
    return(all(ngram %in% n.grams[token == ngram, token, mult = "first", nomatch = 0]))
}

# Removes the first word of an n-gram, returning an n-1-gram
shortenNGram <- function(ngram) {
    return(sub("^[^_]*_", "", ngram))
}

# Returns the Stupid Backoff probability for a word given a sequence of preceding words
SBO <- function(word, prefix, n) {
    if (n == 1) {
        return(n.grams[token == word, count]/unigram.count)
    } else if (ngramExists(paste0(prefix, "_", word))) {
        return(n.grams[token == paste0(prefix, "_", word), count]/sum(n.grams[token == prefix, count]))
    } else {
        return(lambda * SBO(word, shortenNGram(prefix), n - 1))
    }
}
