library(tidyverse)
library(quanteda)
library(data.table)
library(plotly)
library(readtext)
library(parallel)

n.grams <- fread("n_grams.txt", select = c("token", "count", "n"), data.table = TRUE, stringsAsFactors = FALSE, colClasses = c("character", "integer", "integer"))
unigram.count <- sum(n.grams[n == 1, count])
lambda <- 0.4
unigrams <- n.grams[n == 1]
unigrams[, freq := count/unigram.count]
setkey(unigrams, freq)
bigrams <- n.grams[n == 2]
trigrams <- n.grams[n == 3]
tetragrams <- n.grams[n == 4]

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

# Iterative 4-gram SBO
SBOIterative4Gram <- function(word, prefix) {
    # If the tetragram prefix_word exists, calculate the SB probability from the tetragram
    tetragram <- paste0(prefix, "_", word)
    S <- tetragrams[token == tetragram, count]/sum(trigrams[token == prefix, count])
    #print(tetragram)
    if (!is_empty(S)) return(S)
    
    # If no tetragram exists, back off to a trigram
    prefix <- shortenNGram(prefix)
    trigram <- paste0(prefix, "_", word)
    S <- trigrams[token == trigram, count]/sum(bigrams[token == prefix, count])
    #print(trigram)
    if (!is_empty(S)) return(lambda * S)
    
    # If no trigram exists, back off to a bigram
    prefix <- shortenNGram(prefix)
    bigram <- paste0(prefix, "_", word)
    S <- bigrams[token == bigram, count]/sum(unigrams[token == prefix, count])
    #print(bigram)
    if (!is_empty(S)) return((lambda^2)*S)
    
    # If no bigram exists, back of to unigram
    #print(word)
    return((lambda^3)*unigrams[token == word, freq])
}

# Given a prefix, return the most likely next word
findNextWord <- function(prefix) {
    next.word <- unigrams[.N, token]   # Default to the most frequent unigram
    max.S <- (lambda^3)*unigrams[.N, freq]    # Default maximum S value to lambda^3 * max unigram freq
    for (word in unigrams[,token]) {
        S <- SBOIterative4Gram(word, prefix)
        if (S > max.S) {
            max.S <- S
            next.word <- word
        }
    }
    return(next.word)
}

findNextWord("went_home_to")
