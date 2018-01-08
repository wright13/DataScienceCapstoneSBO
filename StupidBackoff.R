library(tidyverse)
library(quanteda)
library(data.table)
library(plotly)
library(readtext)
library(doParallel)
library(stringr)

n.grams <- fread("n_grams.txt", select = c("one.gram", "count", "n"), data.table = TRUE, stringsAsFactors = FALSE, colClasses = c("character", "integer", "integer"))
names(n.grams) <- c("token", "count", "n")
unigram.count <- sum(n.grams[n == 1, count])
n.max <- max(n.grams[, n])
lambda <- 0.4
unigrams <- n.grams[n == 1 & count > 500]
unigrams[, prob := count/unigram.count]
setkey(unigrams, prob)

# Return TRUE if ngram exists in the list of n-grams, FALSE otherwise
countTokens <- function(word.seq) {
    if (word.seq == "") return(0)
    return(str_count(word.seq, "_") + 1)
}

searchPattern <- function(prefix) {
    return(paste0("^", prefix, "_"))
}

ngramExists <- function(ngram) {
    return(n.grams[token %like% searchPattern(ngram), .N] > 0)
}

# Removes the first word of an n-gram, returning an n-1-gram. Returns empty string if passed a single word.
shortenNGram <- function(ngram) {
    if (countTokens(ngram) == 1) return("")
    return(sub("^[^_]*_", "", ngram))
}

SBO <- function(prefix) {
    # Given a prefix of n-1 words, return a data.table of n-grams with that prefix and their probabilities
    n.prefix <- countTokens(prefix)
    while (countTokens(prefix) > 0) {
        if (ngramExists(prefix)) {
            dt <- n.grams[n == (countTokens(prefix) + 1) & token %like% searchPattern(prefix)]
            return(dt[, .(token, prob = (lambda ^ (n.prefix - countTokens(prefix)))*(count / sum(count)))])
        }
        else prefix <- shortenNGram(prefix)
    }
    return(unigrams[, .(token, prob)])
}


predictNextWord <- function(word.seq) {
    # Predicts the next word in a sequence of words, using a 4-gram model
    #
    # Args:
    #   word.seq:   The sequence of words from which the next word will be predicted.
    #               Words must be separated by an underscore and there must be no more than 4 words.
    # Returns:
    #   The predicted next word in the sequence
    
    # Count the number of words in the input phrase
    ngram.list <- SBO(word.seq)
    word <- sub("^.+_", "", ngram.list[prob == max(prob), token])
    return(word)
}


predictNextWord("and_a_case_of")
predictNextWord("it_would_mean_the")
predictNextWord("and_make_me_the")
predictNextWord("still_struggling_but_the")
predictNextWord("romantic_date_at_the")
predictNextWord("and_be_on_my")
predictNextWord("it_in_quite_some")
predictNextWord("eyes_with_his_little")
predictNextWord("the_faith_during_the")
predictNextWord("then_you_must_be")