library(tidyverse)
library(quanteda)
library(data.table)
library(plotly)
library(readtext)
library(stringr)

setwd("C:/Users/sewright/Documents/R/Classes/CourseraDataScienceCapstone/StupidBackoff")
n.grams <- fread("n_grams.txt", select = c("token", "count", "n"), data.table = TRUE, stringsAsFactors = FALSE, colClasses = c("character", "integer", "integer"))
#n.grams <- n.grams[count > 3]
unigram.count <- sum(n.grams[n == 1, count])
n.max <- max(n.grams[, n])
lambda <- 0.4
unigrams <- n.grams[n == 1 & count > 1]
n.grams <- n.grams[n > 1 & count >= 4]
unigrams[, prob := count/unigram.count]
setkey(unigrams, prob)
setkey(n.grams, n, token)
setorder(n.grams, -n)

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
    df <- data.frame()
    a <- 1
    #shorten prefix if longer than the model supports
    while (n.prefix >= n.max) {
        prefix <- shortenNGram(prefix)
        n.prefix <- countTokens(prefix)
    }
    sum.prefix <- sum(n.grams[token %like% paste0("^", prefix) & n == n.prefix, count])
    df <- rbind(df, n.grams[n == (n.prefix + 1) & token %like% searchPattern(prefix), .(token, prob = count/sum.prefix)])
    while (n.prefix > 2) {
        a <- a * lambda
        prefix <- shortenNGram(prefix)
        n.prefix <- n.prefix - 1
        sum.prefix <- sum(n.grams[token %like% paste0("^", prefix) & n == n.prefix, count])
        if (sum.prefix > 0) df <- rbind(df, n.grams[n == (n.prefix + 1) & token %like% searchPattern(prefix), .(token, prob = a * (count/sum.prefix))])
    }
    
    df <- rbind(df, unigrams[, .(token, prob = a*lambda*prob)])
    
    return(df)
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
    word <- sub("^.+_", "", ngram.list[prob == max(prob) & token != "#s#", token][1])
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

