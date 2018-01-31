library(tidyverse)
library(quanteda)
library(data.table)
library(plotly)
library(readtext)
library(stringr)
library(sqldf)

setwd("C:/Users/sewright/Documents/R/Classes/CourseraDataScienceCapstone/StupidBackoff")
n.grams <- fread("filtered_n_grams.txt", drop = "index", col.names = c("prefix", "word", "word.count", "prefix.count"), data.table = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
lambda <- 0.4
setkey(n.grams, prefix)

# Return TRUE if ngram exists in the list of n-grams, FALSE otherwise
countTokens <- function(input.prefix) {
    if (input.prefix == "") return(0)
    return(str_count(input.prefix, "_") + 1)
}

# Removes the first word of an n-gram, returning an n-1-gram. Returns empty string if passed a single word.
shortenNGram <- function(input.prefix) {
    if (countTokens(input.prefix) == 1) return("")
    return(sub("^[^_]*_", "", input.prefix))
}

# Tokenize input
tokenizeInput <- function(input.string, n = 4) {
    if (trimws(input.string, "both") == "") return("<UNK>")
    toks <- tokens(input.string, what = "word", remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE, remove_twitter = TRUE, ngrams = n) %>%
        tokens_tolower() %>%
        unlist(use.names = FALSE) %>%
        as.data.table()
    if (toks[, .N] > 0) return(toks[.N])
    else return(tokenizeInput(input.string, n - 1))
}

SBO <- function(input.prefix, a = 1) {
    result <- n.grams[prefix == input.prefix]
    if (nrow(result) > 0) return(mutate(result, prob = a * word.count/prefix.count))
    else return(SBO(shortenNGram(input.prefix), a * lambda))
}


predictNextWord <- function(input.string) {
    # Predicts the next word in a sequence of words, using a 4-gram model
    #
    # Args:
    #   word.seq:   The sequence of words from which the next word will be predicted.
    # Returns:
    #   The predicted next word in the sequence
    
    ngram <- tokenizeInput(input.string)
    prediction <- SBO(ngram)
    return(prediction)
}

generateParagraph <- function(input.string, n = 20) {
    text <- input.string
    for (i in 1:n) {
        pred <- predictNextWord(text)
        pred <- sample_n(pred, size = 1, weight = pred$prob)
        text <- paste(text, pred$word)
    }
    return(text)
}
