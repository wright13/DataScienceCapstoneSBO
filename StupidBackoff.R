library(tidyverse)
library(quanteda)
library(data.table)
library(plotly)
library(readtext)
library(stringr)
library(sqldf)

setwd("C:/Users/sewright/Documents/R/Classes/CourseraDataScienceCapstone/StupidBackoff")
n.grams <- fread("filtered_n_grams.txt", select = c("prefix", "word", "word.count", "prefix.count"), data.table = TRUE, stringsAsFactors = FALSE, colClasses = c("character", "character", "integer", "integer"))
lambda <- 0.4
setkey(n.grams, prefix)

# Return TRUE if ngram exists in the list of n-grams, FALSE otherwise
countTokens <- function(input.prefix) {
    if (input.prefix == "") return(0)
    return(str_count(input.prefix, "_") + 1)
}

ngramExists <- function(input.prefix) {
    return(n.grams[prefix == input.prefix, .N] > 0)
}

# Removes the first word of an n-gram, returning an n-1-gram. Returns empty string if passed a single word.
shortenNGram <- function(input.prefix) {
    if (countTokens(input.prefix) == 1) return("")
    return(sub("^[^_]*_", "", input.prefix))
}

# Tokenize input
tokenizeInput <- function(input.string, n = 4) {
    toks <- tokens(input.string, what = "word", remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE, remove_twitter = TRUE, ngrams = n) %>%
        tokens_tolower() %>%
        unlist(use.names = FALSE) %>%
        as.data.table()
    if (toks[, .N] > 0) return(toks[.N])
    else return(tokenizeInput(input.string, n - 1))
}

SBO <- function(prefix, a = 1) {
    result <- sqldf(paste0("select * from [n.grams] where prefix == '", prefix, "'"))
    if (nrow(result) > 0) return(mutate(result, prob = a * word.count/prefix.count))
    else if (prefix == "") return(mutate(result, prob = 0))
    else return(SBO(shortenNGram(prefix), a * lambda))
}


predictNextWord <- function(word.seq) {
    # Predicts the next word in a sequence of words, using a 4-gram model
    #
    # Args:
    #   word.seq:   The sequence of words from which the next word will be predicted.
    # Returns:
    #   The predicted next word in the sequence
    
    ngram <- tokenizeInput(word.seq)
    prediction <- SBO(ngram)
    if (nrow(prediction) == 0) return("<UNK>")
    else return(prediction$word)
}
# 
# 
# predictNextWord("and_a_case_of")
# predictNextWord("it_would_mean_the")
# predictNextWord("and_make_me_the")
# predictNextWord("still_struggling_but_the")
# predictNextWord("romantic_date_at_the")
# predictNextWord("and_be_on_my")
# predictNextWord("it_in_quite_some")
# predictNextWord("eyes_with_his_little")
# predictNextWord("the_faith_during_the")
# predictNextWord("then_you_must_be")

