library(tidyverse)
library(quanteda)
library(data.table)
library(plotly)
library(readtext)
library(stringr)
library(sqldf)

# setwd("C:/Users/sewright/Documents/R/Classes/CourseraDataScienceCapstone/StupidBackoff")
# n.grams <- fread("n_grams.txt", select = c("token", "count", "n"), data.table = TRUE, stringsAsFactors = FALSE, colClasses = c("character", "integer", "integer"))
# #n.grams <- n.grams[count > 3]
# unigram.count <- sum(n.grams[n == 1, count])
# n.max <- max(n.grams[, n])
lambda <- 0.4
# unigrams <- n.grams[n == 1 & count > 1]
# n.grams <- n.grams[n > 1 & count >= 4]
# unigrams[, prob := count/unigram.count]
# setkey(unigrams, prob)
# setkey(n.grams, n, token)
# setorder(n.grams, -n)

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
tokenizeInput <- function(input.string, n) {
    toks <- tokens(input.string, what = "word", remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove_url = TRUE, remove_twitter = TRUE, ngrams = n) %>%
        tokens_tolower() %>%
        unlist(use.names = FALSE) %>%
        as.data.table()
    if (toks[, .N] > 0) return(toks[.N])
    else return(tokenizeInput(input.string, n - 1))
}

SBO <- function(prefix, a = 1) {
    if (prefix == "") return(-1)
    prefix.count <- sqldf(paste0("select sum(count) from [n.grams] where prefix == '", prefix, "'"))
    if (is.na(prefix.count)) return(SBO(shortenNGram(prefix), a * lambda))
    result <- sqldf(paste0("select word, max(count) as count from [n.grams] where prefix == '", prefix, "'"))
    return(mutate(result, prob = a * count/prefix.count))
}


# predictNextWord <- function(word.seq) {
#     # Predicts the next word in a sequence of words, using a 4-gram model
#     #
#     # Args:
#     #   word.seq:   The sequence of words from which the next word will be predicted.
#     #               Words must be separated by an underscore and there must be no more than 4 words.
#     # Returns:
#     #   The predicted next word in the sequence
#     
#     # Count the number of words in the input phrase
#     ngram.list <- SBO(word.seq)
#     word <- sub("^.+_", "", ngram.list[prob == max(prob) & token != "#s#", token][1])
#     return(word)
# }
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

