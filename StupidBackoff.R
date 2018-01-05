library(tidyverse)
library(quanteda)
library(data.table)
library(plotly)
library(readtext)
library(doParallel)

n.grams <- fread("n_grams.txt", select = c("token", "count", "n"), data.table = TRUE, stringsAsFactors = FALSE, colClasses = c("character", "integer", "integer"))
unigram.count <- sum(n.grams[n == 1, count])
lambda <- 0.4
unigrams <- n.grams[n == 1 & count > 500]
unigrams[, freq := count/unigram.count]
setkey(unigrams, freq)
bigrams <- n.grams[n == 2]
trigrams <- n.grams[n == 3]
tetragrams <- n.grams[n == 4]

# Stolen from http://rpubs.com/erodriguez/nlpquanteda
parallelizeTask <- function(task, ...) {
    # Calculate the number of cores
    ncores <- detectCores() - 1
    # Initiate cluster
    cl <- makeCluster(ncores)
    registerDoParallel(cl)
    #print("Starting task")
    r <- task(...)
    #print("Task done")
    stopCluster(cl)
    r
}

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
predictNextWord <- function(prefix) {
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

#Rprof(append = TRUE, memory.profiling = TRUE, line.profiling = TRUE)

parallelized <- function() {
    parallelizeTask(predictNextWord, "a_case_of")
    parallelizeTask(predictNextWord, "would_mean_the")
    parallelizeTask(predictNextWord, "make_me_the")
    parallelizeTask(predictNextWord, "struggling_but_the")
    parallelizeTask(predictNextWord, "date_at_the")
    parallelizeTask(predictNextWord, "be_on_my")
    parallelizeTask(predictNextWord, "in_quite_some")
    parallelizeTask(predictNextWord, "with_his_little")
    parallelizeTask(predictNextWord, "faith_during_the")
    parallelizeTask(predictNextWord, "you_must_be")
}

system.time(parallelized())
#Rprof(NULL)

notparallelized <- function() {
    predictNextWord("a_case_of")
    predictNextWord("would_mean_the")
    predictNextWord("make_me_the")
    predictNextWord("struggling_but_the")
    predictNextWord("date_at_the")
    predictNextWord("be_on_my")
    predictNextWord("in_quite_some")
    predictNextWord("with_his_little")
    predictNextWord("faith_during_the")
    predictNextWord("you_must_be")
}

system.time(notparallelized())
