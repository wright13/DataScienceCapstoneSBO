library(tidyverse)
library(quanteda)
library(data.table)
library(plotly)
library(readtext)

n.grams <- fread("n_grams.txt", select = c("one.gram", "count"), data.table = TRUE, stringsAsFactors = FALSE, colClasses = c("character", "integer"))
