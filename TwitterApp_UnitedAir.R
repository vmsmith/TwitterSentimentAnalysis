# ^\s*# TODO:

# Prepare the R environment -----------------------------------------------

rm(list = ls())

#install.packages("twitteR") # should also install bit, bit64, rjson
#install.packages("httr")
#install.packages("devtools")
#install.packages("tm")
#install.packages("stringr")


library(bit)
library(bit64)
library(devtools)
library(dplyr)
library(foreign)
library(ggplot2)
library(httr)
library(rjson)
library(stringr)
library(tm)
library(twitteR)


# Connect with Twitter and test the connection ----------------------------

api_key <- "aep7QPBd7NJ1m1ZSsodG5LBC4"

api_secret <- "OBtMcFkaaHPJKkqoORZoaeB8446BPoc7lqg76v6B0j5aereQlh"

access_token <- "872555012655894528-koPl407v9Py4oPtbj70KmGPygMrAGx0"

access_token_secret <- "OfTEGVMcqbGvWpudCqdQE2r6g8zSapqh0PhpbuieKqHB3"

setup_twitter_oauth(api_key,api_secret, access_token, access_token_secret)

searchTwitter("iphone", n = 10)


# Capture and package United tweets -----------------------------------------

united_tweets <- read.dbf("./1_Data/united.dbf", as.is = TRUE)


# Examine the tweets ------------------------------------------------------

# Size of data frame
dim(united_tweets)

# Column names
names(united_tweets)

# 1st tweet
united_tweets[1, ]

# Text of 1st tweet
united_tweets[1, 5]

# Text of first 20 tweets
united_tweets[1:20, 5]

# Create corpus
united_corpus <- Corpus(VectorSource(united_tweets[ , 5]))
united_corpus

# Text
united_corpus[[5]][1]
# Metadata
united_corpus[[1]][2]

# Process the text --------------------------------------------------------

# See tm native transformations
getTransformations()

# Create custom transformations

remove_emoticons <- function(x) iconv(x, to = "UTF-8-MAC", sub = "byte")

remove_bad_encoding <- function(x) gsub("\\x[^**]", "", x)
  
remove_URLs <- function(x) gsub("http[^[:space:]]*", "", x)

remove_control_characters <- function(x) gsub("[[:cntrl:]]", "", x)

united_corpus <- tm_map(united_corpus, content_transformer(remove_emoticons))

united_corpus <- tm_map(united_corpus, content_transformer(remove_bad_encoding))

united_corpus <- tm_map(united_corpus, content_transformer(remove_URLs))

united_corpus <- tm_map(united_corpus, content_transformer(remove_control_characters))

united_corpus[[5]][1]


# TODO: Remove duplicates

# TODO: Account for NAs

# TODO: Test retweet removal function
# Remove retweet entities from the stored tweets (text)
# bjp_txt = gsub(“(RT|via)((?:\\b\\W*@\\w+)+)”, “”, bjp_txt)


# Remove other than English or space
tweets_text <- gsub("[^[:alpha:][:space:]]*", "", tweets_text)


# Convert to lower case
tweets_text <- tolower(tweets_text)

tweets_text

word.list <- str_split(tweets_text, "\\s+")
word.list
words = unlist(word.list)
# TODO: Compare list of words to vector of words
words
length(words)

# Import and apply sentiments ---------------------------------------------

# TODO: Look for more current sentiment files

posText <- read.delim("./words-positive-2.txt", header = FALSE, stringsAsFactors = FALSE)
typeof(posText)
posText

posText <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))
typeof(posText)
length(posText)

negText <- read.delim("./words-negative.txt", header = FALSE, stringsAsFactors = FALSE)
typeof(negText)

negText <- unlist(lapply(negText, function(x) { str_split(x, "\n") }))
typeof(negText)
length(negText)

posText <- c(posText, 'upgrade')
negText <- c(negText, 'wtf', 'wait', 'waiting','epicfail', 'mechanical')

posMatches <- match(words, posText)
posMatches <- !is.na(posMatches)
posSentiments <- length(which(posMatches))

negMatches <- match(words, negText)
negMatches <- !is.na(negMatches)
negSentiments <- length(which(negMatches))

sentiment <- length(which(posMatches)) - length(which(negMatches))
sentiment


# View the sentiments -----------------------------------------------------

Sentiments <- c("Positive Sentiments" = posSentiments, "Negative Sentiments" = negSentiments)
barplot(Sentiments, main = "Starbucks Tweets", ylab = "Sentiment Count")


################################################################################
#
#                              Issues
#
################################################################################

# 1. Sentiment list completeness, accuracy, and timeliness

# 2. n-grams: e.g., "good" vs "not good"

# 3. Duplicates will skew analysis
