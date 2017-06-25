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

# Load United tweets into a data frame
united_tweets <- read.dbf("./1_Data/united.dbf", as.is = TRUE)


# Examine the tweets ------------------------------------------------------

# Size of data frame
dim(united_tweets)

# Column names
names(united_tweets)

# 1st tweet
united_tweets[1, ] # 01 Jan 2015
united_tweets[268337, ] # 31 Dec 2017

# Text of 1st tweet
united_tweets[1, 5]

# Text of first 20 tweets
united_tweets[1:20, 5]


# Sample of dates
sample(united_tweets[ , 4], 100, replace = FALSE)

# Subset of 100 tweets
united_tweets <- united_tweets[1:100, ]
dim(united_tweets)

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

change_dash_to_space <- function(x) chartr("-", " ", x)

# Remove other than English or space
remove_miscellaneous <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

# Use custom transformations

united_corpus <- tm_map(united_corpus, content_transformer(remove_emoticons))
united_corpus[[5]][1]

united_corpus <- tm_map(united_corpus, content_transformer(remove_bad_encoding))
united_corpus[[5]][1]

united_corpus <- tm_map(united_corpus, content_transformer(remove_URLs))
united_corpus[[5]][1]

united_corpus <- tm_map(united_corpus, content_transformer(remove_control_characters))
united_corpus[[5]][1]

united_corpus <- tm_map(united_corpus, content_transformer(change_dash_to_space))
united_corpus[[5]][1]

united_corpus <- tm_map(united_corpus, content_transformer(remove_miscellaneous))
united_corpus[[5]][1]

# Use tm transformations

united_corpus <- tm_map(united_corpus, removeNumbers)
united_corpus[[5]][1]

united_corpus <- tm_map(united_corpus, removePunctuation)
united_corpus[[5]][1]

united_corpus <- tm_map(united_corpus, stripWhitespace)
united_corpus[[5]][1]

united_corpus <- tm_map(united_corpus, tolower)
united_corpus[[5]][1]


# TODO: Remove duplicates

# TODO: Account for NAs

# TODO: Test retweet removal function
# Remove retweet entities from the stored tweets (text)
# bjp_txt = gsub(“(RT|via)((?:\\b\\W*@\\w+)+)”, “”, bjp_txt)




# Tokenize tweets ---------------------------------------------------------

getTokenizers()

united_corpus[[1]][1]
united_corpus_scan <- scan_tokenizer(united_corpus)
united_corpus_scan[1:50]

united_corpus[[1]][1]
united_corpus_mc <- MC_tokenizer(united_corpus)
united_corpus_mc

# Separate tweets into individual words
tokenize_tweets <- function(x) str_split(x, "\\s+")
united_corpus_t <- tm_map(united_corpus, tokenize_tweets)
united_corpus_t[[1]][1]
united_corpus_tokens <- unname(unlist(united_corpus_t))

length(united_corpus_scan)
length(united_corpus_mc)
length(united_corpus_tokens)


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

posMatches <- match(united_corpus_tokens, posText)
posMatches <- !is.na(posMatches)
posSentiments <- length(which(posMatches))

negMatches <- match(united_corpus_tokens, negText)
negMatches <- !is.na(negMatches)
negSentiments <- length(which(negMatches))

sentiment <- length(which(posMatches)) - length(which(negMatches))
sentiment


# View the sentiments -----------------------------------------------------

Sentiments <- c("Positive Sentiments" = posSentiments, "Negative Sentiments" = negSentiments)
barplot(Sentiments, main = "United Airlines Tweets", ylab = "Sentiment Count")


################################################################################
#
#                              Issues
#
################################################################################

# 1. Sentiment list completeness, accuracy, and timeliness

# 2. n-grams: e.g., "good" vs "not good"

# 3. Duplicates will skew analysis
