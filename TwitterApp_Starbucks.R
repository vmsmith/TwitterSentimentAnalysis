
# ^\s*# TODO:

# Prepare the R environment -----------------------------------------------

# Clear the environment

rm(list = ls())

# Install necessary packages

install.packages("twitteR") # should also install bit, bit64, rjson
install.packages("httr")
install.packages("devtools")
install.packages("tm")
install.packages("stringr")

# Load packages

library(bit)
library(bit64)
library(devtools)
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

searchTwitter("iphone")

# Capture and package some tweets -----------------------------------------

Starbucks_tweets <- searchTwitter("Starbucks", n = 1000, lang = "en")

tweets_df <- twListToDF(Starbucks_tweets)

write.table(tweets_df, "./Tweets_Starbucks_1000.csv", sep = ",", row.names = FALSE)

# read.table("                           ", header = TRUE, sep = ",")


# Examine the tweets ------------------------------------------------------

# Size of data frame
dim(tweets_df)

# Column names
names(tweets_df)

# 1st tweet
tweets_df[1, ]

# Text of 1st tweet
tweets_df[1, 1]

tweets_df[1:3, 1]


# Process the text for analysis---------------------------------------------

# Isolate text
tweets_text <- tweets_df[ , 1]
tweets_text

# TODO: Remove duplicates

# TODO: Account for NAs

# Convert encoding of emoticons
# See: https://stackoverflow.com/questions/9637278/r-tm-package-invalid-input-in-utf8towcs
# TODO: Function to determine operating system and deal with unicode appropriately
if(unname(Sys.info()["sysname"]) == "Darwin") {
  tweets_text <- iconv(tweets_text, to = "UTF-8-MAC", sub = "byte")
} else {
  tweets_text <- iconv(enc2utf8(tweets_text), sub = "byte")
}

# Remove bad encoding
tweets_text <- gsub("\\x[^**]", "", tweets_text)                # <- To Do

# Remove URLs
tweets_text <- gsub("http[^[:space:]]*", "", tweets_text)

# Remove punctuation
tweets_text = gsub("[[:punct:]]", "", tweets_text)

# Remove control characters
tweets_text = gsub("[[:cntrl:]]", "", tweets_text)

# Remove digits
tweets_text = gsub('\\d+', '', tweets_text)

# Remove other than English or space
tweets_text <- gsub("[^[:alpha:][:space:]]*", "", tweets_text)

# Convert to lower case
tweets_text <- tolower(tweets_text)

# Take a look
tweets_text

# Separate sentences into individual words
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
 