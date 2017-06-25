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
library(syuzhet)
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

# Extract TimeStamp and Text columns  
united_passenger_dragged <- select(united_tweets, c(4, 5))
names(united_passenger_dragged)

# TODO: Experiment with changing TimeStamp column with as.Date

# Extract Mar - May 2017 tweets
united_passenger_dragged <- filter(united_passenger_dragged, as.Date(TimeStamp) >= "2017-03-15")
united_passenger_dragged$TimeStamp[1]
tail(united_passenger_dragged$TimeStamp, n = 1)

united_passenger_dragged <- filter(united_passenger_dragged, as.Date(TimeStamp) <= "2017-05-31")
united_passenger_dragged$TimeStamp[1]
tail(united_passenger_dragged$TimeStamp, n = 1)
# TODO: Figure out why the ending date is not working

dim(united_passenger_dragged)

# Process the text --------------------------------------------------------

# Take a peek
sample(united_passenger_dragged[ , 2], 50, replace = FALSE)

?# Create custom transformations

remove_emoticons <- function(x) iconv(x, to = "UTF-8-MAC", sub = "byte")
remove_bad_encoding <- function(x) gsub("\\x[^**]", "", x)
remove_URLs <- function(x) gsub("http[^[:space:]]*", "", x)
remove_control_characters <- function(x) gsub("[[:cntrl:]]", "", x)
remove_at <- function(x) gsub()
remove_hashtag <- function(x) gsub()
remove_punctuation <- function(x) gsub("[[:punct:]]", "", x)
remove_digits <- function(x) gsub("[[:digit:]]+", "", x)
remove_spaces <- function(x) gsub("[ \t]{2,}", "", x)
change_dash_to_space <- function(x) chartr("-", " ", x)
remove_miscellaneous <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

# Custom transformation
remove_pictwittercom <- function(x) gsub("pictwittercom\\w+ *"  , "", x)

# Convert to lower case
convert_to_lower <- function(x) tolower(x)


united_passenger_dragged[ , 2] <- remove_emoticons(united_passenger_dragged[ , 2])
united_passenger_dragged[ , 2] <- remove_URLs(united_passenger_dragged[ , 2])
united_passenger_dragged[ , 2] <- remove_control_characters(united_passenger_dragged[ , 2])
united_passenger_dragged[ , 2] <- remove_punctuation(united_passenger_dragged[ , 2])
united_passenger_dragged[ , 2] <- remove_digits(united_passenger_dragged[ , 2])
united_passenger_dragged[ , 2] <- remove_spaces(united_passenger_dragged[ , 2])
united_passenger_dragged[ , 2] <- remove_miscellaneous(united_passenger_dragged[ , 2])
united_passenger_dragged[ , 2] <- remove_pictwittercom(united_passenger_dragged[ , 2])

united_passenger_dragged[ , 2] <- remove_emoticons(united_passenger_dragged[ , 2]) %>%
                                  remove_URLs() %>%
                                  remove_control_characters() %>%
                                  remove_punctuation() %>%
                                  remove_digits() %>%
                                  remove_spaces() %>%
                                  remove_miscellaneous() %>%
                                  remove_pictwittercom() %>%
                                  tolower()

united_passenger_dragged[1:20, 2]


# TODO: Remove duplicates

# TODO: Account for NAs

# TODO: Test retweet removal function
# Remove retweet entities from the stored tweets (text)
# bjp_txt = gsub(“(RT|via)((?:\\b\\W*@\\w+)+)”, “”, bjp_txt)

# Run initial sentiment functions from Syuzhet package




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
