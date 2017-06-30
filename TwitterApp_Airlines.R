# ^\s*# TODO:

# Prepare the R environment -----------------------------------------------

rm(list = ls())

library(bit)
library(bit64)
library(data.table)
library(devtools)
library(dplyr)
library(foreign)
library(ggplot2)
library(httr)
library(lubridate)
library(plyr)
library(rjson)
library(stringr)
library(syuzhet)
library(tm)
library(twitteR)

source("./cleaning_functions.R")

# Connect with Twitter and test the connection ----------------------------

api_key <- "aep7QPBd7NJ1m1ZSsodG5LBC4"

api_secret <- "OBtMcFkaaHPJKkqoORZoaeB8446BPoc7lqg76v6B0j5aereQlh"

access_token <- "872555012655894528-koPl407v9Py4oPtbj70KmGPygMrAGx0"

access_token_secret <- "OfTEGVMcqbGvWpudCqdQE2r6g8zSapqh0PhpbuieKqHB3"

setup_twitter_oauth(api_key,api_secret, access_token, access_token_secret)

searchTwitter("iphone", n = 10)


# Capture and package United tweets -----------------------------------------

airlines_tweets <- read.dbf("./1_Data/airlines.dbf", as.is = TRUE)


# Examine the tweets ------------------------------------------------------

# Size of data frame
dim(airlines_tweets)

# Column names
names(airlines_tweets)

# 1st tweet
airlines_tweets[1, ]

# Last tweet
tail(airlines_tweets, n = 1)

# Text of 1st tweet
airlines_tweets[1, 5]

# Random 20 tweets
airlines_tweets[sample(1:1087679, 20), 5]


# Isolate the time period of interest -------------------------------------

airlines_tweets <- select(airlines_tweets, c(1, 4, 5))
names(airlines_tweets)

# Change TimeStamp to date
airlines_tweets$TimeStamp <- as.Date(airlines_tweets$TimeStamp)

# Extract Mar - May 2017 tweets
airlines_tweets <- filter(airlines_tweets, TimeStamp >= "2017-03-15")
airlines_tweets$TimeStamp[1]
tail(airlines_tweets$TimeStamp, n = 1)

airlines_tweets <- filter(airlines_tweets, TimeStamp <= "2017-05-31")
airlines_tweets$TimeStamp[1]
tail(airlines_tweets$TimeStamp, n = 1)
# TODO: Figure out why the ending date is not working

dim(airlines_tweets)

# Execute text cleaning functions -----------------------------------------

# Option 1 - Individual functions one at a time
airlines_tweets[1:20, 3]

airlines_tweets[ , 3] <- remove_emoticons(airlines_tweets[ , 3])
airlines_tweets[1:20, 3]

airlines_tweets[ , 3] <- remove_bad_encoding(airlines_tweets[ , 3])
airlines_tweets[1:20, 3]

airlines_tweets[ , 3] <- remove_URLs(airlines_tweets[ , 3])
airlines_tweets[1:20, 3]

airlines_tweets[ , 3] <- change_dash_to_space(airlines_tweets[ , 3])
airlines_tweets[1:20, 3]

airlines_tweets[ , 3] <- remove_punctuation(airlines_tweets[ , 3])
airlines_tweets[1:20, 3]

airlines_tweets[ , 3] <- remove_digits(airlines_tweets[ , 3])
airlines_tweets[1:20, 3]

airlines_tweets[ , 3] <- remove_control_characters(airlines_tweets[ , 3])
airlines_tweets[1:20, 3]

airlines_tweets[ , 3] <- collapse_spaces(airlines_tweets[ , 3])
airlines_tweets[1:20, 3]

airlines_tweets[ , 3] <- remove_pictwittercom(airlines_tweets[ , 3])
airlines_tweets[1:20, 3]

airlines_tweets[ , 3] <- tolower(airlines_tweets[ , 3])
airlines_tweets[1:20, 3]

# Option 2 - With the pipe operator
airlines_tweets[ , 2] <- remove_emoticons(airlines_tweets[ , 2]) %>%
  remove_bad_encoding() %>%
  remove_URLs() %>%
  change_dash_to_space() %>%
  remove_punctuation() %>%
  remove_digits() %>%
  remove_control_characters() %>%
  collapse_spaces() %>%
  remove_pictwittercom() %>%
  tolower()

# TODO: Remove stop words

# TODO: Remove duplicates

# TODO: Account for NAs

# TODO: Test retweet removal function

# Remove retweet entities from the stored tweets (text)
# bjp_txt = gsub(“(RT|via)((?:\\b\\W*@\\w+)+)”, “”, bjp_txt)

# Run initial sentiment functions from Syuzhet package --------------------

scoreSentiment = function(tab)
{
  tab$syuzhet = get_sentiment(tab$Text, method = "syuzhet")
  tab$bing = get_sentiment(tab$Text, method = "bing")
  tab$afinn = get_sentiment(tab$Text, method = "afinn")
  tab$nrc = get_sentiment(tab$Text, method = "nrc")
  emotions = get_nrc_sentiment(tab$Text)
  n = names(emotions)
  for (nn in n) tab[, nn] = emotions[nn]
  return(tab)
}

# Get the sentiment scores for the tweets (about 5 1/2 minutes on the Mac Pro)
tweets = scoreSentiment(airlines_tweets)
names(tweets)
tweets$TimeStamp <- as.Date(tweets$TimeStamp)
tweets[1, ]

# Visualize the results ---------------------------------------------------

# get daily summaries of the results
daily = ddply(tweets, ~ Airline + TimeStamp, summarize, ave_sentiment = mean(bing))

# plot the daily sentiment
ggplot(daily, aes(x = TimeStamp, y = ave_sentiment, colour = Airline)) + geom_line() +
  ggtitle("Airline Sentiment") + xlab("Date") + ylab("Sentiment") + scale_x_date(date_labels = '%d-%b-%y')







################################################################################
#
#                              Issues
#
################################################################################

# 1. Sentiment list completeness, accuracy, and timeliness

# 2. n-grams: e.g., "good" vs "not good"

# 3. Duplicates will skew analysis
