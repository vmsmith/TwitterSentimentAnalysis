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

# First and last dates
united_tweets[1, 4]
tail(united_tweets[ , 4], n = 1)

# Extract TimeStamp and Text columns, and munge TimeStamp -----------------

united_passenger_dragged <- select(united_tweets, c(4, 5))
names(united_passenger_dragged)

# TODO: Experiment with changing TimeStamp column with as.Date
united_passenger_dragged$TimeStamp <- as.Date(united_passenger_dragged$TimeStamp)

# Extract Mar - May 2017 tweets
united_passenger_dragged <- filter(united_passenger_dragged, TimeStamp >= "2017-03-15")
united_passenger_dragged$TimeStamp[1]
tail(united_passenger_dragged$TimeStamp, n = 1)

united_passenger_dragged <- filter(united_passenger_dragged, TimeStamp <= "2017-05-31")
united_passenger_dragged$TimeStamp[1]
tail(united_passenger_dragged$TimeStamp, n = 1)
# TODO: Figure out why the ending date is not working

dim(united_passenger_dragged)

# Take a peek
sample(united_passenger_dragged[ , 2], 50, replace = FALSE)

# Process the text --------------------------------------------------------

# Create transformation functions

remove_URLs <- function(x) gsub("http[^[:space:]]*", "", x)
remove_punctuation <- function(x) gsub("[[:punct:]]", "", x)
remove_digits <- function(x) gsub("[[:digit:]]+", "", x)
remove_pictwittercom <- function(x) gsub("pictwittercom\\w+ *"  , "", x, ignore.case = TRUE)

remove_emoticons <- function(x) iconv(x, to = "UTF-8-MAC", sub = "byte")
remove_bad_encoding <- function(x) gsub("\\x[^**]", "", x)
remove_control_characters <- function(x) gsub("[[:cntrl:]]", "", x)
remove_at <- function(x) gsub()
remove_hashtag <- function(x) gsub()
remove_spaces <- function(x) gsub("[ \t]{2,}", "", x)
change_dash_to_space <- function(x) chartr("-", " ", x)
remove_miscellaneous <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
#remove_spaces <- function(x) gsub("[^[:space:]]*", "", x)
convert_to_lower <- function(x) tolower(x)

# Option 1
united_passenger_dragged[1:20, 2]

united_passenger_dragged[ , 2] <- remove_URLs(united_passenger_dragged[ , 2])
united_passenger_dragged[1:20, 2]

united_passenger_dragged[ , 2] <- remove_punctuation(united_passenger_dragged[ , 2])
united_passenger_dragged[1:20, 2]

united_passenger_dragged[ , 2] <- remove_digits(united_passenger_dragged[ , 2])
united_passenger_dragged[1:20, 2]

united_passenger_dragged[ , 2] <- remove_pictwittercom(united_passenger_dragged[ , 2])
united_passenger_dragged[1:20, 2]

#united_passenger_dragged[ , 2] <- remove_spaces(united_passenger_dragged[ , 2])
#united_passenger_dragged[1:20, 2]

united_passenger_dragged[ , 2] <- remove_emoticons(united_passenger_dragged[ , 2])
united_passenger_dragged[1:20, 2]

united_passenger_dragged[ , 2] <- remove_control_characters(united_passenger_dragged[ , 2])
united_passenger_dragged[1:20, 2]

united_passenger_dragged[ , 2] <- remove_miscellaneous(united_passenger_dragged[ , 2])
united_passenger_dragged[1:20, 2]

united_passenger_dragged[ , 2] <- tolower(united_passenger_dragged[ , 2])
united_passenger_dragged[1:20, 2]


# Option 2
united_passenger_dragged[ , 2] <- remove_emoticons(united_passenger_dragged[ , 2]) %>%
                                  remove_URLs() %>%
                                  remove_control_characters() %>%
                                  remove_punctuation() %>%
                                  remove_digits() %>%
                                  # remove_spaces() %>%
                                  remove_miscellaneous() %>%
                                  tolower()



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

# Get the sentiment scores for the tweets (about 3 1/2 minutes on the Mac Pro)
tweets = scoreSentiment(united_passenger_dragged)
names(tweets)
 tweets[1, ]



# Plot daily sentiments using the four different algorithms ---------------

 
# Get daily summaries of the results - Syuzhet
daily_syuzhet = ddply(tweets, ~ TimeStamp, summarize, ave_sentiment = mean(syuzhet))
 
# Plot the daily sentiment - Syuzhet
ggplot(daily_syuzhet, aes(x = TimeStamp, y = ave_sentiment)) + geom_line() +
   ggtitle("United Airline Sentiment: Syuzhet") + xlab("Date") + ylab("Sentiment") + scale_x_date(date_labels = '%d-%b-%y')
 
 
 
# Get daily summaries of the results - Bing
daily_bing = ddply(tweets, ~ TimeStamp, summarize, ave_sentiment = mean(bing))

# Plot the daily sentiment - Bing
ggplot(daily_bing, aes(x = TimeStamp, y = ave_sentiment)) + geom_line() +
  ggtitle("United Airline Sentiment: Bing") + xlab("Date") + ylab("Sentiment") + scale_x_date(date_labels = '%d-%b-%y')


# Get daily summaries of the results - AFINN
daily_afinn = ddply(tweets, ~ TimeStamp, summarize, ave_sentiment = mean(afinn))

# Plot the daily sentiment - AFINN
ggplot(daily_afinn, aes(x = TimeStamp, y = ave_sentiment)) + geom_line() +
  ggtitle("United Airline Sentiment: AFINN") + xlab("Date") + ylab("Sentiment") + scale_x_date(date_labels = '%d-%b-%y')

# Get daily summaries of the results - NRC
daily_nrc = ddply(tweets, ~ TimeStamp, summarize, ave_sentiment = mean(nrc))

# Plot the daily sentiment - NRC
ggplot(daily_nrc, aes(x = TimeStamp, y = ave_sentiment)) + geom_line() +
  ggtitle("United Airline Sentiment: NRC") + xlab("Date") + ylab("Sentiment") + scale_x_date(date_labels = '%d-%b-%y')


# Delete eventually -------------------------------------------------------
 
# Get daily summaries of the results - Syuzhet
 daily = ddply(tweets, ~ TimeStamp, summarize, num_tweets = length(positive), ave_sentiment = mean(syuzhet),
               ave_negative = mean(negative), ave_positive = mean(positive), ave_anger = mean(anger))
 
# Plot the daily sentiment
 ggplot(daily, aes(x = TimeStamp, y = ave_sentiment)) + geom_line() +
   ggtitle("United Airline Sentiment: Syuzhet") + xlab("Date") + ylab("Sentiment") + scale_x_date(date_labels = '%d-%b-%y')
 
 
# Get daily summaries of the results - Bing
daily = ddply(tweets, ~ TimeStamp, summarize, num_tweets = length(positive), ave_sentiment = mean(bing),
              ave_negative = mean(negative), ave_positive = mean(positive), ave_anger = mean(anger))

# Plot the daily sentiment
ggplot(daily, aes(x = TimeStamp, y = ave_sentiment)) + geom_line() +
  ggtitle("United Airline Sentiment: Bing") + xlab("Date") + ylab("Sentiment") + scale_x_date(date_labels = '%d-%b-%y')


# Get daily summaries of the results - AFINN
daily = ddply(tweets, ~ TimeStamp, summarize, num_tweets = length(positive), ave_sentiment = mean(afinn),
              ave_negative = mean(negative), ave_positive = mean(positive), ave_anger = mean(anger))

# Plot the daily sentiment
ggplot(daily, aes(x = TimeStamp, y = ave_sentiment)) + geom_line() +
  ggtitle("United Airline Sentiment: AFINN") + xlab("Date") + ylab("Sentiment") + scale_x_date(date_labels = '%d-%b-%y')


# Get daily summaries of the results - NRC
daily = ddply(tweets, ~ TimeStamp, summarize, num_tweets = length(positive), ave_sentiment = mean(nrc),
              ave_negative = mean(negative), ave_positive = mean(positive), ave_anger = mean(anger))

# Plot the daily sentiment
ggplot(daily, aes(x = TimeStamp, y = ave_sentiment)) + geom_line() +
  ggtitle("United Airline Sentiment: NRC") + xlab("Date") + ylab("Sentiment") + scale_x_date(date_labels = '%d-%b-%y')






################################################################################
#
#                              Issues
#
################################################################################

# 1. Sentiment list completeness, accuracy, and timeliness

# 2. n-grams: e.g., "good" vs "not good"

# 3. Duplicates will skew analysis
