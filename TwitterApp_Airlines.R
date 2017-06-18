################################################################################
#
#                     Prepare the R environment
#
################################################################################


install.packages("twitteR") # should also install bit, bit64, rjson
install.packages("httr")
install.packages("devtools")
install.packages("tm")
install.packages("stringr")


library(bit)
library(bit64)
library(devtools)
library(httr)
library(rjson)
library(stringr)
library(tm)
library(twitteR)

################################################################################
#
#                        Import Sentiment Files
#
################################################################################

posText <- read.delim("./words-positive.txt", header = FALSE, stringsAsFactors = FALSE)
typeof(posText)

posText <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))
typeof(posText)
length(posText)


negText <- read.delim("./words-negative.txt", header = FALSE, stringsAsFactors = FALSE)
typeof(negText)

negText <- unlist(lapply(negText, function(x) { str_split(x, "\n") }))
typeof(negText)
length(negText)


pos.words = c(posText, 'upgrade')

neg.words = c(negText, 'wtf', 'wait', 'waiting','epicfail', 'mechanical')



################################################################################
#
#               Connect with Twitter and text the connection
#
################################################################################

api_key <- "aep7QPBd7NJ1m1ZSsodG5LBC4"

api_secret <- "OBtMcFkaaHPJKkqoORZoaeB8446BPoc7lqg76v6B0j5aereQlh"

access_token <- "872555012655894528-koPl407v9Py4oPtbj70KmGPygMrAGx0"

access_token_secret <- "OfTEGVMcqbGvWpudCqdQE2r6g8zSapqh0PhpbuieKqHB3"

setup_twitter_oauth(api_key,api_secret, access_token, access_token_secret)

delta_tweets = searchTwitter('@delta', n = 500, lang = "en")
jetblue_tweets = searchTwitter('@jetblue', n = 500, lang = "en")
united_tweets = searchTwitter('@united', n = 500, lang = "en")

delta_txt = sapply(delta_tweets, function(t) t$getText() )
jetblue_txt = sapply(jetblue_tweets, function(t) t$getText() )
united_txt = sapply(united_tweets, function(t) t$getText() )

noof_tweets = c(length(delta_txt), length(jetblue_txt),length(united_txt))
noof_tweets

airline <- c(delta_txt,jetblue_txt,united_txt)
airline






