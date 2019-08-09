library(dplyr) 
library(tm)
library(ggmap)
library(ggplot2)
library(twitteR)
library(stringr)
library(wordcloud)
library(lubridate)
library(data.table)
filter(ira_tweets_csv_hashed, tweet_language == "en")
election<-read.csv(file="C:/Users/xiqia/Documents/russia_201901_1_tweets_csv_hashed.csv", header=TRUE, sep=",")
election5.df <-as.data.frame(election)
election6.df$tweet_time <- (election6.df$tweet_time)
ggplot(data = election6.df, aes(x =tweet_time )) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")
