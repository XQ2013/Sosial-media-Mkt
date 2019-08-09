library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(dplyr)
library(ggplot2)
library(igraph)
library(twitteR)
library(data.table)
library(tm)
library(stringr)
library(syuzhet)
library(wordcloud)

encodeSentiment <- function(x) {
  if(x <= -0.5){
    "very negative"
  }else if(x > -0.5 & x < 0){
    "negative"
  }else if(x > 0 & x < 0.5){
    "positive"
  }else if(x >= 0.5){
    "very positive"
  }else {
    "neutral"
  }
}

getwd()
data <- read.csv("tweets20167to201611.csv", header = TRUE)
data <- filter(data, account_language == "en" )



# 1.
data$hashtags2 <-tolower(data$hashtags)
data$date <- as.Date(data$tweet_time, format ="%m/%d/%Y %H:%M")


#never
data$never <- str_detect(data$hashtags2, "neverhillary",negate = FALSE )
datanever <-data[!(data$never =="FALSE"), ]

datanever.df <-as.data.frame(datanever)


data1 = datanever.df
data1 <- filter(data1, account_language == "en" )

############################################################################
#             Sentiment Analysis
############################################################################

# connect to twitter app

data1$text <- sapply(data1$tweet_text,function(x) iconv(x,to='UTF-8'))

nohandles <- str_replace_all(data1$text, "@\\w+", "")
wordCorpus <- Corpus(VectorSource(nohandles))
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, removeWords, c("amp"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)

pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]
set.seed(123)
wordcloud(words = wordCorpus, scale=c(5,0.1), max.words=1000, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=pal)


tweetSentiments <- get_sentiment (data1$text,method = "syuzhet")
tweets <- cbind(data1, tweetSentiments)
tweets$sentiment <- sapply(tweets$tweetSentiments,encodeSentiment)

qplot(tweets$tweetSentiments) + theme(legend.position="none")+
  xlab("Sentiment Score") +
  ylab("Number of tweets") + 
  ggtitle("Tweets by Sentiment Score") 

ggplot(tweets, aes(sentiment)) +
  geom_bar(fill = "aquamarine4") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Number of tweets") + 
  ggtitle("Tweets by Sentiment") 



# NRC Sample
tweetSentiments <- get_nrc_sentiment(data1$text)
tweets1 <- cbind(data1, tweetSentiments)

sentimentTotals <- data.frame(colSums(tweets1[,c(35:44)]))

names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL

ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")
