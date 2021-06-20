library("NLP")
library("twitteR")
library("syuzhet")
library("tm")
library("SnowballC")
library("topicmodels")
library("twitteR")

consumer_key <-  "bxtG5D6naPlHN0VcQPr8s2OIp"
consumer_secret <- "1B5VlUPiXfKsXx3vgoXm7BGfMt0ANS9ZgAd1ulA8SDGVpkWWxz"
access_token <- "1395215176203259907-1zn7C5HuAQkZPTT6uNU7rZxbyrYln9"
access_secret <- "iZ5FDFOHsnr0dlKJ1pL5yAg3fDHfqtXNqOcwjUSUvOsEc"


setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
user_name <- readline(prompt="TweeterAccount?  : ")
country <- readline(prompt="Country?  : ")
tweets <- userTimeline(user_name, n=100)
ottawa_tweets <- twListToDF(tweets)
#View((ottawa_tweets))
ottawa_text <- ottawa_tweets$text

#convert all text to lower case
ottawa_text<- tolower(ottawa_text)

# Replace blank space (“rt”)
ottawa_text <- gsub("rt", "", ottawa_text)

# Replace @UserName
ottawa_text <- gsub("@\\w+", "", ottawa_text)


# Remove punctuation
ottawa_text <- gsub("[[:punct:]]", "", ottawa_text)


# Remove links
ottawa_text <- gsub("http\\w+", "", ottawa_text)

# Remove tabs
ottawa_text <- gsub("[ |\t]{2,}", "", ottawa_text)


# Remove blank spaces at the beginning
ottawa_text <- gsub("^ ", "", ottawa_text)


# Remove blank spaces at the end
ottawa_text <- gsub(" $", "", ottawa_text)

sentiment_ottawa<-get_nrc_sentiment((ottawa_text))

#calculationg total score for each sentiment
Sentimentscores_ottawa<-data.frame(colSums(sentiment_ottawa[,]))

names(Sentimentscores_ottawa)<-"Score"
Sentimentscores_ottawa<-cbind("sentiment"=rownames(Sentimentscores_ottawa),Sentimentscores_ottawa)
rownames(Sentimentscores_ottawa)<-NULL
Sentimentscores_ottawa
class(Sentimentscores_ottawa)
library(dplyr)
library(ggplot2)
ggplot(data=Sentimentscores_ottawa,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
    theme(legend.position="none")+
    xlab("Sentiments")+ylab("scores")+ggtitle(sprintf("Sentimentanalysis(@%s,%s)",user_name,country))

Neg <- Sentimentscores_ottawa %>% filter(sentiment == "anger" | sentiment == "disgust" | sentiment == "fear" | sentiment == "sadness" | sentiment == "negative")
neg_score <- Neg %>% summarise(total_score = sum(Score))

Pos <- Sentimentscores_ottawa %>% filter(sentiment == "anticipation" | sentiment == "joy" | sentiment == "surprise" | sentiment == "trust" | sentiment == "positive")
pos_score <- Pos %>% summarise(total_score = sum(Score))

ne <- neg_score$total_score
po <- pos_score$total_score


neg_rate <- ne / (ne + po)
neg_rate
