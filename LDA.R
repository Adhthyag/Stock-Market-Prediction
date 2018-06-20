library(tm)
library(SnowballC)
library(stringi)
library(stringr)
library(topicmodels)
library(xts)
library(ggplot2)
library(topicmodels)

#Read in the training data
tweets_train<-read.csv("final.csv",stringsAsFactors = F)



#Remove Duplicate data
tweets_train<-tweets_train[!duplicated(tweets_train$text),]
#Remove empty rows where there is no entry in the text column and if the entry is just a single word
tweets_train<-tweets_train[nchar(tweets_train$text)>5,]

tweets_train$created<-as.POSIXct(tweets_train$created,tz="Australia/Sydney",format="%Y-%m-%d %H:%M:%S")

tweets_train<-tweets_train[order(tweets_train$created),]



#Create a corpus
tweet_corpus<-Corpus(VectorSource(tweets_train$text))

#Pre-Processing to normalise the texts in the corpus
tweet_corpus<-tm_map(tweet_corpus,content_transformer(tolower))
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
tweet_corpus <- tm_map(tweet_corpus, content_transformer(removeURL))
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
tweet_corpus <- tm_map(tweet_corpus, content_transformer(removeNumPunct))
# remove stopwords
myStopwords <- stopwords('english')
tweet_corpus <- tm_map(tweet_corpus, removeWords, myStopwords)
# remove extra whitespace
tweet_corpus <- tm_map(tweet_corpus, stripWhitespace)

#Find the terms most frequently used
tdm<-TermDocumentMatrix(tweet_corpus)
tdm<-removeSparseTerms(tdm,0.99)
tdm

#Create Document Term Matrix
dtm <- as.DocumentTermMatrix(tdm)
rowTotals <- apply(dtm , 1, sum)
NullDocs <- dtm[rowTotals==0, ]
dtm   <- dtm[rowTotals> 0, ]
dtm


#LDA

lda <- LDA(dtm, k = 7, control = list(seed = 1234))
lda
library(tidytext)
ap_topics <- tidy(lda, matrix = "beta")
ap_topics

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
