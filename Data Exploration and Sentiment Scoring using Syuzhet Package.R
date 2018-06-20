library(stringi)
library(stringr)
library(wordcloud)
library(tidytext)
library(reshape2)
library(tm)
library(SnowballC)
library(topicmodels)
library(syuzhet)
library(ggplot2)
library(plyr)
library(dplyr)

#Read in the training data
tweets_train<-read.csv("train.csv",stringsAsFactors = F)
tweets_test<-read.csv("test.csv",stringsAsFactors = F)



#Text Processing Function
text_cleaning<-function(test){
# Removes RT
test = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", test)
#Remove non-ASCII characters
Encoding(test) = "latin1"
test<-iconv(test, "latin1", "ASCII", sub = "")
# Removes @<twitter handle>
test = gsub("@\\w+", "", test)
#Replace smiley with text
smiles <- data.frame(s=c(":<",":>",":)",":(",";)",":D",";D",":P",":'("),
                     r=c("unhappy","happy","happy","unhappy","happy","happy","happy","happy","sad"))
## replace smiley with text
test <- stri_replace_all_fixed(test,pattern = smiles$s,replacement = smiles$r,vectorize_all = FALSE)

# Removes punctuations
test=gsub("(?!')[[:punct:]]", "",test, perl=TRUE)

# remove links http
test = gsub("http\\w+", " ", test) 
# Removes html links
test = gsub("http\\w+", " ", test)
# Removes numbers
test= gsub("[[:digit:]]", " ", test)
# Fix for error related to formatting 'utf8towcs'"
test <- str_replace_all(test,"[^[:graph:]]", " ")
#Converting text to lower case
test<-tolower(test)
#Remove Extra White Space
test<-stripWhitespace(test)
}
test<-tweets_train$text
test<-text_cleaning(test)
tweets_train$text<-test

test<-tweets_test$text
test<-text_cleaning(test)
tweets_test$text<-test

#Remove Duplicate Train data
tweets_train<-tweets_train[!duplicated(tweets_train$text),]

#Remove Duplicate Test data
tweets_test<-tweets_test[!duplicated(tweets_test$text),]
#Remove empty rows where there is no entry in the text column and if the entry is just a single word
tweets_train<-tweets_train[nchar(tweets_train$text)>5,]

#Remove empty rows in Test Data
tweets_test<-tweets_test[nchar(tweets_test$text)>5,]

#Format Date and Time for train data
tweets_train$created<-as.POSIXct(tweets_train$created,tz="Australia/Sydney",format="%Y-%m-%d %H:%M:%S")

#Format Date and Time for Test data
tweets_test$created<-as.POSIXct(tweets_test$created,tz="Australia/Sydney",format="%Y-%m-%d %H:%M:%S")



#Write Test data
write.csv(tweets_test,file = "test.csv",row.names = F)

tweets_train<-tweets_train[order(tweets_train$created),]


#Remove customised stop words:
stopwords = c("cloud","drones","sap","excel")
tweets_train$text <- gsub(paste0(stopwords,collapse = "|"),"", tweets_train$text)

ctweets <- tweets_train
#Generate Tokens
word_token<-ctweets
head(word_token)
word_token<-word_token %>% unnest_tokens(word,text)
word_token$created<-NULL
#Remove Stop Words and create a wordcloud
word_token %>% anti_join(stop_words) %>% count(word) %>%
  with(wordcloud(word,n,max.words=100,scale=c(1,1),vfont=c("sans serif","plain"),colors=palette()))

#Find the most common words
most_common<- word_token %>% anti_join(stop_words) %>% count(word,sort = TRUE)

#Plot the most common words
most_common %>%filter(n>3000) %>% mutate(word=reorder(word,n)) %>% ggplot(aes(word,n))+geom_col()+
  xlab(NULL)+ coord_flip()

#Find the most common Positive and negative words using the Bing Lexicon
bing_word_counts <- word_token %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>% ungroup()

#Plot for the most common Positive and Negative words
bing_word_counts %>% group_by(sentiment) %>% top_n(25) %>% ungroup() %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",x = NULL) +coord_flip()

#Word cloud with the positive and negative words 
cloud_2<-word_token %>% 
  inner_join(get_sentiments("bing")) %>%
        count(word,sentiment,sort=T) %>%
        acast(word ~ sentiment,value.var="n",fill=0)%>%
  comparison.cloud(colors=c("gray20","gray80"),scale=c(0.7,0.7),max.words=100)

twit_vec<-get_sentences(tweets_train$text)

scoreSentiment = function(tab)
{
  syuzhet = get_sentiment(tab, method="syuzhet")
  bing = get_sentiment(tab, method="bing")
  afinn = get_sentiment(tab, method="afinn")
  nrc = get_sentiment(tab, method="nrc")
  sentiments <- data.frame(syuzhet, bing, afinn, nrc)
}

# get the sentiment scores for the tweets
twit <- scoreSentiment(twit_vec)

table(twit$nrc)
table(twit$syuzhet)
table(twit$bing)

tweets_train$sentiment<-twit$syuzhet


tweets_train$sentiment <- ifelse(tweets_train$sentiment < 0, 0, 4)


#Write Train data with sentiments
write.csv(tweets_train,file = "train_senti.csv",row.names = F)




with(tweets_train,plot(created,sentiment,type="l"))
abline(h=median(twit$syuzhet),lty=2,col="green")
with(twit,plot(Timestamp,bing,type="l"))
abline(h=median(twit$bing),lty=2,col="red")
with(twit,plot(Timestamp,afinn,type="l"))
abline(h=median(twit$afinn),lty=2,col="blue")
with(twit,plot(Timestamp,nrc,type="l"))
abline(h=median(twit$nrc),lty=2,col="orange")

hist(tweets_train$sentiment)
#abline(v=median(twit$syuzhet))


ggplot(data=tweets_train)+geom_freqpoly(mapping=aes(x=sentiment),binwidth=0.1)

tweets_train %>% filter(date=="2018-05-24") %>% 
  ggplot(data=tweets_train)+geom_histogram(mapping=aes(x=sentiment),binwidth = 0.5)
test<-tweets_train
test$created<-as.Date(tweets_train$created)
test<-tweets_train%>%filter(date="2018-05-24")
ggplot(data=test)+geom_histogram(mapping=aes(x=sentiment),binwidth = 0.5)
test<-cbind(tweets_train,twit)
test$created<-NULL
test$Timestamp<-as.Date(test$Timestamp,'%m/%d/%Y')
test_1<-test %>% filter(Timestamp=="2018-04-22")
hist(test_1$nrc) 


