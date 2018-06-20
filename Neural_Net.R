library(data.table)
library(tidyverse)
library(text2vec)
library(caret)
library(glmnet)
library(ggrepel)
library(purrrlyr)
tweets_1<-read.csv("train_senti.csv",stringsAsFactors = F)
colnames(tweets_1)[which(names(tweets_1) == "created")] <- "date"

tweets_test<-read.csv("test.csv",stringsAsFactors = F)
colnames(tweets_test)[which(names(tweets_test) == "created")] <- "date"

conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")

tweets_train<-tweets_1 %>% # converting some symbols
  dmap_at('text', conv_fun) %>%
  # replacing class values
  mutate(sentiment = ifelse(sentiment == 0, 0, 1))


tweets_test<-tweets_test%>%dmap_at('text', conv_fun)


##### doc2vec #####
# define preprocessing function and tokenization function
prep_fun <- tolower
tok_fun <- word_tokenizer

it_train <- itoken(tweets_train$text, 
                   preprocessor = prep_fun, 
                   tokenizer = tok_fun,
                   progressbar = TRUE)
it_test <- itoken(tweets_test$text, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun,
                  progressbar = TRUE)


# creating vocabulary and document-term matrix
vocab <- create_vocabulary(it_train)
vectorizer <- vocab_vectorizer(vocab)
dtm_train <- create_dtm(it_train, vectorizer)
dtm_test <- create_dtm(it_test, vectorizer)
# define tf-idf model
tfidf <- TfIdf$new()
# fit the model to the train data and transform it with the fitted model
dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
dtm_test_tfidf <- fit_transform(dtm_test, tfidf)

# train the model
t1 <- Sys.time()
glmnet_classifier <- cv.glmnet(x = dtm_train_tfidf, y = tweets_train[['sentiment']], 
                               family = 'binomial', 
                               # L1 penalty
                               alpha = 1,
                               # interested in the area under ROC curve
                               type.measure = "auc",
                               # 5-fold cross-validation
                               nfolds = 5,
                               # high value is less accurate, but has faster training
                               thresh = 1e-3,
                               # again lower number of iterations for faster training
                               maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'mins'))

plot(glmnet_classifier)
print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))

preds <- predict(glmnet_classifier, dtm_test_tfidf, type = 'response')[ ,1]

# adding rates to initial dataset
tweets_test$sentiment <- preds
# save the model for future using
saveRDS(glmnet_classifier, 'glmnet_classifier.RDS')


df_tweets<-tweets_test



# color palette
cols <- c("#ce472e", "#f05336", "#ffd73e", "#eec73a", "#4ab04a")

set.seed(932)
samp_ind <- sample(c(1:nrow(df_tweets)), nrow(df_tweets) * 0.1) # 10% for labeling

# plotting

ggplot(df_tweets, aes(x = date, y = sentiment, color = sentiment)) +
  theme_minimal() +
  scale_color_gradientn(colors = cols, limits = c(0, 1),
                        breaks = seq(0, 1, by = 1/4),
                        labels = c("0", round(1/4*1, 1), round(1/4*2, 1), round(1/4*3, 1), round(1/4*4, 1)),
                        guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
  geom_point(aes(color = sentiment), alpha = 0.8) +
  geom_hline(yintercept = 0.65, color = "#4ab04a", size = 1.5, alpha = 0.6, linetype = "longdash") +
  geom_hline(yintercept = 0.35, color = "#f05336", size = 1.5, alpha = 0.6, linetype = "longdash") +
  geom_smooth(size = 1.2, alpha = 0.2) +
  geom_label_repel(data = df_tweets[samp_ind, ],
                   aes(label = round(sentiment, 2)),
                   fontface = 'bold',
                   size = 2.5,
                   max.iter = 100) +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
        axis.text.x = element_text(size = 8, face = "bold", color = 'black')) +
  ggtitle("Tweets Sentiment rate (probability of positiveness)")






