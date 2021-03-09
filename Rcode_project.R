#######################################################################################################
########################################### Decision Trees ###################################################
#######################################################################################################


###################################################################
#C5.0 Algorithm
library(C50)
card_data <- read.csv(file = "clean_dataset.csv", header = TRUE)
np <- ceiling(0.3*nrow(card_data))
np

summary(card_data)

# Check attributes of data
str(card_data)

# Check number of rows and columns
dim(card_data)

test.index <- sample(1:nrow(card_data), np)
card.test <- card_data[test.index, ]
card.train <- card_data[-test.index, ]

#C5.0 algorithm control
c <- C5.0Control(subset = FALSE,
                 bands = 0,
                 winnow = FALSE,
                 noGlobalPruning = FALSE,
                 CF = 0.25,
                 minCases = 2,
                 fuzzyThreshold = FALSE,
                 sample = 0,
                 seed = sample.int(4096, size = 1) -1L,
                 earlyStopping = TRUE
)

#convert into factor(required to run C5.0)
card.train$PAY_DEF <- as.factor(card.train$PAY_DEF)

card_treeModel <- C5.0(x = card.train[, -24], y = card.train$PAY_DEF,control =c)
summary(card_treeModel)

#plot 1 (slightly overloaded)
plot(card_treeModel)

# Accuracy with train data
train.output <- predict(card_treeModel, card.train[, -24], type = "class")
train.output
n <- length(train.output)
number = 0
for ( i in 1:n){
  if(train.output[i] == card.train[i, 24])
  {
    number=number+1}
}
train.accuracy = number/n*100
train.accuracy #82.36% accuracy

# Accuracy with test data
test.output <- predict(card_treeModel, card.test[, -24], type = "class")
test.output
n <- length(test.output)
number = 0
for ( i in 1:n){
  if(test.output[i] == card.test[i, 24])
  {
    number=number+1}
}
test.accuracy = number/n*100
test.accuracy #82.67% accuracy


#############################################################
## Algorithm rpart

# Make dependent variable as a factor (categorical)
card_data$PAY_DEF = as.factor(card_data$PAY_DEF)

## ------------------------------------------------------------------------
# Split data into training (70%) and validation (30%)
dt = sort(sample(nrow(card_data), nrow(card_data)*.7))
train<-card_data[dt,]
val<-card_data[-dt,] 

## ------------------------------------------------------------------------

library(rpart)

#Decision tree model 1(simple)

#model
mtree1 <- train(PAY_DEF ~ .,
                data = train,
                method = "rpart")
mtree1

rpart.plot(mtree1$finalModel)

#Decision Tree model 2 (more complex)

mtree2 <- rpart(PAY_DEF~., data = train, method="class", control = rpart.control(cp=0.0019))
mtree2
rpart.plot(mtree2)

#Beautify tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#view1
prp(mtree1$finalModel, faclen = 0, cex = 0.8, extra = 1)
prp(mtree2, faclen = 0, cex = 0.8, extra = 1)

## ------------------------------------------------------------------------
#view2 - total count at each node
tot_count <- function(x, labs, digits, varlen)
{paste(labs, "\n\nn =", x$frame$n)}

prp(mtree1$finalModel, faclen = 0, cex = 0.8, node.fun=tot_count)
prp(mtree2, faclen = 0, cex = 0.8, node.fun=tot_count)

## ------------------------------------------------------------------------
#view3- fancy Plot
library(rattle)
#library(gKt)
#rattle()
fancyRpartPlot(mtree1$finalModel)
fancyRpartPlot(mtree2)

# Advanced Plot
prp(mtree2, main="Decision Tree",
    nn=TRUE, 
    fallen.leaves=TRUE, 
    branch=.5, 
    faclen=0, 
    trace=1, 
    shadow.col="gray", 
    branch.lty=3, 
    split.cex=1.2, 
    split.prefix="is ", 
    split.suffix="?", 
    split.box.col="lightgray", 
    split.border.col="darkgray", 
    split.round=.5)

## ------------------------------------------------------------------------
# confusion matrix (training data)
conf.matrix <- table(train$PAY_DEF, predict(mtree2,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)
Kappa(table(train$PAY_DEF, predict(mtree2,type="class")))

# Accuracy with train data
train.mtree <- predict(mtree2, card.train[, -24], type = "class")
train.mtree
n <- length(train.mtree)
number = 0
for ( i in 1:n){
  if(train.mtree[i] == card.train[i, 24])
  {
    number=number+1}
}
train.ac = number/n*100
train.ac #82.3% accuracy

# Accuracy with test data
test.mtree <- predict(mtree.test, val, type = "class")
test.mtree
n <- length(test.mtree)
number = 0
for ( i in 1:n){
  if(test.mtree[i] == card.test[i, 24])
  {
    number=number+1}
}
test.ac = number/n*100
test.ac #71.62% accuracy

## ------------------------------------------------------------------------
#Scoring

library(ROCR)
library(gplots)

val0 = predict(card_treeModel, card.test, type = "prob")
val0

val2 = predict(mtree.test, val, type = "prob")
val2

#Storing Model Performance Scores

pred_1 <-prediction(val0[,2], card.test$PAY_DEF)
pred_1

pred_3 <-prediction(val2[,2], val$PAY_DEF)
pred_3

# Calculating Area under Curve
perf_val1 <- performance(pred_1,"auc")
perf_val1 #0.694

perf_val3 <- performance(pred_3,"auc")
perf_val3 #0.6869

## I keep pred_3 and not use pred_2, 
# because the result would be repetitive

# Plotting Lift curve
plot(performance(pred_1, measure="lift", x.measure="rpp"), colorize=TRUE)
plot(performance(pred_3, measure="lift", x.measure="rpp"), colorize=TRUE)

# Calculating True Positive and False Positive Rate
perf_1 <- performance(pred_1, "tpr", "fpr")
perf_3 <- performance(pred_3, "tpr", "fpr")

# Plot the ROC curve
plot(perf_1, col = "green", lwd = 1.5)
plot(perf_3, col = "green", lwd = 1.5)

# Calculating KS statistics
ks1.tree <- max(attr(perf_1, "y.values")[[1]] - (attr(perf_1, "x.values")[[1]]))
ks1.tree #0.37 result for C5.0 algorithm
ks3.tree <- max(attr(perf_3, "y.values")[[1]] - (attr(perf_3, "x.values")[[1]]))
ks3.tree # 0.3591079 result for rpart algorithm


#######################################################################################################
########################################### Random Forests ###################################################
#######################################################################################################


#######################################################################################################
## Random forest with train() algorithm (caret)

library(dplyr)

#Step 1- Import Data
card_data <- read.csv(file = "clean_dataset.csv", header = TRUE)

card_data$PAY_DEF <- as.factor(card_data$PAY_DEF)
np <- ceiling(0.3*nrow(card_data))
np

test.index <- sample(1:nrow(card_data), np)
card.test <- card_data[test.index, ]
card.train <- card_data[-test.index, ]

#Step 2 - Loading libraries
library(randomForest)
library(caret)
library(e1071)

#Step 3 - define controls
# Define the control
trControl <- trainControl(method = "cv", number = 10, search = "grid")

#Step 4 - build models with controls

?caret::train

# Run the model

rf_default <- caret::train(PAY_DEF~.,
                           data = card.train,
                           method = "rf",
                           metric = "Accuracy",
                           trControl = trControl,
                           na.action=na.exclude)

# Print the results
print(rf_default)

prediction0 <-predict(rf_default, card.train)
confusionMatrix(prediction0, card.train$PAY_DEF)

#Step 5 - Changing the parameters of caret algorithm to see if accuracy improves

# Search for best mtry

# mtry is number of variables used at each node to perform the splitting

tuneGrid <- expand.grid(.mtry = c(1: 24))
rf_mtry <- caret::train(PAY_DEF~.,
                        data = card.train,
                        method = "rf",
                        metric = "Accuracy",
                        tuneGrid = tuneGrid,
                        trControl = trControl,
                        importance = TRUE,
                        nodesize = 14,
                        ntree = 300,
                        na.action=na.exclude)
print(rf_mtry)

#Step 6 Store best value of mtry
rf_mtry$bestTune$mtry
#2

#Step 7
best_mtry<- rf_mtry$bestTune$mtry 
best_mtry

#Step 8 - search for best max loops
#Maxnodes is the maximum number of levels in the tree

store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(24: 30)) {
  set.seed(1234)
  rf_maxnode <- caret::train(PAY_DEF~.,
                             data = card.train,
                             method = "rf",
                             metric = "Accuracy",
                             tuneGrid = tuneGrid,
                             trControl = trControl,
                             importance = TRUE,
                             nodesize = 14,
                             maxnodes = maxnodes,
                             ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)
##26 whole dataset

#Step 8 - search for best ntrees
# Maximum number of trees

store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- caret::train(PAY_DEF~.,
                              data = card.train,
                              method = "rf",
                              metric = "Accuracy",
                              tuneGrid = tuneGrid,
                              trControl = trControl,
                              importance = TRUE,
                              nodesize = 14,
                              maxnodes = 26,
                              ntree = ntree,
                              na.action=na.exclude)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)
#500

#Step 9 - display best settings for model

fit_rf <- caret::train(PAY_DEF~.,
                       card.train,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       ntree = 500,
                       maxnodes = 26)

#Step 10 evaluate model
prediction <-predict(fit_rf, card.test)
confusionMatrix(prediction, card.test$PAY_DEF)


#######################################################################################################
#Bagging classification trees

install.packages("vcd")

# Load many required packages
library(rpart)
library(adabag)
library(randomForest)
library(partykit)
library(party)
library(vcd)

#Split data into training and test
N <- nrow(card_data)
indtrain <- sample(1:N, size=0.70*N)
indtrain <- sort(indtrain)
indtest <- setdiff(1:N, indtrain)


fit.b <- bagging(PAY_DEF~., data=card_data[indtrain,], control=rpart.control(cp=0.0035))
pred.b <- predict(fit.b, newdata=card_data, type="class")$class

# Test data
table(card_data$PAY_DEF[indtest], pred.b[indtest])
sum(card_data$PAY_DEF[indtest]==pred.b[indtest]) / length(indtest)
Kappa(table(card_data$PAY_DEF[indtest], pred.b[indtest]))

#Training data

table(card_data$PAY_DEF[indtrain], pred.b[indtrain])
sum(card_data$PAY_DEF[indtrain]==pred.b[indtrain]) / length(indtrain)
Kappa(table(card_data$PAY_DEF[indtrain], pred.b[indtrain]))

# Visualising 4 sample trees
par(mfrow=c(2,2))
for (j in 1:4)
{
  plot(fit.b$trees[[j]], main = paste("Example ", j, sep = ""))
  text(fit.b$trees[[j]], use.n=TRUE, xpd=TRUE, col = "red")     
}
par(mfrow=c(1,1))


#######################################################################################################
# Random forest classifier

fit.rf <- randomForest(PAY_DEF~., data=card_data, subset=indtrain)
pred.rf <- predict(fit.rf, newdata=card_data, type="class")

#Test data
table(card_data$PAY_DEF[indtest], pred.rf[indtest])
sum(card_data$PAY_DEF[indtest]==pred.rf[indtest]) / length(indtest)
Kappa(table(card_data$PAY_DEF[indtest], pred.rf[indtest]))

#Training data
table(card_data$PAY_DEF[indtrain], pred.rf[indtrain])
sum(card_data$PAY_DEF[indtrain]==pred.rf[indtrain]) / length(indtrain)
Kappa(table(card_data$PAY_DEF[indtrain], pred.rf[indtrain]))

# Variable of importance plot
varImpPlot(fit.rf, main= "Variable of Importance Plot")


########################################### Packages ###################################################


if (!require("twitteR")) install.packages("twitteR", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("tidytext")) install.packages("tidytext")
if (!require("textdata")) install.packages("textdata")
if (!require("rtweet")) install.packages("rtweet")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("multcomp")) install.packages("multcomp")
if (!require("party")) install.packages("party", dependencies = TRUE)
if (!require("openssl")) install.packages("openssl")
if (!require("httpuv")) install.packages("httpuv")
if (!require("base64enc")) install.packages("base64enc")
if (!require("httr")) install.packages("httr")
if (!require("tm")) install.packages("tm", dependencies = TRUE)
if (!require("wordcloud")) install.packages("wordcloud")
if (!require("topicmodels")) install.packages("topicmodels")
if (!require("SnowballC")) install.packages("SnowballC", dependencies = TRUE)


library(rtweet)
library(twitteR)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(textdata)
library(tidyverse)
library(multcomp)
library(party)
library(openssl)
library(httpuv)
library(base64enc)
library(httr)
library(tm)
library(wordcloud)
library(topicmodels)
library(SnowballC)
library(openssl)
library(httpuv)


############################################ Data #####################################################


## Scrape data from Twitter
# Set up access consumer_key, consume_secret, access_token, and access_secret. 
consumer_key <- "wIlhmTQZImUHs9ic38YA0iudF"
consumer_secret <- "czwejdr2QtKo5ST6Z8mHhBzsoSoXCeupD8UyAX4Vo6gxbjt607"
access_token <- "1294244714020048896-Lk9AXwnG8t2vQxokEEDL3NE4Sh4G5K"
access_secret <- "47tIKmi5k9EV6MCcQdCD21DcKZ7PysHTT0JUt4vvrM9E6"

setup_twitter_oauth(consumer_key, consumer_secret, access_token , access_secret)

# Scrape 1000 tweets containing relevant words and hastags in english

?searchTwitter

tweets1 <- searchTwitter("card default", n=1000, lang="en")
tweets2 <- searchTwitter("#creditscore", n=1000, lang="en")
tweets3 <- searchTwitter("card debt", n=1000, lang="en")

# Convert tweets to DF

card_default <- twListToDF(tweets1) #817 obs
creditscore <- twListToDF(tweets2) #979 obs
card_debt <- twListToDF(tweets3) #1000 obs

# The term that works best for marketing purposes is #credit score,
# so this analysis will be based on that

#######################################################################################################
#################################### Twitter Scrapping ##################################################
#######################################################################################################

#See frequent words 
tweet_words <- creditscore %>% dplyr::select(id, text) %>% unnest_tokens(word,text)
tweet_words

#visually check words (including stop words)
tweet_words %>% count(word,sort=T) %>% slice(1:20) %>% 
  ggplot(aes(x = reorder(word, 
                         n, function(n) -n), y = n)) + geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab("")

## create a list of stop words not worth including

myStopwords <- stop_words %>% dplyr::select(-lexicon) %>% 
  bind_rows(data.frame(word = c("https", "t.co", "rt", "out","the", "to", "on", "a", "this", "you", "of", "his", "and", "i", "be")))

tweet_words_interesting <- tweet_words %>% anti_join(myStopwords)

#visually check words (without stop words)

test_graph <- tweet_words_interesting %>% group_by(word) %>% 
  tally(sort=TRUE) %>% slice(1:25) %>% ggplot(aes(x = reorder
                                                  (word,n,function(n) -n), y = n)) + geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 60,hjust = 1)) + xlab("")
test_graph

# build a corpus, and specify the source to be character vectors

myCorpus <- Corpus(VectorSource(tweet_words_interesting$word))

# convert to lower case

myCorpus <- tm_map(myCorpus, content_transformer(tolower))

# remove URLs

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)

myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

# remove anything other than English letters or space

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

# count word frequence

tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
tdm

# inspect frequent words (appearing more than 25 times)

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >=25)
df <- data.frame(term = names(term.freq), freq = term.freq)

# Visualize result

ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))

#######################################################################################################
####################################### Sentiment Analysis ############################################
#######################################################################################################

# Create copy of working data [Sentiment Analysis Requirement]
tweets <- creditscore %>% dplyr::select(screenName, text)

## Process & Clean text using transformers

# Remove http elements
tweets$stripped_text1 <- gsub("https\\S+","",tweets$text)

# Use unnest_tokens() function to convert to lowercase, remove punctuation and add tweet ID
tweets.stem <- tweets %>% dplyr::select(stripped_text1) %>% 
  unnest_tokens(word, stripped_text1)

# Remove stop words from the list
cleaned_tweets <- tweets.stem %>% anti_join(stop_words)

myStopwords <- stop_words %>% dplyr::select(-lexicon) %>% 
  bind_rows(data.frame(word = c("t.co", "rt", "out","the", "to", "on", "a", "this", "you", "of", "his", 
                                "and", "i", "be")))
cleaned_tweets <- cleaned_tweets %>% anti_join(myStopwords)

## Check Bing Lexicon
# Bing Sentiment
# Positive
get_sentiments("bing") %>% filter(sentiment=="positive")
# Negative
get_sentiments("bing") %>% filter(sentiment=="negative")

## Bing Sentiment Analysis
# score and combine the cleaned tweets with the bing lexicon
bing_tweets <- cleaned_tweets %>% inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% ungroup()

# Visualise Bing Sentiment
bing_tweets %>% group_by(sentiment) %>% top_n(15) %>% ungroup() %>% mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets containing Credit Score", y = "Contribution to Sentiment", x = NULL) + coord_flip() + theme_bw()

# Get sentiment score for each tweet
# Build sentiment calculator function for the bing lexicon
sentiment_bing = function(twt){
  #Perform basic text cleaning & strip https elements
  twt_tbl = tibble(text = twt) %>% 
    mutate(
      stripped_text = gsub("http\\S+","",text)
    ) %>%
    unnest_tokens(word, stripped_text)%>%
    anti_join(stop_words) %>% 
    anti_join(myStopwords) %>%    
    inner_join(get_sentiments("bing")) %>% #remove stop words & merge with bing sentiment
    count(word, sentiment, sort = TRUE) %>% 
    ungroup() %>% #next create a score column, -1 for negative, 1 for positive
    mutate(
      score = case_when(
        sentiment == 'negative'~ n*(-1),
        sentiment =='positve'~ n*1)
    )
  #Calculate Total Score
  sent.score = case_when(
    nrow(twt_tbl)==0~0, #if there are no words, zero score
    nrow(twt_tbl)>0~sum(twt_tbl$score) #otherwise sum pos & neg score
  )
  #This is to keep track of which tweets contained no words at all from the bing list
  zero.type = case_when(
    nrow(twt_tbl)==0~"Type 1",
    nrow(twt_tbl)>0~"Type 2"
  )
  list(score = sent.score, type = zero.type, twt_tbl = twt_tbl)
}

# Apply bing sentiment calculator
tweet_sent <- lapply(tweets$text, function(x){sentiment_bing(x)})

#Results
# Tibble to specify topic, score and type
score_breakdown <- tibble(topic = 'Credit Score', score = unlist(map(tweet_sent, 'score')), type = unlist(map(tweet_sent, 'type')))

# Visualise overall sentiment towards Credit Score
ggplot(score_breakdown, aes(x=score, fill = topic)) + geom_histogram(bins = 15, alpha = 0.6) + 
  facet_grid(~topic) + theme_bw()

#######################################################################################################
####################################### Topic Modelling ###############################################
#######################################################################################################

# We start from the corpus built in previous phases and we remove few other words
# Remove the words "credit", "score" and "creditscore" from corpus

# build a corpus, and specify the source to be character vectors
myCorpus2 <- Corpus(VectorSource(myCorpus))

# convert to lower case
myCorpus2 <- tm_map(myCorpus2, content_transformer(tolower))

# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus2 <- tm_map(myCorpus2, content_transformer(removeURL))

# remove anything other than English letters or space

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus2 <- tm_map(myCorpus2, content_transformer(removeNumPunct))

# remove stopwords
myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),
                 "use", "see", "used", "via", "amp")

myCorpus2 <- tm_map(myCorpus2, removeWords, myStopwords)

# remove extra whitespace
myCorpus2 <- tm_map(myCorpus2, stripWhitespace)

# keep a copy for stem completion later
myCorpusCopy <- myCorpus2

myCorpus2 <- tm_map(myCorpus2, stemDocument) # stem words

myCorpusLB <- tm_map(myCorpus2, removeWords, c("credit", "score", "creditscore", "creditscor"))

#stem the document
myCorpusLB <- tm_map(myCorpusLB, stemDocument)

# Replace incomplete words
replaceWord <- function(corpus, oldword, newword) {
  tm_map(corpus, content_transformer(gsub), pattern = oldword, replacement = newword)
}

myCorpusLB <- replaceWord(myCorpusLB, "busi", "business")
myCorpusLB <- replaceWord(myCorpusLB, "financ", "finance")
myCorpusLB <- replaceWord(myCorpusLB, "improv", "improve")
myCorpusLB <- replaceWord(myCorpusLB, "mortgag", "mortgage")
myCorpusLB <- replaceWord(myCorpusLB, "websit", "website")
myCorpusLB <- replaceWord(myCorpusLB, "readi", "read")
myCorpusLB <- replaceWord(myCorpusLB, "appli", "apply")

#create document term matrix
dtm1 <- TermDocumentMatrix(myCorpusLB, control = list(wordLengths = c(1, Inf)))
dtm1
inspect(dtm1)

# Find the most frequent terms
freq.terms<-findFreqTerms(dtm1, lowfreq = 20)
termFreq<-rowSums(as.matrix(dtm1))
termFreq<-subset(termFreq, termFreq>=20)
df<-data.frame(term=names(termFreq), freq=termFreq)

# List of most frequent terms
df 

#find most frequent words
m<-as.matrix(dtm1)
word.freq<-sort(rowSums(m), decreasing=T)
word.freq

#create word cloud
pal1<-brewer.pal(12, "BuGn")[-(1:4)]
wordcloud(words=names(word.freq), freq=word.freq,min.freq = 3, 
          random.order = F, colors = pal1)

#include in the Document term matrix only documents with at least one non 0 entry
dtmLDA<-as.DocumentTermMatrix(dtm1)
doc.length = apply(dtmLDA, 1, sum)
dtmLDA = dtmLDA[doc.length > 0,]
dtmLDA

#create LDA (2 topics)
lda_2=LDA(dtmLDA, k = 2, method = 'Gibbs', 
          control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                         thin = 500, burnin = 4000, iter = 2000))

#LDA model with 3 topics selected
lda_3 = LDA(dtmLDA, k = 3, method = 'Gibbs', 
            control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                           thin = 500, burnin = 4000, iter = 2000))

#LDA model with 4 topics selected
lda_4 = LDA(dtmLDA, k = 4, method = 'Gibbs', 
            control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                           thin = 500, burnin = 4000, iter = 2000))

#select top 10 most frequent words under each topic

top10terms_2 = as.matrix(terms(lda_2,10))
top10terms_3 = as.matrix(terms(lda_3,10))
top10terms_4 = as.matrix(terms(lda_4,10))

#display top 10 most frequent words by topic

top10terms_2
top10terms_3
top10terms_4
top10terms_5
top10terms_6

#search number of words belonging to each topic and display it

lda.topics_2 = as.matrix(topics(lda_2))
lda.topics_3=as.matrix(topics(lda_3))
lda.topics_4=as.matrix(topics(lda_4))

#lda.topics_10 = as.matrix(topics(lda_10))
summary(as.factor(lda.topics_2[,1]))

summary(as.factor(lda.topics_3[,1]))
