#load libraries
library(tm)
library(SnowballC)
library(e1071)
library(wordcloud)
#load dataset 
hamspam <- read.csv("/home/awaz/Documents/UISCourses/DataScience/NaiveBayes/SMSSpamCollection.csv",stringsAsFactors = FALSE)
str(hamspam)
#convert type to factor
hamspam$Type <- as.factor(hamspam$Type)
#convert the text portion to a corpus
hs_corpus <- VCorpus(VectorSource(hamspam$Text))
#Clean the corpus
hs_corpus_cln <- tm_map(hs_corpus, content_transformer(tolower))
hs_corpus_cln <- tm_map(hs_corpus_cln, removeNumbers)
hs_corpus_cln <- tm_map(hs_corpus_cln, removeWords, stopwords())
hs_corpus_cln <- tm_map(hs_corpus_cln, removePunctuation)
hs_corpus_cln <- tm_map(hs_corpus_cln, stemDocument)
hs_corpus_cln <- tm_map(hs_corpus_cln, stripWhitespace)
#Lets see a word cloud on our clean corpus
wordcloud(hs_corpus_cln,min.freq = 50,random.order = FALSE)
#Create DTM
hs_DTM <- DocumentTermMatrix(hs_corpus_cln)
#Remove words occuring less than 5 times
freq_words <- findFreqTerms(hs_DTM, 5)
hs_DTM_freq <- hs_DTM[,freq_words]
#Convert counts to YES or NO
conv_counts <- function(x){x <- ifelse(x > 0, "YES", "NO")}
hs_DTM_cat <- apply(hs_DTM_freq, MARGIN = 2, conv_counts)
#convert it to a data frame
hs_df <- as.data.frame(hs_DTM_cat)
#split train and test
index <- 1:floor(nrow(hs_DTM)/3)
hs_train <- hs_df[-index,]
hs_test <- hs_df[index,]
hs_train_label <- hamspam$Type[-index]
hs_test_label <- hamspam$Type[index]
#Build model
hs_model <- naiveBayes(hs_train,hs_train_label)
hs_test_pred <- predict(hs_model,hs_test)
#Cross table
library(gmodels)
CrossTable(hs_test_pred,hs_test_label,prop.chisq = FALSE,prop.t = FALSE,dnn=c('predicted','actual'))
#ROC
library(ROCR)
#In order for ROC to function, need to convert ham and spam to 0 and 1
conv_num <- function(x){x <- ifelse(x=="ham",1,0)}
hs_pred_ROC <- sapply(hs_test_pred, conv_num)
hs_label_ROC <- sapply(hs_test_label, conv_num)
pred <- prediction(hs_pred_ROC,hs_label_ROC)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred,measure = "auc")@y.values[[1]]
auc
#Try with a small laplacian correction
hs_model_lap <- naiveBayes(hs_train,hs_train_label,laplace = 1)
hs_test_pred_lap <- predict(hs_model_lap,hs_test)
#Cross table
library(gmodels)
CrossTable(hs_test_pred_lap,hs_test_label,prop.chisq = FALSE,prop.t = FALSE,dnn=c('predicted','actual'))
