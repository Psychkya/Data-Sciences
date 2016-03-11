#0.  Using breast_cancer.csv, create a random forest model that predicts malignant given the other relevant variables.  
#      Use a single holdout (test/train split).  Use Grid Search to optimize model hyperparameters.  
#       Measure the model's performance using AUC, Accuracy, Precision, and Recall.
#1.  Implement K-Fold Cross Validation, with 10 folds, on your Breast Cancer Model
#2.  Report on how the K-Fold CV score compared to your single holdout AUC
#3.  Write a short description of your model's performance.   Include AUC, Accuracy, Precision, and Recall in your discussion.
#Turn in a github link to your ipython notebook, containing the above three items.
bcancer <- read.csv('~/Documents/UISCourses/DataScience/breast_cancer.csv')
str(bcancer)
library(caret)
library(randomForest)
#check for nas
set.seed(100)
index <- sample(nrow(bcancer),nrow(bcancer)/3)
bcancer.test <- bcancer[index,]
bcancer.train <- bcancer[-index,]
bcancer.train1 <- bcancer.train[,-12]
bcancer.test1 <- bcancer.test[,-12]
bcancer.rf.grid <- train(as.factor(bcancer.train$malignant)~.,data = bcancer.train1,method = "rf",
                         allowParallel=TRUE)
bcancer.rf.grid
#predict and calc AUC
results.try1 <- predict(bcancer.rf.grid$finalModel, bcancer.test1, type='prob')[,2]
library(ROCR)
pred <- prediction(results.try1, bcancer.test$malignant)
auc <- performance(pred,measure = "auc")@y.values[[1]]
auc
#Not sure if there is a separate cross validation method applicable for random forest in R. 
#I am going to attempt two different ways
#First, caret itself has a way to define a train control for cv
bcancer.rf.cv <- train(as.factor(bcancer.train$malignant)~.,data = bcancer.train1,method = "rf",
                       trControl=trainControl(method="cv",number=10),allowParallel=TRUE)
results.try2 <- predict(bcancer.rf.cv$finalModel,bcancer.test,type='prob')[,2]
pred2 <- prediction(results.try2,bcancer.test$malignant)
auc2 <- performance(pred2,measure="auc")@y.values[[1]]
auc2
prec2 <- performance(pred2,"prec","rec")
plot(prec2)
#Next create a CV manually by looping (courtesy a random blog video in r-bloggers)
k <- 10
n <- floor(nrow(bcancer)/k)
v.auc <- rep(NA,k)
for (i in 1:k) {
  s1 <- (i-1)*n + 1
  s2 <- i*n
  index1 <- s1:s2
  index1
  cv.train <- bcancer[-index1,]
  cv.train1 <- cv.train[,-12]
  cv.test <- bcancer[index1,]
  cv.test1 <- cv.test[,-12]
  rf.cv <- randomForest(as.factor(cv.train$malignant) ~., data=cv.train1,ntree=1001,replace=TRUE,sampsize=nrow(cv.train1),importance=TRUE)
  results <- predict(rf.cv,cv.test1,type='prob')[,2]
  pval <- prediction(results,cv.test$malignant)
  v.auc[i] <- performance(pval,measure="auc")@y.values[[1]]
  pprec <- performance(pval,"prec","rec")
  pprec@y.values[[1]][1] <- 0
  cat("AUC",i,": ", v.auc[i],"; Average recall: ", mean(pprec@x.values[[1]]), " Average precision: ", mean(pprec@y.values[[1]]), " \n")
}
cat("Average AUC: ",mean(v.auc))
v.prec
