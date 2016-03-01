titanic.df <- read.csv("/home/awaz/Documents/UISCourses/DataScience/train.csv")
#remove passenger ID
titanic.df <- titanic.df[-1]
#Name is not relevant and so also ticket number; cabin may not be as relevant - we will drop these 3
exclude.col <- c("Name","Ticket","Cabin")
titanic.mod <- titanic.df[,-which(names(titanic.df) %in% exclude.col)]
str(titanic.mod)
sum(is.na(titanic.mod$Survived))
sum(is.na(titanic.mod$Pclass))
sum(is.na(titanic.mod$Sex))
sum(is.na(titanic.mod$Age))
sum(is.na(titanic.mod$SibSp))
sum(is.na(titanic.mod$Parch))
sum(is.na(titanic.mod$Fare))
sum(is.na(titanic.mod$Embarked))
#from above we see lot of missing ages, lets try a regression model to assign an age
#create df with missing age removed. It appears, linear reg does not like factors
titan.lr.df <- read.csv("/home/awaz/Documents/UISCourses/DataScience/train.csv", stringsAsFactors = FALSE)
titan.lr.df$Sex[titan.lr.df$Sex == "male"] <- 0
titan.lr.df$Sex[titan.lr.df$Sex == "female"] <- 1
titan.lr.df <- titan.lr.df[,-which(names(titan.lr.df) %in% c("Name","Ticket","Cabin","Embarked"))]
titan.lr.df <- titan.lr.df[-1]
titan.lr.df <- transform(titan.lr.df,Sex = as.integer(Sex))
str(titan.lr.df)
View(titan.lr.df)
titan.age.train <- na.omit(titan.lr.df, cols = "Age")
titan.age.ped <- subset(titan.lr.df, is.na(titan.lr.df$Age))
str(titan.age.train)
str(titan.age.ped)
#train without dependent var
titan.age.train1 <- titan.age.train[,-which(names(titan.age.train) %in% c("Age"))]
str(titan.age.train1)
titan.age.ped1 <- titan.age.ped[,-which(names(titan.age.ped) %in% c("Age"))]
str(titan.age.ped1)
#ridge linear using ridgelm
library(ridge)
lmridge.model <- linearRidge(titan.age.train$Age~.,data=titan.age.train1,lamda="automatic")
lmridge.model
#predict age for the missing ones
titan.age.predict <- predict(lmridge.model,titan.age.ped1)
titan.age.predict <- round(titan.age.predict, digits = 0)
titan.age.ped$Age <- titan.age.predict
View(titan.age.ped)
titan.age.determined <- rbind(titan.age.train,titan.age.ped)
str(titan.age.determined)
titanic.mod$Age <- titan.age.determined$Age
View(titanic.mod)
#---------#
#split test and train
index <- 1:nrow(titanic.df)
testindex <- sample(index,trunc(length(index)/3))
str(testindex)
titanic.test <- titanic.mod[testindex,]
titanic.train <- titanic.mod[-testindex,]
#save and remove dependent variable from test
titanic.test.label <- titanic.test[,1]
titanic.test.label
titanic.test <- titanic.test[,-1]
str(titanic.test)
#use glm model
titanic.model <- glm(titanic.train$Survived~.,family = binomial(link = 'logit'),data = titanic.train)
summary(titanic.model)
#predict
predicted.results <- predict(titanic.model,newdata = titanic.test,type = 'response')
#measure auc
library(ROCR)
pred <- prediction(predicted.results,titanic.test.label)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred,measure = "auc")
auc <- auc@y.values[[1]]
auc
