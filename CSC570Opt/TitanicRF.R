titanic.tr <- read.csv('~/Documents/UISCourses/DataScience/train.csv')
str(titanic.tr)
#quick random forest
#drop name and ticket number
titanic.quick <- titanic.tr[,-which(names(titanic.tr) %in% c("Name","Ticket"))]
#quick look at cabins
#drop the numeric part and just make it alpha
#for (i in 1:nrow(titanic.quick)) {
#  titanic.quick$Cabin[i] <- substr(titanic.quick$Cabin[i],1,1)  
#}
#well, cabin has lot of missing values, lets drop it for initial model
titanic.quick <- titanic.quick[,-which(names(titanic.quick) %in% c("Cabin"))]
#age got bunch of stuff missing - attempt random forest to first predict age
titan.age.train <- titanic.quick[!is.na(titanic.quick$Age),]
str(titan.age.train)
titan.age.test <- titanic.quick[is.na(titanic.quick$Age),]
str(titan.age.test)
#save age and remove
titan.age.train.orig <- titan.age.train[,which(names(titan.age.train) %in% c("Age"))]
titan.age.test.orig <- titan.age.test[,which(names(titan.age.test) %in% c("Age"))]
titan.age.train1 <- titan.age.train[,-which(names(titan.age.train) %in% c("Age"))]
titan.age.test1 <- titan.age.test[,-which(names(titan.age.test) %in% c("Age"))]
#build model
library(randomForest)
model <- randomForest(titan.age.train$Age~.,data = titan.age.train1,ntree=1000)
results <- predict(model,titan.age.test1)
results <- trunc(results)
titan.age.test$Age <- results
str(titan.age.test)
titan.age.det <- rbind(titan.age.train,titan.age.test)
#fill missing age now
titanic.quick$Age <- titan.age.det$Age
#split test and train
index <- sample(nrow(titanic.quick),nrow(titanic.quick)/3)
titanic.test <- titanic.quick[index,]
titanic.train <- titanic.quick[-index,]
str(titanic.train)
titanic.train1 <- titanic.train[,-which(names(titanic.train) %in% c("Survived"))]
titanic.test1 <- titanic.test[,-which(names(titanic.test) %in% c("Survived"))]
model.t <- randomForest(as.factor(titanic.train$Survived)~.,data = titanic.train1,ntree=2001,replace=TRUE,sampsize=nrow(titanic.train1),importance=TRUE)
model.t
titanic.results <- predict(model.t,titanic.test1,type = 'prob')[,2]
library(ROCR)
str(titanic.results)
pred <- prediction(titanic.results,titanic.test$Survived)
auc <- performance(pred,measure = "auc")@y.values[[1]]
auc
#Retry with some feature engineering
titanic.ft <- titanic.tr
#Lets deal with name. Original intent is to determine married, single - but for now leaving it as title
name.v <- as.character(titanic.ft$Name)
strsplit(name.v[1], split='[,.]')[[1]][2]
#function to split names. courtesy Trevor Stephens
separate <- function(x){strsplit(x, split='[,.]')[[1]][2]}
name.t <- lapply(name.v, separate)
name.t <- unlist(name.t)
name.t <- trimws(name.t,which = c("both"))
table(name.t)
#convert Mlle and Ms to Miss
name.t[name.t %in% c("Mlle", "Ms")] <- "Miss"
#convert Mme to Mrs
name.t[name.t=="Mme"] <- "Mrs"
#convert Jonkheer, Countess to lady
name.t[name.t %in% c("Jonkheer","the Countess")] <- "Lady"
#convert Capt, col, Don, Dr, Major to Sir
name.t[name.t %in% c("Capt", "Col", "Don", "Major")] <- "Sir"
table(name.t)
titanic.ft$title <- as.factor(name.t)
str(titanic.ft)
#Next deal with family size - small, medium, large
#Add sibsp and parch - if total less than 3, small. Between 3 and 5 - medium. Larger than 5, large family
fam.size <- titanic.ft$SibSp + titanic.ft$Parch
famsize <- function(x){if(x < 3) "small" else if(x > 2 & x <6) "medium" else "large"}
family.size <- sapply(fam.size, famsize)
str(family.size)
titanic.ft$familysize <- as.factor(family.size)
str(titanic.ft)
#could have dealt with age in the original dataset, but lest fix age again
#age got bunch of stuff missing - attempt random forest to first predict age
titan.age.train <- titanic.ft[!is.na(titanic.ft$Age),]
str(titan.age.train)
titan.age.test <- titanic.ft[is.na(titanic.ft$Age),]
str(titan.age.test)
#remove age, cabin and name frm a temporary seet
titan.age.train1 <- titan.age.train[,-which(names(titan.age.train) %in% c("Age","Cabin", "Name", "Ticket"))]
titan.age.test1 <- titan.age.test[,-which(names(titan.age.test) %in% c("Age","cabin","Name", "Ticket"))]
#build model
library(randomForest)
model <- randomForest(titan.age.train$Age~.,data = titan.age.train1,ntree=1000)
results <- predict(model,titan.age.test1)
results <- trunc(results)
titan.age.test$Age <- results
str(titan.age.test)
titan.age.det <- rbind(titan.age.train,titan.age.test)
#fill missing age now
titanic.ft$Age <- titan.age.det$Age
str(titanic.ft)
#we can drop Name from the dataset now
titanic.ft <- titanic.ft[,-which(names(titanic.ft) %in% c("Name"))]
#lets look at cabin, ticket price and ticket number and see if we can derive a feature
titanic.cabin.price <- titanic.ft[,which(names(titanic.ft) %in% c("Fare","Cabin", "Ticket"))]
View(titanic.cabin.price)
#Interest of time, lets see if we can create categories based on fare. Mean ticket price is $32 while median is $14
#perhaps, anything greater than 100, we call class A, 100 to 60 we can class B, 59 to 32 we call class C, 32 to 15 we call class D
#anything less, we call class E
cclass <- function(x){ if(x > 100) "A" else if(x <= 100 & x > 59) "B" else if (x <= 59 & x > 31) "C" else if (x <= 31 & x > 14) "D" else "E"}
cabin.cl <- sapply(titanic.ft$Fare, cclass)
cabin.cl
titanic.ft$cabinclass <- as.factor(cabin.cl)
str(titanic.ft)
#Lets now drop cabin and ticket number
titanic.ft <- titanic.ft[,-which(names(titanic.ft) %in% c("Cabin","Ticket"))]
#Lets now build our model after some feature engineering
#split test and train
set.seed(100)
index2 <- sample(nrow(titanic.ft),nrow(titanic.ft)/3)
titanic.test.ft <- titanic.ft[index2,]
titanic.train.ft <- titanic.ft[-index2,]
str(titanic.train.ft)
titanic.train.ft1 <- titanic.train.ft[,-which(names(titanic.train.ft) %in% c("Survived","cabinclass"))]
str(titanic.train.ft1)
titanic.test.ft1 <- titanic.test.ft[,-which(names(titanic.test.ft) %in% c("Survived","cabinclass"))]
rf.tune <-tuneRF(titanic.train.ft1,as.factor(titanic.train.ft$Survived),ntreeTry = 50,stepFactor = 1,improve = 0.05)
rf.tune
model.ft <- randomForest(as.factor(titanic.train.ft$Survived)~.,mtry=3,data = titanic.train.ft1,ntree=3001,replace=TRUE,sampsize=nrow(titanic.train.ft1),importance=FALSE)
model.ft
titanic.results.ft <- predict(model.ft,titanic.test.ft1,type = 'prob')[,2]
library(ROCR)
str(titanic.results)
pred.ft <- prediction(titanic.results.ft,titanic.test.ft$Survived)
auc.ft <- performance(pred.ft,measure = "auc")@y.values[[1]]
auc.ft
library(party)
model.ft2 <- cforest(as.factor(titanic.train.ft$Survived)~.,data=titanic.train.ft1,controls=cforest_unbiased(ntree=2001,mtry=3))
titanic.results.ft2 <- predict(model.ft2,titanic.test.ft1,OOB=TRUE,type='response')
pred.ft2 <- prediction(as.numeric(titanic.results.ft2),titanic.test.ft$Survived)
auc.ft2 <- performance(pred.ft2,measure = "auc")@y.values[[1]]
auc.ft2
