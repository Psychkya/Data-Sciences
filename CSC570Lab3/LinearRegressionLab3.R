#had to convert file to csv
boston <- read.csv("D:/UIS courses/Data Sciences/Boston housing/housing.data.csv",header=F);
View(boston);
#assign column names
colnames(boston) <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV");
#split data between train and test
#create a vector with all indexes
index <- 1:nrow(boston)
#create a vector with test indexes = 1/3 of total index
testindex <- sample(index,trunc(length(index)/3))
testindex
#load records with testindexes to a new data frame
boston.test <- boston[testindex,]
str(boston.test)
View(boston.test)
#load rest to train dataset
boston.train <- boston[-testindex,]
str(boston.train)
#include library MASS
library(MASS)
#remove the dependent variable from training set
boston.train1 <- boston.train[,-14]
View(boston.train1)
#correlation matrix
cor(boston)
#linear regression model
lm.model <- lm(boston.train$MEDV~.,data = boston.train1)
#summary
summary(lm.model)
lm.RSqure <- summary(lm.model)$r.squared
#predict with test set
pred.lm <- predict(lm.model, boston.test[,1:13])
View(pred.lm)
#MSE value
lm.MSE <- mean((pred.lm - boston.test[,14])^2)
lm.MSE
#ridge regression
#can be done using lm.ridge or linearridge from 'ridge' package. 'ridge' package is now deprecated, but 
#provides an easier way to predict values. lm.ridge from MASS does not have a predict function.
#we will try linearridge first and then try lm.ridge
library(ridge)
lmridge1.model <- linearRidge(boston.train$MEDV~.,data = boston.train1,lambda = "automatic")
summary(lmridge1.model,all.coef = T)
#did not give me R squared value
#lets get predicted values and MSE
pred.lmridge1 <- predict(lmridge1.model, boston.test[,1:13])
str(pred.lmridge1)
lmridge1.MSE <- mean((pred.lmridge1 - boston.test[,14])^2)
lmridge1.MSE
#Calculating R squared
test.mean <- mean(boston.test[,14])
test.mean
lmridge1.RSquare <- 1 - (sum((boston.test[,14] - pred.lmridge1)^2)/sum((boston.test[,14] - test.mean)^2))
lmridge1.RSquare
#Lets now use lm.ridge() from MASS library
#We will first try with lambda=0
lmridge2.model <- lm.ridge(boston.train$MEDV~.,data = boston.train1,lambda = 0)
#MASS does not support summary
#lm.ridge also does not support predict(). So, we use following process
pred.lmridge2 <- scale(boston.test[,1:13],center = F,scale = lmridge2.model$scales) %*% lmridge2.model$coef
pred.lmridge2
#Calculate MSE
#first find the offset. If we print the model, the first coefficient is the offset
lmridge2.model
#We add the offset to our MSE calc
lmridge2.MSE <- mean((pred.lmridge2 + coef(lmridge2.model)[1]  - boston.test[,14])^2)
lmridge2.MSE
#Calculating R squared
lmridge2.RSquare <- 1 - (sum((pred.lmridge2 + coef(lmridge2.model)[1] - boston.test[,14])^2)/sum((boston.test[,14] - test.mean)^2))
lmridge2.RSquare
#lets arbitrarily chose lambda = 4
lmridge2.modelL4 <- lm.ridge(boston.train$MEDV~.,data = boston.train1,lambda = 4)
lmridge2.modelL4
#predict values
pred.lmridge2L4 <- scale(boston.test[,1:13],center = F,scale = lmridge2.modelL4$scales) %*% lmridge2.modelL4$coef
#calculate MSE
lmridge2.MSEL4 <- mean((pred.lmridge2L4 + coef(lmridge2.modelL4)[1]  - boston.test[,14])^2)
lmridge2.MSEL4
#calculate R-squared
lmridge2.RSquareL4 <- 1 - (sum((pred.lmridge2L4 + coef(lmridge2.modelL4)[1] - boston.test[,14])^2)/sum((boston.test[,14] - test.mean)^2))
lmridge2.RSquareL4
#Lets try with a sequence and see if R can suggest a good lambda
lmridge2.modelFinal <- lm.ridge(boston.train$MEDV~.,data = boston.train1,lambda = seq(0,10,0.01))
select(lmridge2.modelFinal)
lowestGCV <- which.min(lmridge2.modelFinal$GCV)
lowestGCV
#From above we see that R chose a lamda from 0 to 10 incrementing by 0.01 and gave us a GCV value of 5.41. We will use this as our
#final lambda
lmridge2.modelFinal <- lm.ridge(boston.train$MEDV~.,data = boston.train1,lambda = 5.41)
lmridge2.modelFinal
#predict using final model
coef(lmridge2.modelFinal)[1]
pred.lmridge2Final <- scale(boston.test[,1:13],center = F,scale = lmridge2.modelFinal$scales) %*% lmridge2.modelFinal$coef
#calculate MSE
lmridge2.MSEFinal <- mean((pred.lmridge2Final + coef(lmridge2.modelFinal)[1]  - boston.test[,14])^2)
lmridge2.MSEFinal
#calculate R squared
lmridge2.RSquareFinal <- 1 - (sum((pred.lmridge2Final + coef(lmridge2.modelFinal)[1] - boston.test[,14])^2)/sum((boston.test[,14] - test.mean)^2))
lmridge2.RSquareFinal
AllMSE <- c(lm.MSE,lmridge1.MSE,lmridge2.MSE,lmridge2.MSEL4,lmridge2.MSEFinal)
AllMSE
AllRSquared <- c(lm.RSqure, lmridge1.RSquare, lmridge2.RSquare, lmridge2.RSquareL4, lmridge2.RSquareFinal)
df <- data.frame(AllMSE, AllRSquared)
names(df) <- c("MSE","R Squared")
df
`row.names<-`(df, c("lm","ridge1", "ridge2", "ridge2 L4", "ridge2 Final"))
library(knitr)
kable((`row.names<-`(df, c("lm","ridge1", "ridge2", "ridge2 L4", "ridge2 Final"))),format = "markdown")
