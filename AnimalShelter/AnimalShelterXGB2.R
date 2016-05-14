shelter.trn <- read.csv('/home/awaz/Documents/UISCourses/DataScience/AnimalShelter/train.csv')
shelter.tst <- read.csv('/home/awaz/Documents/UISCourses/DataScience/AnimalShelter/test.csv')
str(shelter.trn)
str(shelter.tst)
#Outcome subtype is not available in test. So lets remove it from train along with ID
shelter.train <- shelter.trn[,-c(1,5)]
#Do the same for test
shelter.test <- shelter.tst[,-1]
#Add Outcometype to test set and fill it with NA
shelter.test$OutcomeType <- NA
#Reorder test to be same as train
shelter.test <- shelter.test[,c("Name","DateTime","OutcomeType","AnimalType", "SexuponOutcome","AgeuponOutcome","Breed","Color")]
#Combine the two dataframes for cleansing
shelter.df <- rbind(shelter.train,shelter.test)
#Starting from bottom, splitting colors
for (i in 1:nrow(shelter.df)) {
  shelter.df$Color1[i] <- strsplit(as.character(shelter.df$Color[i]),"/")[[1]][1]
  shelter.df$Color2[i] <- strsplit(as.character(shelter.df$Color[i]),"/")[[1]][2]
}
#Change NA in color2 to a factor
shelter.df$Color2[is.na(shelter.df$Color2)] <- "Not Applicable"
#We are good with color - now deal with breed
for (i in 1:nrow(shelter.df)) {
  shelter.df$Breed1[i] <- strsplit(as.character(shelter.df$Breed[i]),"/")[[1]][1]
  shelter.df$Breed2[i] <- strsplit(as.character(shelter.df$Breed[i]),"/")[[1]][2]
}
#Convert NA in Breed2 to 'Not Applicable'
shelter.df$Breed2[is.na(shelter.df$Breed2)] <- "Not Applicable"
shelter.df$Breed1[grep("Pit Bull", shelter.df$Breed1)] <- "Pit Bull"
shelter.df$Breed1[grep("Staffordshire", shelter.df$Breed1)] <- "Pit Bull"
shelter.df$Breed1[grep("Black", shelter.df$Breed1)] <- "Black Mouth Cur"
shelter.df$Breed1[grep("Chihuahua", shelter.df$Breed1)] <- "Chihuahua"
#Create new features
breedhw <- read.csv('/home/awaz/Documents/UISCourses/DataScience/AnimalShelter/BreedsizeWeightHeight.csv',stringsAsFactors = FALSE)
breedhw$Weight.M <- as.numeric(breedhw$Weight.M)
breedhw$Weight.M[is.na(breedhw$Weight.M)] <- 0
breedhw$Height.M <- as.numeric(breedhw$Height.M)
breedhw$Height.M [is.na(breedhw$Height.M)] <- 0
breedhw$Weight.F <- as.numeric(breedhw$Weight.F)
breedhw$Weight.F[is.na(breedhw$Weight.F)] <- 0
breedhw$Height.F <- as.numeric(breedhw$Height.F)
breedhw$Height.F[is.na(breedhw$Height.F)] <- 0
#Create a new feature for large, small or medium
shelter.df$Breed1H <- NA
shelter.df$Breed1W <- NA
for (i in 1:nrow(shelter.df)) {
  x <- trimws(strsplit(shelter.df$Breed1[i],"Mix"))
  for (j in 1:nrow(breedhw)) {
    if(grepl(x[[1]][1],breedhw$Breed[j])) {
      if (grepl("Male",shelter.df$SexuponOutcome[i])){
        shelter.df$Breed1W[i] <- breedhw$Weight.M[j]
        shelter.df$Breed1H[i] <- breedhw$Height.M[j]
      }
      if (grepl("Female",shelter.df$SexuponOutcome[i])){
        shelter.df$Breed1W[i] <- breedhw$Weight.F[j]
        shelter.df$Breed1H[i] <- breedhw$Height.F[j]
      }
      if (grepl("Unknow",shelter.df$SexuponOutcome[i])){
        shelter.df$Breed1W[i] <- breedhw$Weight.M[j]
        shelter.df$Breed1H[i] <- breedhw$Height.M[j]
      }
      break
    }  
  }
}
#Cats are missing some info. We are assuming all cats are average 9inch and 9lbs
shelter.df$Breed1H[which(shelter.df$AnimalType=="Cat")] <- 9 
shelter.df$Breed1W[which(shelter.df$AnimalType=="Cat")] <- 9 
shelter.df$Breed1H[which(shelter.df$Breed1=="Dachshund" & shelter.df$SexuponOutcome=="")] <- 8
shelter.df$Breed1W[which(shelter.df$Breed1=="Dachshund" & shelter.df$SexuponOutcome=="")] <- 16
shelter.df$Breed1H[which(shelter.df$Breed1=="Dachshund Stan Mix")] <- 8
shelter.df$Breed1W[which(shelter.df$Breed1=="Dachshund Stan Mix")] <- 16
#Repeat for Breed2
shelter.df$Breed2H <- NA
shelter.df$Breed2W <- NA
for (i in 1:nrow(shelter.df)) {
  x <- trimws(strsplit(shelter.df$Breed2[i],"Mix"))
  for (j in 1:nrow(breedhw)) {
    if(grepl(x[[1]][1],breedhw$Breed[j])) {
      if (grepl("Male",shelter.df$SexuponOutcome[i])){
        shelter.df$Breed2W[i] <- breedhw$Weight.M[j]
        shelter.df$Breed2H[i] <- breedhw$Height.M[j]
      }
      if (grepl("Female",shelter.df$SexuponOutcome[i])){
        shelter.df$Breed2W[i] <- breedhw$Weight.F[j]
        shelter.df$Breed2H[i] <- breedhw$Height.F[j]
      }
      if (grepl("Unknow",shelter.df$SexuponOutcome[i])){
        shelter.df$Breed2W[i] <- breedhw$Weight.M[j]
        shelter.df$Breed2H[i] <- breedhw$Height.M[j]
      }
      break
    }
    else if(shelter.df$Breed2[i]=="Not Applicable"){
      shelter.df$Breed2W[i] <- 0
      shelter.df$Breed2H[i] <- 0
      break
    }
  }
}
shelter.df$Breed2H[which(shelter.df$Breed2=="Cirneco")] <- 18
shelter.df$Breed2W[which(shelter.df$Breed2=="Cirneco")] <- 22
#Got two choice - create a single height and weight feature combining the two or leave them separate
shelter.df$BreedHeight <- (shelter.df$Breed1H + shelter.df$Breed2H)/2
shelter.df$BreedWeight <- (shelter.df$Breed1W + shelter.df$Breed2W)/2
#Also create a purebreed feature
shelter.df$Purebreed <- ifelse(grepl("Mix",shelter.df$Breed1) | shelter.df$Breed2=="Not Applicable", "No", "Yes")
#Clean age
shelter.df$AgeuponOutcome <- as.character(shelter.df$AgeuponOutcome)
convertAge <- function(x){
  x <- ifelse(x!="",strsplit(x,"\\s")[[1]][1],x)
}
shelter.df$Age <- sapply(shelter.df$AgeuponOutcome,convertAge)
shelter.df$Age <- as.numeric(shelter.df$Age)
#convert age to weeks
for (i in 1:nrow(shelter.df)) {
  if(!is.na(shelter.df$Age[i])){
    if(grepl("year",shelter.df$AgeuponOutcome[i])){
      shelter.df$Age[i] <- floor(shelter.df$Age[i]*52)
    }
    else if(grepl("month",shelter.df$AgeuponOutcome[i])){
      shelter.df$Age[i] <- floor(shelter.df$Age[i]*4)
    }
    else if(grepl("week",shelter.df$AgeuponOutcome[i])){
      shelter.df$Age[i] <- floor(shelter.df$Age[i])
    }
  }
}
#Replace NA ages with mean
shelter.df$Age[is.na(shelter.df$Age)] <- mean(na.omit(shelter.df$Age))
#Date time: Earlier iterations have issues with 'Died' factor - it had the worst accuracy. Maybe date time will improve it
#Will extract only the time part
datetimesplt <- function(x){x <- strsplit(as.character(x),"\\s")[[1]][2]}
shelter.df$Time <- sapply(shelter.df$DateTime, datetimesplt)
#Convert Time to the closest hour
for (i in 1:nrow(shelter.df)) {
  t1 <- as.numeric(strsplit(shelter.df$Time[i],":")[[1]][1])
  t2 <- as.numeric(strsplit(shelter.df$Time[i],":")[[1]][2])
  shelter.df$TimeHour[i] <- ifelse(t2 > 30, t1+1, t1)
}
#Lets clean up name. Name may have a play in returning to owner. Lots of name missing - we will just say has a name
shelter.df$HasName <- ifelse(shelter.df$Name=="", "No", "Yes")
#One missing sex
shelter.df$SexuponOutcome[which(shelter.df$SexuponOutcome=="")] <- "Unknown"
shelter.df$SexuponOutcome <- as.factor(as.character(shelter.df$SexuponOutcome))
#Lets extract a few features to work with for first model
shelter.XB <- shelter.df[,which(colnames(shelter.df) %in% c("OutcomeType","HasName","TimeHour", "AnimalType", "SexuponOutcome", 
                                                            "Color1", "Color2","Age","BreedHeight","BreedWeight", "Purebreed"))]
shelter.XB$Color1 <- as.factor(shelter.XB$Color1)
shelter.XB$Color2 <- as.factor(shelter.XB$Color2)
shelter.XB$HasName <- as.factor(shelter.XB$HasName)
shelter.XB$Purebreed <- as.factor(shelter.XB$Purebreed)
#Split original test and train
shelter.XB.Train <- shelter.XB[!is.na(shelter.XB$OutcomeType),]
shelter.XB.Test <- shelter.XB[is.na(shelter.XB$OutcomeType),]
#Visualize data
#Try xboost model
library(caret)
library(xgboost)
dummies <- dummyVars(~.,data = shelter.XB.Train[,-c(1,6,7,8,9)])
tmp.train <- data.frame(predict(dummies, newdata = shelter.XB.Train[,-c(1,6,7,8,9)]))
tmp.train <- cbind(tmp.train,shelter.XB.Train[,c(6,7,8,9)])
#convert output label to numeric Adoption=0, Died=1, Euthanasia=2, Return_to_owner=3, Transfer=4
train.label <- rep(NA,nrow(tmp.train))
for(i in 1:nrow(tmp.train)) {
  switch (as.character(shelter.XB.Train$OutcomeType[i]),
    Adoption = {train.label[i] <- 0},
    Died = {train.label[i] <- 1},
    Euthanasia = {train.label[i] <- 2},
    Return_to_owner = {train.label[i] <- 3},
    Transfer = {train.label[i] <- 4},
    stop("Invalid case")
  )  
}
tmp.train.tr <- tmp.train[1:16729,]
tmp.train.val <- tmp.train[16730:26729,]
train.label.tr <- train.label[1:16729]
train.label.val <- train.label[16730:26729]
xb_model <- xgboost(data = data.matrix(tmp.train.tr), label = train.label.tr, max_depth = 6, nround = 100, eta = 0.1, 
                    subsample = 1.0, eval_metric = "mlogloss",objective = "multi:softprob", num_class = 5 )

pred <- predict(xb_model,data.matrix(tmp.train.val))
pred_df <-data.frame(matrix(pred, ncol=5, byrow = TRUE))
colnames(pred_df) <- c("Adoption","Died","Euthanasia","Return_to_owner","Transfer")
library(MLmetrics)
Adoption <- rep(0,10000)
Died <- rep(0,10000)
Euthanasia <- rep(0,10000)
R2O <- rep(0,10000)
Transfer <- rep(0,10000)
for (i in 1:nrow(tmp.train.val)) {
  if(train.label.val[i]==0){
     Adoption[i] <- 1
  }
  else if(train.label.val[i]==1){
    Died[i] <- 1
  }
  else if(train.label.val[i]==2){
    Euthanasia[i] <- 1
  }
  else if(train.label.val[i]==3){
    R2O[i] <- 1
  }
  else if(train.label.val[i]==4){
    Transfer[i] <- 1
  }
}
actual <- data.frame(Adoption,Died,Euthanasia,R2O,Transfer)
colnames(actual) <- c("Adoption","Died","Euthanasia","Return_to_owner","Transfer")
#rownames(actual) <- rownames(traindf.val)
#Find log loss
m_actual <- as.matrix(actual)
m_results <- as.matrix(pred_df)
MultiLogLoss(m_results,m_actual)
#Predict test set
dummies2 <- dummyVars(~.,data = shelter.XB.Test[,-c(1,6,7,8,9)])
tmp.test <- data.frame(predict(dummies, newdata = shelter.XB.Test[,-c(1,6,7,8,9)]))
tmp.test <- cbind(tmp.test,shelter.XB.Test[,c(6,7,8,9)])
final.pred <- predict(xb_model,data.matrix(tmp.test))
final.pred.df <-data.frame(matrix(final.pred, ncol=5, byrow = TRUE))
colnames(final.pred.df) <- c("Adoption","Died","Euthanasia","Return_to_owner","Transfer")
submit.data <- cbind(data.frame(shelter.tst$ID),final.pred.df)
colnames(submit.data)[1] <- "ID"
write.csv(submit.data,file = "/home/awaz/Documents/UISCourses/DataScience/AnimalShelter/submit3.csv",row.names = FALSE)
