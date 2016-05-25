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
#Change any colors in color1 less than 5 to 'other'
#colortbl <- data.frame(table(as.factor(shelter.df$Color1)))
#for (i in 1:nrow(shelter.df)) {
#  for(j in 1:nrow(colortbl)){
#    if(shelter.df$Color1[i]==colortbl$Var1[j]) {
#      if(colortbl$Freq[j] < 5){
#        shelter.df$Color1[i] <- "Other"
#      }
#      break
#    }
#  }
#}
#We are good with color - now deal with breed
for (i in 1:nrow(shelter.df)) {
  shelter.df$Breed1[i] <- strsplit(as.character(shelter.df$Breed[i]),"/")[[1]][1]
  shelter.df$Breed2[i] <- strsplit(as.character(shelter.df$Breed[i]),"/")[[1]][2]
}
#Convert NA in Breed2 to 'Not Applicable'
shelter.df$Breed2[is.na(shelter.df$Breed2)] <- "Not Applicable"
#Remove mix
#removemix <- function(x){
#  x <- trimws(strsplit(x,"Mix")[[1]][1])
#}
#shelter.df$Breed1 <- sapply(shelter.df$Breed1,removemix)
#Combine like breeds
#shelter.df$Breed1[grep("Husky", shelter.df$Breed1)] <- "Husky"
#shelter.df$Breed1[grep("Bulldog", shelter.df$Breed1)] <- "Bulldog"
shelter.df$Breed1[grep("Pit Bull", shelter.df$Breed1)] <- "Pit Bull"
shelter.df$Breed1[grep("Staffordshire", shelter.df$Breed1)] <- "Pit Bull"
#shelter.df$Breed1[grep("Collie", shelter.df$Breed1)] <- "Collie"
shelter.df$Breed1[grep("Black", shelter.df$Breed1)] <- "Black Mouth Cur"
#shelter.df$Breed1[grep("Miniature", shelter.df$Breed1)] <- "Miniature"
shelter.df$Breed1[grep("Chihuahua", shelter.df$Breed1)] <- "Chihuahua"
#shelter.df$Breed1[grep("Dachshund", shelter.df$Breed1)] <- "Dachshund"
#shelter.df$Breed1[grep("Domestic", shelter.df$Breed1)] <- "Domestic"
#shelter.df$Breed1[grep("Cocker Spaniel", shelter.df$Breed1)] <- "Cocker Spaniel"
#shelter.df$Breed1[grep("Pointer", shelter.df$Breed1)] <- "Pointer"
#shelter.df$Breed1[grep("Cocker Spaniel", shelter.df$Breed1)] <- "Cocker Spaniel"
#shelter.df$Breed1[grep("St. Bernard", shelter.df$Breed1)] <- "St. Bernard"
#Create breed size
breedsize <- read.csv('/home/awaz/Documents/UISCourses/DataScience/AnimalShelter/BreedsizeFinal.csv',stringsAsFactors = FALSE)
breedsize <- breedsize[-which(breedsize$Size==""),]
#combine large, medium and small dog breeds into a single variable respectively. This will help with grep
smallbreed <- ""
largebreed <- ""
mediumbreed <- ""
for (i in 1:nrow(breedsize)) {
  if(breedsize$Size[i]=="Small"){
    smallbreed <- paste(smallbreed, breedsize$Breed[i], sep = " ")
  }
  if(breedsize$Size[i]=="Large"){
    largebreed <- paste(largebreed,breedsize$Breed[i],sep = " ")
  }
  if(breedsize$Size[i]=="Medium"){
    mediumbreed <- paste(mediumbreed,breedsize$Breed[i],sep = " ")
  }
}
largebreed <- trimws(largebreed)
mediumbreed <- trimws(mediumbreed)
smallbreed <- trimws(smallbreed)
#Create a new feature for large, small or medium
shelter.df$LargeBreed1 <- NA
shelter.df$MediumBreed1 <- NA
shelter.df$SmallBreed1 <- NA
for (i in 1:nrow(shelter.df)) {
  x <- trimws(strsplit(shelter.df$Breed1[i],"Mix"))
  if(grepl(x[[1]][1],largebreed)){
    shelter.df$LargeBreed1[i] <- shelter.df$Breed1[i]
  }
  else if(grepl(x[[1]][1],mediumbreed)){
    shelter.df$MediumBreed1[i] <- shelter.df$Breed1[i]
  }
  else if(grepl(x[[1]][1],smallbreed)){
    shelter.df$SmallBreed1[i] <- shelter.df$Breed1[i]
  }
}

#My input file had some cat breeds set as large, we will change then first
shelter.df$SmallBreed1[which(shelter.df$AnimalType=="Cat" & !is.na(shelter.df$LargeBreed1))] <- 
                    shelter.df$LargeBreed1[which(shelter.df$AnimalType=="Cat" & !is.na(shelter.df$LargeBreed1))]
shelter.df$LargeBreed1[which(shelter.df$AnimalType=="Cat" & !is.na(shelter.df$LargeBreed1))] <- NA
shelter.df$SmallBreed1[which(shelter.df$AnimalType=="Cat" & 
                    is.na(shelter.df$SmallBreed1) & is.na(shelter.df$MediumBreed1) & is.na(shelter.df$LargeBreed1))] <- 
                    shelter.df$Breed1[which(shelter.df$AnimalType=="Cat" & 
                                              is.na(shelter.df$SmallBreed1) & is.na(shelter.df$MediumBreed1) &
                                              is.na(shelter.df$LargeBreed1))]
shelter.df$SmallBreed1[grep("Coton De Tulear",shelter.df$Breed1)] <- shelter.df$Breed1[grep("Coton De Tulear",shelter.df$Breed1)]
shelter.df$SmallBreed1[grep("Dandie Dinmont",shelter.df$Breed1)] <- shelter.df$Breed1[grep("Dandie Dinmont",shelter.df$Breed1)]
shelter.df$SmallBreed1[grep("Eng Toy Spaniel",shelter.df$Breed1)] <- shelter.df$Breed1[grep("Eng Toy Spaniel",shelter.df$Breed1)]
shelter.df$LargeBreed1[grep("Unknown",shelter.df$Breed1)] <- shelter.df$Breed1[grep("Unknown",shelter.df$Breed1)]
shelter.df$LargeBreed1[grep("Gordon Setter",shelter.df$Breed1)] <- shelter.df$Breed1[grep("Gordon Setter",shelter.df$Breed1)]
shelter.df$SmallBreed1[grep("Dachshund Stan Mix",shelter.df$Breed1)] <- shelter.df$Breed1[grep("Dachshund Stan Mix",shelter.df$Breed1)]
#Convert all NA into some sort of level
shelter.df$SmallBreed1[is.na(shelter.df$SmallBreed1)] <- "Not Applicable"
shelter.df$LargeBreed1[is.na(shelter.df$LargeBreed1)] <- "Not Applicable"
shelter.df$MediumBreed1[is.na(shelter.df$MediumBreed1)] <- "Not Applicable"
#Repeat the steps for Breed2
shelter.df$LargeBreed2 <- NA
shelter.df$MediumBreed2 <- NA
shelter.df$SmallBreed2 <- NA
for (i in 1:nrow(shelter.df)) {
  x <- trimws(strsplit(shelter.df$Breed2[i],"Mix"))
  if(grepl(x[[1]][1],largebreed)){
    shelter.df$LargeBreed2[i] <- shelter.df$Breed2[i]
  }
  else if(grepl(x[[1]][1],mediumbreed)){
    shelter.df$MediumBreed2[i] <- shelter.df$Breed2[i]
  }
  else if(grepl(x[[1]][1],smallbreed)){
    shelter.df$SmallBreed2[i] <- shelter.df$Breed2[i]
  }
}
shelter.df$SmallBreed2[which(shelter.df$AnimalType=="Cat" & !is.na(shelter.df$LargeBreed2))] <- 
  shelter.df$LargeBreed2[which(shelter.df$AnimalType=="Cat" & !is.na(shelter.df$LargeBreed2))]
shelter.df$LargeBreed2[which(shelter.df$AnimalType=="Cat" & !is.na(shelter.df$LargeBreed2))] <- NA
shelter.df$LargeBreed2[which(shelter.df$Breed2=="Not Applicable")] <- "Not Applicable"
shelter.df$MediumBreed2[which(shelter.df$Breed2=="Not Applicable")] <- "Not Applicable"
shelter.df$SmallBreed2[which(shelter.df$Breed2=="Not Applicable")] <- "Not Applicable"
shelter.df$SmallBreed2[which(shelter.df$Breed2=="Unknown")] <- "Not Applicable"
shelter.df$MediumBreed2[which(shelter.df$Breed2=="Unknown")] <- "Not Applicable"
shelter.df$LargeBreed2[which(shelter.df$Breed2=="Unknown")] <- "Not Applicable"
shelter.df$SmallBreed2[which(shelter.df$Breed2=="Cirneco")] <- "Cirneco"
#Convert all NA into some sort of level
shelter.df$SmallBreed2[is.na(shelter.df$SmallBreed2)] <- "Not Applicable"
shelter.df$LargeBreed2[is.na(shelter.df$LargeBreed2)] <- "Not Applicable"
shelter.df$MediumBreed2[is.na(shelter.df$MediumBreed2)] <- "Not Applicable"
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
shelter.df$Age[is.na(shelter.df$Age)] <- floor(mean(na.omit(shelter.df$Age)))
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
#Sex upon outcome
shelter.df$SexuponOutcome[shelter.df$SexuponOutcome==""] <- "Unknown"
shelter.df$SexuponOutcome <- as.factor(as.character(shelter.df$SexuponOutcome))
#Lets extract a few features to work with for first model
shelter.XB <- shelter.df[,which(colnames(shelter.df) %in% c("OutcomeType","HasName","TimeHour", "AnimalType", "SexuponOutcome", "Color1", "Color2",
                                                            "Age","LargeBreed1","MediumBreed1","SmallBreed1","LargeBreed2",
                                                            "MediumBreed2","SmallBreed2"))]
shelter.XB$Color1 <- as.factor(shelter.XB$Color1)
shelter.XB$Color2 <- as.factor(shelter.XB$Color2)
shelter.XB$LargeBreed1 <- as.factor(shelter.XB$LargeBreed1)
shelter.XB$MediumBreed1 <- as.factor(shelter.XB$MediumBreed1)
shelter.XB$SmallBreed1 <- as.factor(shelter.XB$SmallBreed1)
shelter.XB$LargeBreed2 <- as.factor(shelter.XB$LargeBreed2)
shelter.XB$MediumBreed2 <- as.factor(shelter.XB$MediumBreed2)
shelter.XB$SmallBreed2 <- as.factor(shelter.XB$SmallBreed2)
shelter.XB$HasName <- as.factor(shelter.XB$HasName)
#Split original test and train
shelter.XB.Train <- shelter.XB[!is.na(shelter.XB$OutcomeType),]
shelter.XB.Test <- shelter.XB[is.na(shelter.XB$OutcomeType),]
#Visualize data
#Try xboost model
library(caret)
library(xgboost)
tmp.train.tr <- shelter.XB.Train[1:14000,]
tmp.train.val <- shelter.XB.Train[14001:26729,]
train.label.tr <- shelter.XB.Train[1:14000]
train.label.val <- shelter.XB.Train[14001:26729]
xb_model_tr <- xgboost(data = data.matrix(tmp.train.tr), label = train.label.tr, max_depth = 6, nround = 200, eta = 0.07, 
                    colsample_bytree = 0.7,subsample = 1.0, eval_metric = "mlogloss",objective = "multi:softprob", num_class = 5 )

pred <- predict(xb_model_tr,data.matrix(tmp.train.val))
pred_df <-data.frame(matrix(pred, ncol=5, byrow = TRUE))
colnames(pred_df) <- c("Adoption","Died","Euthanasia","Return_to_owner","Transfer")
library(MLmetrics)
Adoption <- rep(0,nrow(tmp.train.val))
Died <- rep(0,nrow(tmp.train.val))
Euthanasia <- rep(0,nrow(tmp.train.val))
R2O <- rep(0,nrow(tmp.train.val))
Transfer <- rep(0,nrow(tmp.train.val))
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
#STACKING !!!
#run model on val set
xb_model_val <- xgboost(data = data.matrix(tmp.train.val), label = train.label.val, max_depth = 6, nround = 200, eta = 0.07, 
                       colsample_bytree = 0.7,subsample = 1.0, eval_metric = "mlogloss",objective = "multi:softprob", num_class = 5 )

pred_val <- predict(xb_model_val,data.matrix(tmp.train.tr))
pred_df_val <-data.frame(matrix(pred_val, ncol=5, byrow = TRUE))
colnames(pred_df_val) <- c("Adoption","Died","Euthanasia","Return_to_owner","Transfer")
#Test log loss
Adoption <- rep(0,nrow(tmp.train.tr))
Died <- rep(0,nrow(tmp.train.tr))
Euthanasia <- rep(0,nrow(tmp.train.tr))
R2O <- rep(0,nrow(tmp.train.tr))
Transfer <- rep(0,nrow(tmp.train.tr))
for (i in 1:nrow(tmp.train.tr)) {
  if(train.label.tr[i]==0){
    Adoption[i] <- 1
  }
  else if(train.label.tr[i]==1){
    Died[i] <- 1
  }
  else if(train.label.tr[i]==2){
    Euthanasia[i] <- 1
  }
  else if(train.label.tr[i]==3){
    R2O[i] <- 1
  }
  else if(train.label.tr[i]==4){
    Transfer[i] <- 1
  }
}
actual <- data.frame(Adoption,Died,Euthanasia,R2O,Transfer)
colnames(actual) <- c("Adoption","Died","Euthanasia","Return_to_owner","Transfer")
#rownames(actual) <- rownames(traindf.val)
#Find log loss
m_actual <- as.matrix(actual)
m_results <- as.matrix(pred_df_val)
MultiLogLoss(m_results,m_actual)
#Create a combined df of train and val predictions
combine_df <- rbind(pred_df_val,pred_df)
####Stopped here

#Add predictions back to training
shelter.XB.Train.Stack <- cbind(shelter.XB.Train, combine_df)
#Predict test set using xb_model_tr
dummies2 <- dummyVars(~.,data = shelter.XB.Test[,-c(1,12,14)])
tmp.test <- data.frame(predict(dummies, newdata = shelter.XB.Test[,-c(1,12,14)]))
tmp.test <- cbind(tmp.test,shelter.XB.Test[,c(12,14)])
pred.test.set <- predict(xb_model_tr,data.matrix(tmp.test))
pred.test.df <-data.frame(matrix(final.pred, ncol=5, byrow = TRUE))
colnames(pred.test.df) <- c("Adoption","Died","Euthanasia","Return_to_owner","Transfer")
#Add these predictions to test set
shelter.XB.Test.Stack <- cbind(shelter.XB.Test,pred.test.df)
#Now train on full stacked train set
dummies <- dummyVars(~.,data = shelter.XB.Train.Stack[,-c(1,12,14)])
tmp.train <- data.frame(predict(dummies, newdata = shelter.XB.Train[,-c(1,12,14)]))
tmp.train <- cbind(tmp.train,shelter.XB.Train[,c(12,14)])




final.pred <- predict(xb_model_tr,data.matrix(tmp.test))
final.pred.df <-data.frame(matrix(final.pred, ncol=5, byrow = TRUE))
colnames(final.pred.df) <- c("Adoption","Died","Euthanasia","Return_to_owner","Transfer")
submit.data <- cbind(data.frame(shelter.tst$ID),final.pred.df)
colnames(submit.data)[1] <- "ID"
write.csv(submit.data,file = "/home/awaz/Documents/UISCourses/DataScience/AnimalShelter/submit4.csv",row.names = FALSE)
