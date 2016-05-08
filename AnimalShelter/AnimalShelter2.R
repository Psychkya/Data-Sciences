shelter.train <- read.csv('/home/awaz/Documents/UISCourses/DataScience/AnimalShelter/train.csv')
str(shelter.train)
#There is too many breeds. Attempting to pair them down.
shelter.train$uniqbreed <- as.character(shelter.train$Breed)
transbreed <- function(x){
    x <- ifelse(grepl("/",x),paste(strsplit(x,"/")[[1]][1], "Mix"),x)
}
shelter.train$uniqbreed <- sapply(shelter.train$uniqbreed,transbreed)
shelter.train$uniqbreed[grep("Pit Bull Terrier Mix", shelter.train$uniqbreed)] <- "Pit Bull Mix"
shelter.train$uniqbreed[grep("American Pit Bull", shelter.train$uniqbreed)] <- "Pit Bull"
shelter.train$uniqbreed[grep("Bulldog Mix", shelter.train$uniqbreed)] <- "Bulldog Mix"
shelter.train$uniqbreed[grep(" Bulldog",shelter.train$uniqbreed)] <- "Bulldog"
shelter.train$uniqbreed[grep("Terrier Mix", shelter.train$uniqbreed)] <- "Terrier Mix"
shelter.train$uniqbreed[grep("Collie.*Mix", shelter.train$uniqbreed)] <- "Collie Mix"
shelter.train$uniqbreed[grep("Bull Terrier.*Mix",shelter.train$uniqbreed)] <- "Terrier Mix"
shelter.train$uniqbreed[grep(" Terrier",shelter.train$uniqbreed)] <- "Terrier"
shelter.train$uniqbreed[grep("Chihuahua.*Mix", shelter.train$uniqbreed)] <- "Chihuahua Mix"
shelter.train$uniqbreed[grep("Chihuahua.*hair", shelter.train$uniqbreed)] <- "Chihuahua"
shelter.train$uniqbreed[grep("Dachshund.*Mix", shelter.train$uniqbreed)] <- "Dachshund Mix"
shelter.train$uniqbreed[grep("Dachshund.*hair", shelter.train$uniqbreed)] <- "Dachshund"
shelter.train$uniqbreed[grep("St. Bernard", shelter.train$uniqbreed)] <- "St. Bernard"
shelter.train$uniqbreed[grep("Poodle", shelter.train$uniqbreed)] <- "Poodle Mix"
shelter.train$uniqbreed[shelter.train$AnimalType=="Cat" & grepl("Domestic|American",shelter.train$uniqbreed)] <- "Domestic"
Breedtable <- table(shelter.train$uniqbreed)
Breedtable <- data.frame(Breedtable)
for (i in 1:nrow(Breedtable)) {
  
  for (j in 1:nrow(shelter.train)) {
    if(shelter.train$uniqbreed[j]==Breedtable$Var1[i]){
      Breedtable$Type[i] <- as.character(shelter.train$AnimalType[j])
      break
    }
  }
}
#Define a pure breed column. People may tend to like pure breed
purebreed <- function(x){
  x <- ifelse(grepl("Mix",x),"No","Yes")
}
shelter.train$purebreed <- sapply(shelter.train$uniqbreed,purebreed)
shelter.train$purebreed[grep("Domestic",shelter.train$uniqbreed)] <- "No" 
shelter.train$purebreed <- as.factor(shelter.train$purebreed)
#Still too many categories - pair down even more. Saving the original list
shelter.train$uniqbreed2 <- shelter.train$uniqbreed
removemix <- function(x){
  x <- trimws(strsplit(x,"Mix")[[1]][1])
}
shelter.train$uniqbreed <- sapply(shelter.train$uniqbreed, removemix)
Breedtable2 <- data.frame(table(as.factor(shelter.train$uniqbreed)))
for (i in 1:nrow(Breedtable2)) {
  
  for (j in 1:nrow(shelter.train)) {
    if(shelter.train$uniqbreed[j]==Breedtable2$Var1[i]){
      Breedtable2$Type[i] <- as.character(shelter.train$AnimalType[j])
      break
    }
  }
}
for(i in 1:nrow(Breedtable2)){
  Breedtable2$var2[i] <- ifelse(Breedtable2$Freq[i] < 10 & Breedtable2$Type[i]=="Dog", "Other", as.character(Breedtable2$Var1[i]))
}
#There is just too many breeds. Trying to see if I can define large, small, medium instead.
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
for (i in 1:nrow(shelter.train)) {
  if(grepl("Mix",as.character(shelter.train$Breed[i]))){
    x <- trimws(strsplit(as.character(shelter.train$Breed[i]),"Mix"))
    if(grepl(x[[1]][1],largebreed)){
      shelter.train$BreedSize[i] <- "Large"
    }
    else if(grepl(x[[1]][1],mediumbreed)){
      shelter.train$BreedSize[i] <- "Medium"
    }
    else if(grepl(x[[1]][1],smallbreed)){
      shelter.train$BreedSize[i] <- "Small"
    }
    else{
      shelter.train$BreedSize[i] <- "Unknown"
    }
  }
  else if(grepl("/",as.character(shelter.train$Breed[i]))){
      x <- strsplit(as.character(shelter.train$Breed[i]),"/")
      if(grepl(x[[1]][1],largebreed) & grepl(x[[1]][2],largebreed)){
        shelter.train$BreedSize[i] <- "Large"
      }
      else if(grepl(x[[1]][1],largebreed) & grepl(x[[1]][2],mediumbreed)){
        shelter.train$BreedSize[i] <- "Medium"
      }
      else if(grepl(x[[1]][1],largebreed) & grepl(x[[1]][2],smallbreed)){
        shelter.train$BreedSize[i] <- "Medium"
      }
      else if(grepl(x[[1]][1],mediumbreed) & grepl(x[[1]][2],mediumbreed)){
        shelter.train$BreedSize[i] <- "Medium"
      }
      else if(grepl(x[[1]][1],mediumbreed) & grepl(x[[1]][2],largebreed)){
        shelter.train$BreedSize[i] <- "Medium"
      }
      else if(grepl(x[[1]][1],mediumbreed) & grepl(x[[1]][2],smallbreed)){
        shelter.train$BreedSize[i] <- "Small"
      }
      else if(grepl(x[[1]][1],smallbreed) & grepl(x[[1]][2],smallbreed)){
        shelter.train$BreedSize[i] <- "Small"
      }
      else if(grepl(x[[1]][1],smallbreed) & grepl(x[[1]][2],mediumbreed)){
        shelter.train$BreedSize[i] <- "Small"
      }
      else if(grepl(x[[1]][1],smallbreed) & grepl(x[[1]][2],largebreed)){
        shelter.train$BreedSize[i] <- "Medium"
      }
      else{
        shelter.train$BreedSize[i] <- "Unknown"
      }
    
  }
  else{
    if(grepl(as.character(shelter.train$Breed[i]),largebreed)){
      shelter.train$BreedSize[i] <- "Large"
    }
    else if(grepl(as.character(shelter.train$Breed[i]),mediumbreed)){
      shelter.train$BreedSize[i] <- "Medium"
    }
    else if(grepl(as.character(shelter.train$Breed[i]),smallbreed)){
      shelter.train$BreedSize[i] <- "Small"
    }
    else{
      shelter.train$BreedSize <- "Unknown"
    }
  }
}
#After ovservation, convert cat with unknown size to small. convert dogs to medium
shelter.train$BreedSize[grep("Munchkin",shelter.train$Breed)] <- "Small"
shelter.train$BreedSize[which(shelter.train$BreedSize=="Unknown")] <- "Large"
shelter.train$BreedSize <- as.factor(shelter.train$BreedSize)

#Clean age
shelter.train$AgeuponOutcome <- as.character(shelter.train$AgeuponOutcome)
convertAge <- function(x){
  x <- ifelse(x!="",strsplit(x,"\\s")[[1]][1],x)
}
shelter.train$Age <- sapply(shelter.train$AgeuponOutcome,convertAge)
shelter.train$Age <- as.numeric(shelter.train$Age)
#convert age to weeks
for (i in 1:nrow(shelter.train)) {
  if(!is.na(shelter.train$Age[i])){
    if(grepl("year",shelter.train$AgeuponOutcome[i])){
      shelter.train$Age[i] <- shelter.train$Age[i]*52
    }
    else if(grepl("month",shelter.train$AgeuponOutcome[i])){
      shelter.train$Age[i] <- shelter.train$Age[i]*4
    }
    else if(grepl("week",shelter.train$AgeuponOutcome[i])){
      shelter.train$Age[i] <- shelter.train$Age[i]
    }
  }
}
#Replace NA ages with median
shelter.train$Age[is.na(shelter.train$Age)] <- mean(na.omit(shelter.train$Age))
#Clean color. can use transbreed
shelter.train$uniqcolor <- as.character(shelter.train$Color)
shelter.train$uniqcolor <- sapply(shelter.train$uniqcolor,transbreed)
#Find least occuring colors
colortable <- data.frame(table(as.factor(shelter.train$uniqcolor)))
for (i in 1:nrow(colortable)) {
  
  for (j in 1:nrow(shelter.train)) {
    if(shelter.train$uniqcolor[j]==colortable$Var1[i]){
      colortable$Type[i] <- as.character(shelter.train$AnimalType[j])
      break
    }
  }
}
shelter.train$uniqcolor[grep("Agouti",shelter.train$uniqcolor)] <- "Other"
shelter.train$uniqcolor[grep("Apricot",shelter.train$Color)] <- "Other"
shelter.train$uniqcolor[grep("Black Brindle",shelter.train$Color)] <- "Black Brindle"
shelter.train$uniqcolor[grep("Black Smoke",shelter.train$Color)] <- "Black Mix"
shelter.train$uniqcolor[grep("Black Tiger",shelter.train$Color)] <- "Black Mix"
shelter.train$uniqcolor[grep("Black Tabby",shelter.train$Color)] <- "Black Tabby"
shelter.train$uniqcolor[grep("Buff",shelter.train$Color)] <- "Buff"

shelter.train$uniqcolor[grep("Blue Cream",shelter.train$Color)] <- "Blue Mix"
shelter.train$uniqcolor[grep("Blue Point",shelter.train$Color)] <- "Blue Mix"
shelter.train$uniqcolor[grep("Blue Smoke",shelter.train$Color)] <- "Blue Mix"
shelter.train$uniqcolor[grep("Blue Tick",shelter.train$Color)] <- "Blue Mix"
shelter.train$uniqcolor[grep("Blue Tiger",shelter.train$Color)] <- "Blue Mix"
shelter.train$uniqcolor[grep("Blue Merle",shelter.train$Color)] <- "Blue Mix"

shelter.train$uniqcolor[grep("Brown Merle",shelter.train$Color)] <- "Brown Mix"
shelter.train$uniqcolor[grep("Brown Tiger",shelter.train$Color)] <- "Brown Mix"
shelter.train$uniqcolor[grep("Calico",shelter.train$Color)] <- "Calico"
shelter.train$uniqcolor[grep("Chocolate",shelter.train$Color)] <- "Chocolate"

shelter.train$uniqcolor[grep("Lilac",shelter.train$Color)] <- "Other"
shelter.train$uniqcolor[grep("Liver",shelter.train$Color)] <- "Other"
shelter.train$uniqcolor[grep("Lynx",shelter.train$Color)] <- "Lynx Point"

shelter.train$uniqcolor[grep("Orange Mix",shelter.train$Color)] <- "Orange"
shelter.train$uniqcolor[grep("Orange Tiger",shelter.train$Color)] <- "Orange"
shelter.train$uniqcolor[shelter.train$Color=="Orange Mix"] <- "Orange"

shelter.train$uniqcolor[grep("Pink",shelter.train$Color)] <- "Other"

shelter.train$uniqcolor[grep("Red Merle",shelter.train$Color)] <- "Red Mix"
shelter.train$uniqcolor[grep("Red Tick",shelter.train$Color)] <- "Red Mix"
shelter.train$uniqcolor[grep("Ruddy",shelter.train$Color)] <- "Other"

shelter.train$uniqcolor[grep("Seal",shelter.train$Color)] <- "Seal Point"
shelter.train$uniqcolor[grep("Silver",shelter.train$Color)] <- "Other"

shelter.train$uniqcolor[grep("Tortie",shelter.train$Color)] <- "Tortie"
shelter.train$uniqcolor[grep("Tricolor",shelter.train$Color)] <- "Tricolor"
shelter.train$uniqcolor[grep("Yellow Brindle",shelter.train$Color)] <- "Yellow Mix"
#pair down even more
shelter.train$uniqcolor[grep("Cream Mix",shelter.train$uniqcolor)] <- "Cream"
shelter.train$uniqcolor[grep("Cream Tabby",shelter.train$uniqcolor)] <- "Cream Tabby"
shelter.train$uniqcolor[grep("Fawn",shelter.train$uniqcolor)] <- "Fawn"
shelter.train$uniqcolor[grep("Gold",shelter.train$uniqcolor)] <- "Gold"
shelter.train$uniqcolor[grep("Grey",shelter.train$uniqcolor)] <- "Grey"
shelter.train$uniqcolor[grep("Gray Tabby",shelter.train$uniqcolor)] <- "Gray"
shelter.train$uniqcolor[grep("Torbie",shelter.train$uniqcolor)] <- "Torbie"
shelter.train$uniqcolor[grep("Flame",shelter.train$uniqcolor)] <- "Other"
shelter.train$uniqcolor[grep("Gold",shelter.train$uniqcolor)] <- "Other"
shelter.train$uniqcolor[grep("Yellow",shelter.train$uniqcolor)] <- "Yellow"

colortable <- data.frame(table(as.factor(shelter.train$uniqcolor)))
for (i in 1:nrow(colortable)) {
  
  for (j in 1:nrow(shelter.train)) {
    if(shelter.train$uniqcolor[j]==colortable$Var1[i]){
      colortable$Type[i] <- as.character(shelter.train$AnimalType[j])
      break
    }
  }
}
shelter.train$uniqcolor <- as.factor(shelter.train$uniqcolor)
#One sex has spaces, clean it up
shelter.train$SexuponOutcome[shelter.train$SexuponOutcome==""] <- "Unknown"
shelter.train$SexuponOutcome <- as.factor(as.character(shelter.train$SexuponOutcome))
#Cleanup outcomesubtype
#There is lots of blanks for seach outcometype
#First save original outcomesubtype
shelter.train$OutcomeSubtypeBKP <- shelter.train$OutcomeSubtype
#In case of Adoption, create new outcomesubtype as 'Newhome'
shelter.train$OutcomeSubtype <- as.character(shelter.train$OutcomeSubtype)
shelter.train$OutcomeSubtype[which(grepl("Adoption",shelter.train$OutcomeType) & shelter.train$OutcomeSubtype=="")] <- "Newhome"
#In case of return_to_owner, all outcomesubtype is blank. So we will call it 'Backhome'
shelter.train$OutcomeSubtype[which(grepl("Return_to_owner",shelter.train$OutcomeType) & shelter.train$OutcomeSubtype=="")] <- "Backhome"
#There is about 16 died and 1 Euthanasia that we do not know outcomesubtype. So we will call them unknown
shelter.train$OutcomeSubtype[which(shelter.train$OutcomeSubtype=="")] <- "Unknown"
shelter.train$OutcomeSubtype <- as.factor(shelter.train$OutcomeSubtype)
#############
#First Model
traindf <- shelter.train[,which(names(shelter.train) %in% c("OutcomeType","OutcomeSubtype","AnimalType","SexuponOutcome",
                                                            "purebreed","Age","uniqcolor","BreedSize"))]
index <- sample(nrow(traindf),nrow(traindf)/3)
traindf.val <- traindf[index,]
traindf.trn <- traindf[-index,]
library(caret)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
grid_rf <- expand.grid(.mtry = c(2, 4, 6, 8))
set.seed(1)
rfmodel1 <- train(traindf.trn$OutcomeType ~ ., data = traindf.trn[,-1], method = "rf", metric = "Kappa", 
              trControl = ctrl, tuneGrid = grid_rf)
rfmodel1
results <- predict(rfmodel1,traindf.val[,-1],type='prob')
#To calculate log loss, convert actual results to a matrix. Since the predicted results is a data frame, lets try that first
Adoption <- rep(0,8900)
Died <- rep(0,8900)
Euthanasia <- rep(0,8900)
R2O <- rep(0,8900)
Transfer <- rep(0,8900)
#Remove the NA's
traindf.val <- traindf.val[!is.na(traindf.val$OutcomeSubtype),]
for (i in 1:nrow(traindf.val)) {
  if(grepl("Adoption",traindf.val$OutcomeType[i])){
    Adoption[i] <- 1
  }
  else if(grepl("Died",traindf.val$OutcomeType[i])){
    Died[i] <- 1
  }
  else if(grepl("Euthanasia",traindf.val$OutcomeType[i])){
    Euthanasia[i] <- 1
  }
  else if(grepl("Return_to_owner",traindf.val$OutcomeType[i])){
    R2O[i] <- 1
  }
  else if(grepl("Transfer",traindf.val$OutcomeType[i])){
    Transfer[i] <- 1
  }
}
actual <- data.frame(Adoption,Died,Euthanasia,R2O,Transfer)
colnames(actual) <- c("Adoption","Died","Euthanasia","Return_to_owner","Transfer")
rownames(actual) <- rownames(traindf.val)
#Got an issue, number of rows for actual and predicted does not match
c <- 1
for(i in 1:nrow(results)){
  if(rownames(actual[i,])!=rownames(results[i,])){
    cat(c, " : ", rownames(actual[i,])," : ", rownames(results[i,]), "\n")
    break
  }
  c <- c + 1
}
#Welp! Seems I left a few NA in Outcomesubtype. Lets get rid of those and re-run the code to get data frame of actuals
#Find log loss
library(MLmetrics)
m_actual <- as.matrix(actual)
m_results <- as.matrix(results)
MultiLogLoss(m_results,m_actual)
#Try again using a log loss function
LogLoss <- function(actual, predicted, eps=1e-15) {
  predicted[predicted < eps] <- eps;
  predicted[predicted > 1 - eps] <- 1 - eps;
  -1/nrow(actual)*(sum(actual*log(predicted)))
}
LogLoss(m_actual,m_results)
