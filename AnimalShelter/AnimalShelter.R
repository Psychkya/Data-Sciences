shelter.train <- read.csv('/home/awaz/Documents/UISCourses/DataScience/AnimalShelter/train.csv')
str(shelter.train)
shelter.train$uniqbreed <- gsub('Mix','',shelter.train$Breed)
shelter.train$uniqbreed <- as.factor(shelter.train$uniqbreed)
shelter.train <- shelter.train[,-11]
#transbreed <- function(x){
#  x <- gsub("Mix Mix","Mix",paste(strsplit(as.character(x),"/")[[1]][1], "Mix"))
#}
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