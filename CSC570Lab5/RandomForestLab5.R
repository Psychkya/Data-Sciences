reddit <- read.csv('/home/awaz/Documents/UISCourses/DataScience/RedditShortDemoSurvey-1-Cleaned.csv')
str(reddit)
#create a column with continent name
coc <- read.csv('/home/awaz/Documents/UISCourses/DataScience/Countries-Continents-csv.csv',stringsAsFactors = FALSE)
str(coc)
#removed garbage columns. Lots of cleansing
#convert to lower case, remove leading and trailing spaces
#convert country from factor to string
#There are some contry names with accents - convert these to non-accented ones
coc <- coc[which(names(coc) %in% c("Continent", "Country"))]
reddit$Treated.country.data <- as.character(reddit$Treated.country.data)
reddit$Treated.country.data[reddit$Treated.country.data == "Amerikka"] <- "United States"
reddit$Treated.country.data[reddit$Treated.country.data == "Amurika"] <- "United States"
reddit$Treated.country.data[grep("Canada",reddit$Treated.country.data,value = FALSE,fixed = TRUE)] <- "Canada"
reddit$Treated.country.data[grep("Austra",reddit$Treated.country.data,value = FALSE,fixed = TRUE)] <- "Australia"
reddit$Treated.country.data[grep("England",reddit$Treated.country.data,value = FALSE,fixed = TRUE)] <- "England"
reddit$Treated.country.data[reddit$Treated.country.data == "Bosnia and Herzegowina"] <- "Bosnia and Herzegovina"
reddit$Treated.country.data <- iconv(reddit$Treated.country.data,from = "UTF-8",to = "ASCII//TRANSLIT")
reddit.sorted <- reddit[order(reddit$Treated.country.data),]
coc.sorted <- coc[order(coc$Country),]
View(coc.sorted)
j <- 1
for (i in 1:nrow(reddit.sorted)) {
  if(tolower(trimws(reddit.sorted$Treated.country.data[i])) == tolower(trimws(coc.sorted$Country[j])))
  {
    reddit.sorted$continent[i] <- coc.sorted$Continent[j]
  }
  else
  {
    saved.j <- j
    j <- j + 1
    while (j <= nrow(coc.sorted)) {
      if(tolower(trimws(reddit.sorted$Treated.country.data[i])) == tolower(trimws(coc.sorted$Country[j]))){
        reddit.sorted$continent[i] <- coc.sorted$Continent[j]
        break
      }
      j <- j + 1
    }
    if(j > nrow(coc.sorted)){ 
      j <- saved.j
      reddit.sorted$continent[i] <- "unknown"
    }
  }
}
#extract out only "unknown" continents. Lots of unknown, went back and added some
reddit.explore <- reddit.sorted[reddit.sorted$continent == "unknown",]
reddit.explore <- reddit.explore[,which(names(reddit.explore) %in% c("Treated.country.data", "continent"))]
View(reddit.explore)
str(reddit.explore)
#aggregated data by continent
aggdata <- aggregate(reddit.sorted, by=list(group.cont=reddit.sorted$continent),FUN=length)
View(aggdata)                
#countries are cleaned up as best as possible, dropping the unknown rows
reddit.sorted.cln1 <- reddit.sorted[reddit.sorted$continent != "unknown",]
#treated.subreddit.data seems to be garbage - deleting this for now. Also drop the question on US state
#Not sure if the 'X' variables offer much. So will remove these as well
#Dropping entry.id as well
reddit.sorted.cln1 <- reddit.sorted.cln1[-which(names(reddit.sorted.cln1) %in% c("Entry.Id","treated.subreddit.data",  
                "X.For.U..S..redditors..In.which.state.do.you.live.", "X", "X.1", "X.2", "X.3", "X.4", "X.5"))]
str(reddit.sorted.cln1)
#Lets check levels of each variable
levels(reddit.sorted.cln1$What.best.describes.your.employment.status.)
levels(reddit.sorted.cln1$Are.you.a.dog.or.a.cat.person.)
#It looks like 'movies', 'askreddit' and 'Reddit.com' values are removed from the dataframe. So, refactoring
#to drop these variables. Looks like ' reddit.com' and '' still exists. So dropping these
reddit.sorted.cln1 <- reddit.sorted.cln1[reddit.sorted.cln1$Are.you.a.dog.or.a.cat.person.!= " Reddit.com" &
                                         reddit.sorted.cln1$Are.you.a.dog.or.a.cat.person. != "",]
reddit.sorted.cln1$Are.you.a.dog.or.a.cat.person. <- factor(reddit.sorted.cln1$Are.you.a.dog.or.a.cat.person.)
levels(reddit.sorted.cln1$Are.you.a.dog.or.a.cat.person.)
#Lets check all other levels
levels(reddit.sorted.cln1$Which.one.of.the.following.ranges.includes.your.total.yearly.household.income.before.taxes.)
levels(reddit.sorted.cln1$Please.indicate.your.gender.)
levels(reddit.sorted.cln1$Please.select.the.category.that.includes.your.age.)
levels(reddit.sorted.cln1$What.is.your.marital.status.)
levels(reddit.sorted.cln1$Are.you.or.have.you.ever.been.in.military.service.)
#Wonder how many 'none' are in the above variable
table(reddit.sorted.cln1$Are.you.or.have.you.ever.been.in.military.service.=="none")
#There is 5 rows that has 'none' - lets change that to "No"
reddit.sorted.cln1$Are.you.or.have.you.ever.been.in.military.service.[
      reddit.sorted.cln1$Are.you.or.have.you.ever.been.in.military.service.=="none"] <- "No"
#Refactor this variable
reddit.sorted.cln1$Are.you.or.have.you.ever.been.in.military.service. <-
      factor(reddit.sorted.cln1$Are.you.or.have.you.ever.been.in.military.service.)
levels(reddit.sorted.cln1$Are.you.or.have.you.ever.been.in.military.service.)
#continue checking rest
levels(reddit.sorted.cln1$Do.you.currently.have.children.under.the.age.of.18.living.in.your.household.)
#Again, lets chance the 'none' to 'No' and refactor the variable
reddit.sorted.cln1$Do.you.currently.have.children.under.the.age.of.18.living.in.your.household.[
    reddit.sorted.cln1$Do.you.currently.have.children.under.the.age.of.18.living.in.your.household. == "None"] <- "No"
reddit.sorted.cln1$Do.you.currently.have.children.under.the.age.of.18.living.in.your.household. <-
    factor(reddit.sorted.cln1$Do.you.currently.have.children.under.the.age.of.18.living.in.your.household.)
levels(reddit.sorted.cln1$Do.you.currently.have.children.under.the.age.of.18.living.in.your.household.)
#continue checking
levels(reddit.sorted.cln1$What.best.describes.your.level.of.education.)
levels(reddit.sorted.cln1$If.you.were.a.cheese..what.cheese.would.you.be.)
#The question on cheese is iffy. It may have some information on income level, but lets start by dropping this column
#dropping the rows with garbage values may deny us information on other useful columns.
reddit.sorted.cln1 <- reddit.sorted.cln1[-which(names(reddit.sorted.cln1) %in% c("If.you.were.a.cheese..what.cheese.would.you.be."))]
#column names are too long - so need to change them.
str(reddit.sorted.cln1)
colnames(reddit.sorted.cln1) <- c("gender","age","marital","employed","military","household","education","country","income","dogcat","continent")
#Visualize data - lets plot each of the variables against the income variable and observe the distribution
library(ggplot2)
ggplot(reddit.sorted.cln1,aes(reddit.sorted.cln1$gender,..count..
      )) + geom_bar(aes(fill=reddit.sorted.cln1$income
      )) + xlab("Gender") + ylab("count") + ggtitle("Gender vs Income") + theme(axis.text.x=element_text(angle = 90,hjust = 1))
ggplot(reddit.sorted.cln1,aes(reddit.sorted.cln1$age,..count..
      )) + geom_bar(aes(fill=reddit.sorted.cln1$income
      )) + xlab("Age") + ylab("count") + ggtitle("Age vs Income") + theme(axis.text.x=element_text(angle = 90,hjust = 1))
ggplot(reddit.sorted.cln1,aes(reddit.sorted.cln1$marital,..count..
      )) + geom_bar(aes(fill=reddit.sorted.cln1$income
      )) + xlab("Marital Status") + ylab("count") + ggtitle("Marital Status vs Income") + theme(axis.text.x=element_text(angle = 90,hjust = 1))
ggplot(reddit.sorted.cln1,aes(reddit.sorted.cln1$employed,..count..
      )) + geom_bar(aes(fill=reddit.sorted.cln1$income
      )) + xlab("Employment Status") + ylab("count") + ggtitle("Employment Status vs Income") + theme(axis.text.x=element_text(angle = 90,hjust = 1))
ggplot(reddit.sorted.cln1,aes(reddit.sorted.cln1$household,..count..
      )) + geom_bar(aes(fill=reddit.sorted.cln1$income
      )) + xlab("Household") + ylab("count") + ggtitle("Household vs Income") + theme(axis.text.x=element_text(angle = 90,hjust = 1))
ggplot(reddit.sorted.cln1,aes(reddit.sorted.cln1$education,..count..
      )) + geom_bar(aes(fill=reddit.sorted.cln1$income
      )) + xlab("Education") + ylab("count") + ggtitle("Education vs Income") + theme(axis.text.x=element_text(angle = 90,hjust = 1))
#convert continent data to categorical as well. Continent will be easier to plot
reddit.sorted.cln1$continent <- as.factor(reddit.sorted.cln1$continent)
ggplot(reddit.sorted.cln1,aes(reddit.sorted.cln1$continent,..count..
      )) + geom_bar(aes(fill=reddit.sorted.cln1$income
      )) + xlab("continent") + ylab("count") + ggtitle("continent vs Income") + theme(axis.text.x=element_text(angle = 90,hjust = 1))
ggplot(reddit.sorted.cln1,aes(x=reddit.sorted.cln1$dogcat,..count..)) + geom_bar(aes(fill=reddit.sorted.cln1$income)) + 
      xlab("dog or cat") + ylab("count") + ggtitle("dog or cat vs income") + theme(axis.text.x = element_text(angle = 90,hjust = 1))
#correlation matrix
library(polycor)
reddit.sorted.cln1$country <- as.factor(reddit.sorted.cln1$country)
polycor <- hetcor(reddit.sorted.cln1)
polycor$correlations
library(corrplot)
corrplot(polycor$correlations,method="color",type = "lower")
#we are ready for random forest
index <- sample(nrow(reddit.sorted.cln1),nrow(reddit.sorted.cln1)/3)
reddit.test <- reddit.sorted.cln1[index, -which(names(reddit.sorted.cln1) %in% c("country"))]
reddit.train <- reddit.sorted.cln1[-index,-which(names(reddit.sorted.cln1) %in% c("country"))]
reddit.test.label <- reddit.test[,which(names(reddit.test) %in% c("income"))]
reddit.train.label <- reddit.train[,which(names(reddit.train) %in% c("income"))]
#remove dependent variabe
reddit.train.tmp <- reddit.train[,-which(names(reddit.train) %in% c("income"))]
reddit.test.tmp <- reddit.test[,-which(names(reddit.test) %in% c("income"))]
#build model
library(randomForest)
model <- randomForest(reddit.train$income~.,data = reddit.train.tmp,importance=TRUE,ntree=2000,sampsize=nrow(reddit.train.tmp),replace=TRUE)
model


