loan.train <- read.csv("/home/awaz/Documents/UISCourses/DataScience/midterm_train.csv", stringsAsFactors = FALSE)
str(loan.train)
#First try with selected variable
loan.third <- loan.train[,-which(names(loan.train) %in% c("Loan_ID", "Borrower_ID", "Employer", "Loan_Description",
                          "Purpose_Of_Loan", "Loan_Title", "Zip_3", "Month_Earliest_Credit", "Months_Since_Record",
                          "Public_Record_Count", "Outstanding_Principal", "Outstanding_Principal_Remaining", 
                          "Last_Payment_Received", "Last_Payment_Total", "Next_Payment_date", "Policy_Code",
                          "Length_Employed", "Month_Issued", "State", "Number_Delinqueny_2yrs"))]
str(loan.third)
#remove $, % and ',' from data
intrtonum <- function(x){gsub("%|\\$|,|[a-zA-Z]|\\s+","",x)}
#mode function
Defmode <- function(x){ ux <- unique(x); ux[which.max(tabulate(match(x,ux)))]}
#clean interest rate
loan.third$Interest_Rate <- sapply(loan.third$Interest_Rate, intrtonum)
loan.third$Interest_Rate <- as.numeric(loan.third$Interest_Rate)
loan.third$Interest_Rate <- loan.third$Interest_Rate/100
#there is NA in interest rate. Since this is our dependent var, we will omit those
loan.third <- loan.third[!is.na(loan.third$Interest_Rate),]
#clean loan_amount_requested
loan.third$Loan_Amount_Requested <- sapply(loan.third$Loan_Amount_Requested, intrtonum)
loan.third$Loan_Amount_Requested <- as.numeric(loan.third$Loan_Amount_Requested)
#clean initial loan term
loan.third$Initial_Loan_Term_Months <- sapply(loan.third$Initial_Loan_Term_Months, intrtonum)
loan.third$Initial_Loan_Term_Months <- as.numeric(loan.third$Initial_Loan_Term_Months)
#check loan grade
loan.third$Loan_Grade[loan.third$Loan_Grade==""] <- "X"
loan.third$Loan_Grade <- as.factor(loan.third$Loan_Grade)
#convert "" in loan sub grade to 0
loan.third$Sub_Loan_Grade[loan.third$Sub_Loan_Grade==""] <- "X"
loan.third$Sub_Loan_Grade <- as.factor(loan.third$Sub_Loan_Grade)
#Home owner cleanup. ENDED UP DROPPING HOME OWNER
#Drop bad homes
#h_index <- grep("year|Acctg|ANY",loan.third$Home_Owner.)
#loan.third <- loan.third[-h_index,]
#loan.third <- loan.third[-which(loan.third$Home_Owner. %in% c("","0")),]
#loan.third$Home_Owner. <- as.factor(loan.third$Home_Owner.)
#Clean Income verification
loan.third <- loan.third[-grep("[0-9]|MORTGAGE",loan.third$Income_Verified),]
loan.third$Income_Verified[loan.third$Income_Verified==""] <- "YES"
loan.third$Income_Verified[grep("VERIFIED",loan.third$Income_Verified)] <- "YES"
loan.third$Income_Verified[grep("not",loan.third$Income_Verified)] <- "NO"
loan.third$Income_Verified <- as.factor(loan.third$Income_Verified)
#-------
#clean current status
#There are very few obs with month-yr date and one 'VERIFIED-income'. Best to get rid of these obs
#index_stat <- grep("Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec|VERIFIED",loan.third$Current_Status)
#loan.third <- loan.third[-index_stat,]
#loan.third$Current_Status <- as.factor(loan.third$Current_Status)
#Payment plan cleanup. Getting rid of garbage
index_pay <- grep("Charged|Current|Fully|Sep-14",loan.third$Payment_Plan_Ind)
loan.third <- loan.third[-index_pay,]
loan.third$Payment_Plan_Ind <- as.factor(loan.third$Payment_Plan_Ind)
#Clean debt to income
loan.third$Debt_To_Income <- sapply(loan.third$Debt_To_Income, intrtonum)
temp <- as.numeric(loan.third$Debt_To_Income)
loan.third$Debt_To_Income <- temp
loan.third <- loan.third[na.omit(loan.third$Debt_To_Income),]
#Clean month since dilinquency
#There are 289 records that have some sort of alpha value - mosthly a month-yr instead of number of months. We will drop these
loan.third$Months_Since_Deliquency[loan.third$Months_Since_Deliquency==""] <- 0
temp <- floor(as.numeric(loan.third$Months_Since_Deliquency))
convert_dil <- function(x){x <- ifelse(x==0 | x > 36, "NO", "YES")}
loan.third$Delinquency <- sapply(temp,convert_dil)
loan.third$Delinquency <- as.factor(loan.third$Delinquency)
#clean inquiries_last_6Mo
#first round, we will convert all alpha values to 0
loan.third$Inquiries_Last_6Mo <- floor(as.numeric(loan.third$Inquiries_Last_6Mo))
#clean number of open accounts. 
loan.third$Number_Open_Accounts <- floor(as.numeric(loan.third$Number_Open_Accounts))
#Clean revolving util
loan.third$Revolving_Utilization <- sapply(loan.third$Revolving_Utilization, intrtonum)
temp <- as.numeric(loan.third$Revolving_Utilization)/100
loan.third$Revolving_Utilization <- temp
#Clean total accounts
temp <- as.numeric(loan.third$Total_Accounts)
loan.third$Total_Accounts <- temp
#Clean initial status
#There a few numeric values - we will assume these to be 'f'
#loan.third$Initial_Status[grep("[0-9]",loan.third$Initial_Status)] <- "f"
#loan.third$Initial_Status <- as.factor(loan.third$Initial_Status)
#Derogatory record
convert_der <- function(x){x <- ifelse(x=="", "NO", "YES")}
loan.third$Derogatory <- sapply(loan.third$Months_Since_Last_Derogatory, convert_der)
loan.third$Derogatory <- as.factor(loan.third$Derogatory)
str(loan.third)
#Remove a few more columns to build model
loan.third.sub <- loan.third[,-which(names(loan.third) %in% c("Loan_Amount_Funded","Loan_Amount_Funded_by_Investors",
                                                              "Current_Status","Initial_Status","Total_Payment","Total_Payment_Funded",
                                                                 "Total_Received_Interest","Total_Received_Late_Fees","Next_Payment_Date",
                                                                 "Recoveries","Collection_Recovery_Fee","Months_Since_Deliquency",
                                                                 "Revolving_Balance","Total_Received","Last_Credit_Pulled",
                                                                "Collections_12Mo_Exclude_Med","Months_Since_Last_Derogatory",
                                                                "Home_Owner."))]
str(loan.third.sub)
#Clean annual income ! COME BACK TO THIS !
loan.third.sub$Annual_Income <- sapply(loan.third.sub$Annual_Income, intrtonum)
ann.income.tr <- loan.third.sub[,-which(names(loan.third.sub) %in% c("Interest_Rate"))]
ann.income.tr$Annual_Income <- as.numeric(ann.income.tr$Annual_Income)
library(rpart)
Annual_Income <- ann.income.tr$Annual_Income[!is.na(ann.income.tr$Annual_Income)]
rpanninc <- rpart(Annual_Income~.,
                  data=ann.income.tr[!is.na(ann.income.tr$Annual_Income),-which(names(ann.income.tr) %in% c("Annual_Income"))],
                  method = "anova")
ann.income.tr$Annual_Income[is.na(ann.income.tr$Annual_Income)] <- predict(rpanninc,
                                ann.income.tr[is.na(ann.income.tr$Annual_Income),-which(names(ann.income.tr) %in% c("Annual_Income"))])
sum(is.na(ann.income.tr$Annual_Income))
loan.third.sub$Annual_Income <- ann.income.tr$Annual_Income

#temp <- as.numeric(loan.third.sub$Annual_Income)
#temp[is.na(temp)] <- median(na.omit(temp))
#loan.third.sub$Annual_Income <- temp
#Set seed
set.seed(100)
#randomize the training 
#split train and test data
library(randomForest)
k <- 5
n <- floor(nrow(loan.third)/k)
v.rmse <- rep(NA,k)
v.r2 <- rep(NA,k)
for (i in 1:k) {
  s1 <- (i-1)*n + 1
  s2 <- i*n
  index <- s1:s2
  loan.third.sub.test <- loan.third.sub[index,]
  loan.third.sub.train <- loan.third.sub[-index,]
  rfmodel4 <- randomForest(loan.third.sub.train$Interest_Rate~.,data=loan.third.sub.train[,-1],
                           sampsize=nrow(loan.third.sub.train),replace=TRUE,ntree=101,Proximity=FALSE,importance=TRUE) 
  results4 <- predict(rfmodel4, loan.third.sub.test[,-1])
  v.rmse[i] <- sqrt(mean((loan.third.sub.test[,1]) - results4)^2)
  SSResidual <- sum((loan.third.sub.test[,1] - results4)^2)
  SSTotal <- sum((loan.third.sub.test[,1] - mean(loan.third.sub.test[,1]))^2)
  v.r2[i] <- 1 - (SSResidual/SSTotal)

}

#Bagging
#loan.third.sub.train <- loan.third.sub.train[sample(1:nrow(loan.third.sub.train), replace = TRUE),]
#Build model

