loan.test <- read.csv("/home/awaz/Documents/UISCourses/DataScience/midterm_test.csv", stringsAsFactors = FALSE)
str(loan.test)
#First try with selected variable
loan.third.tst <- loan.test[,-which(names(loan.test) %in% c("Loan_ID", "Borrower_ID", "Employer", "Loan_Description",
                                                          "Purpose_Of_Loan", "Loan_Title", "Zip_3", "Month_Earliest_Credit", "Months_Since_Record",
                                                          "Public_Record_Count", "Outstanding_Principal", "Outstanding_Principal_Remaining", 
                                                          "Last_Payment_Received", "Last_Payment_Total", "Next_Payment_date", "Policy_Code",
                                                          "Length_Employed", "Month_Issued", "State", "Number_Delinqueny_2yrs"))]
str(loan.third.tst)
#remove $, % and ',' from data
intrtonum <- function(x){gsub("%|\\$|,|[a-zA-Z]|\\s+","",x)}
#mode function
Defmode <- function(x){ ux <- unique(x); ux[which.max(tabulate(match(x,ux)))]}
#clean loan_amount_requested
loan.third.tst$Loan_Amount_Requested <- sapply(loan.third.tst$Loan_Amount_Requested, intrtonum)
temp <- as.numeric(loan.third.tst$Loan_Amount_Requested)
loan.third.tst$Loan_Amount_Requested <- temp
#clean initial loan term
loan.third.tst$Initial_Loan_Term_Months <- sapply(loan.third.tst$Initial_Loan_Term_Months, intrtonum)
temp <- as.numeric(loan.third.tst$Initial_Loan_Term_Months)
loan.third.tst$Initial_Loan_Term_Months <- temp
#check loan grade
loan.third.tst$Loan_Grade[loan.third.tst$Loan_Grade==""] <- "X"
loan.third.tst$Loan_Grade <- as.factor(loan.third.tst$Loan_Grade)
#convert "" in loan sub grade to 0
loan.third.tst$Sub_Loan_Grade[loan.third.tst$Sub_Loan_Grade==""] <- "X"
loan.third.tst$Sub_Loan_Grade <- as.factor(loan.third.tst$Sub_Loan_Grade)
#Home owner cleanup DROPPING HOME OWNER
#Some homeowner got moved to income field. We will move these over
#loan.test$Home_Owner.[grep("year",loan.test$Home_Owner.)] <- loan.test$Annual_Income[grep("year",loan.test$Home_Owner.)]
#loan.third.tst$Home_Owner. <- loan.test$Home_Owner.
#18k records with no home-owner, COMEBACK TO THIS
#Clean Income verification
loan.test$Income_Verified[grep("[0-9]",loan.test$Income_Verified)] <- loan.test$Month_Issued[grep("[0-9]",loan.test$Income_Verified)]
loan.third.tst$Income_Verified <- loan.test$Income_Verified
loan.third.tst$Income_Verified[grep("VERIFIED",loan.third.tst$Income_Verified)] <- "YES"
loan.third.tst$Income_Verified[grep("not",loan.third.tst$Income_Verified)] <- "NO"
loan.third.tst$Income_Verified[loan.third.tst$Income_Verified==""] <- "NO"
loan.third.tst$Income_Verified <- as.factor(loan.third.tst$Income_Verified)
#-------
#Payment plan cleanup
loan.third.tst$Payment_Plan_Ind[grep("Charged Off|Curr|Fully",loan.test$Payment_Plan_Ind)] <- "n"
loan.third.tst$Payment_Plan_Ind <- as.factor(loan.third.tst$Payment_Plan_Ind)
#Clean debt to income
loan.third.tst$Debt_To_Income <- sapply(loan.third.tst$Debt_To_Income, intrtonum)
#columns are shifted again
temp <- as.numeric(loan.third.tst$Debt_To_Income)
loan.third.tst$Debt_To_Income[is.na(temp)] <- loan.test$Number_Delinqueny_2yrs[is.na(temp)]
temp <- as.numeric(loan.third.tst$Debt_To_Income)
#More shifting
loan.third.tst$Debt_To_Income[is.na(temp)] <- loan.test$Inquiries_Last_6Mo[is.na(temp)]
temp <- as.numeric(loan.third.tst$Debt_To_Income)
#Even more shifting
loan.third.tst$Debt_To_Income[is.na(temp)] <- loan.test$Months_Since_Deliquency[is.na(temp)]
temp <- as.numeric(loan.third.tst$Debt_To_Income)
#Some more shifting!
loan.third.tst$Debt_To_Income[1785] <- "12.66"
loan.third.tst$Debt_To_Income[6748] <- "13.93"
loan.third.tst$Debt_To_Income[56941] <- "16.27"
loan.third.tst$Debt_To_Income[23522] <- "22.21"
temp <- as.numeric(loan.third.tst$Debt_To_Income)
temp[is.na(temp)] <- Defmode(na.omit(temp))
loan.third.tst$Debt_To_Income <- temp
#clean inquiries_last_6Mo
loan.third.tst$Inquiries_Last_6Mo[grep("\\.",loan.third.tst$Inquiries_Last_6Mo)] <- 
          loan.third.tst$Months_Since_Deliquency[grep("\\.",loan.third.tst$Inquiries_Last_6Mo)]
temp <- as.numeric(loan.third.tst$Inquiries_Last_6Mo)
loan.third.tst$Inquiries_Last_6Mo[is.na(temp)] <- loan.third.tst$Months_Since_Deliquency[is.na(temp)]
temp <- as.numeric(loan.third.tst$Inquiries_Last_6Mo)
loan.third.tst$Inquiries_Last_6Mo[is.na(temp)] <- loan.third.tst$Revolving_Utilization[is.na(temp)]
loan.third.tst$Inquiries_Last_6Mo[grep("\\.",loan.third.tst$Inquiries_Last_6Mo)] <- "0"
temp <- floor(as.numeric(loan.third.tst$Inquiries_Last_6Mo))
temp[is.na(temp)] <- 0
loan.third.tst$Inquiries_Last_6Mo <- temp
#Clean month since dilinquency
loan.third.tst$Months_Since_Deliquency[loan.third.tst$Months_Since_Deliquency==""] <- 0
temp <- floor(as.numeric(loan.third.tst$Months_Since_Deliquency))
loan.third.tst$Months_Since_Deliquency[is.na(temp)] <- loan.test$Months_Since_Record[is.na(temp)]
temp <- floor(as.numeric(loan.third.tst$Months_Since_Deliquency))
temp[is.na(temp)] <- 0
convert_dil <- function(x){x <- ifelse(x==0 | x > 36, "NO", "YES")}
loan.third.tst$Delinquency <- sapply(temp,convert_dil)
loan.third.tst$Delinquency <- as.factor(loan.third.tst$Delinquency)
#clean number of open accounts. 
temp <- as.numeric(loan.third.tst$Number_Open_Accounts)
loan.third.tst$Number_Open_Accounts[is.na(temp)] <- loan.test$Public_Record_Count[is.na(temp)]
temp <- as.numeric(loan.third.tst$Number_Open_Accounts)
loan.third.tst$Number_Open_Accounts[107399] <- "8"
loan.third.tst$Number_Open_Accounts[is.na(temp)] <- loan.test$Revolving_Balance[is.na(temp)]
loan.third.tst$Number_Open_Accounts[23522] <- "9"
temp <- as.numeric(loan.third.tst$Number_Open_Accounts)
temp[is.na(temp)] <- Defmode(na.omit(temp))
loan.third.tst$Number_Open_Accounts <- temp
#Clean revolving util
index_rev <- grep("%",loan.third.tst$Revolving_Utilization)
loan.third.tst$Revolving_Utilization[-index_rev] <- loan.test$Total_Accounts[-index_rev]
loan.third.tst$Revolving_Utilization[-grep("%",loan.third.tst$Revolving_Utilization)] <-
          loan.test$Initial_Status[-grep("%",loan.third.tst$Revolving_Utilization)]
loan.third.tst$Revolving_Utilization[-grep("%",loan.third.tst$Revolving_Utilization)] <-
  loan.test$Outstanding_Principal[-grep("%",loan.third.tst$Revolving_Utilization)]
loan.third.tst$Revolving_Utilization[-grep("%",loan.third.tst$Revolving_Utilization)] <-
  loan.test$Outstanding_Principal_Remaining[-grep("%",loan.third.tst$Revolving_Utilization)]
loan.third.tst$Revolving_Utilization[-grep("%",loan.third.tst$Revolving_Utilization)] <-
  loan.test$Total_Payment_Funded[-grep("%",loan.third.tst$Revolving_Utilization)]
loan.third.tst$Revolving_Utilization[-grep("%",loan.third.tst$Revolving_Utilization)] <- ""
loan.third.tst$Revolving_Utilization <- sapply(loan.third.tst$Revolving_Utilization, intrtonum)
temp <- as.numeric(loan.third.tst$Revolving_Utilization)/100
temp[is.na(temp)] <- median(na.omit(temp))
loan.third.tst$Revolving_Utilization <- temp
#Clean total accounts
loan.third.tst$Total_Accounts[grep("\\.",loan.third.tst$Total_Accounts)] <- 
            loan.test$Initial_Status[grep("\\.",loan.third.tst$Total_Accounts)]
loan.third.tst$Total_Accounts[grep("%",loan.third.tst$Total_Accounts)] <-
            loan.test$Initial_Status[grep("%",loan.third.tst$Total_Accounts)]
loan.third.tst$Total_Accounts[is.na(temp)] <- loan.test$Initial_Status[is.na(temp)]
loan.third.tst$Total_Accounts[is.na(temp)] <- loan.test$Outstanding_Principal[is.na(temp)]
temp <- as.numeric(loan.third.tst$Total_Accounts)
temp[is.na(temp)] <- Defmode(na.omit(temp))
loan.third.tst$Total_Accounts <- temp
#Clean initial status
#There a few numeric values - we will assume these to be 'f'
#loan.third.tst$Initial_Status[grep("[0-9]",loan.third.tst$Initial_Status)] <- "f"
#loan.third.tst$Initial_Status <- as.factor(loan.third.tst$Initial_Status)
#Derogatory record
loan.third.tst$Months_Since_Last_Derogatory[grep("[a-zA-Z]",loan.third.tst$Months_Since_Last_Derogatory)] <-
            loan.test$Policy_Code[grep("[a-zA-Z]",loan.third.tst$Months_Since_Last_Derogatory)]
temp <- as.numeric(loan.third.tst$Months_Since_Last_Derogatory)
loan.third.tst$Months_Since_Last_Derogatory[which(temp > 1000)] <- ""
loan.third.tst$Months_Since_Last_Derogatory[grep("[a-zA-Z]",loan.third.tst$Months_Since_Last_Derogatory)] <- ""
convert_der <- function(x){x <- ifelse(x=="", "NO", "YES")}
loan.third.tst$Derogatory <- sapply(loan.third.tst$Months_Since_Last_Derogatory, convert_der)
loan.third.tst$Derogatory <- as.factor(loan.third.tst$Derogatory)
str(loan.third.tst)
#Remove a few more columns to build model
loan.third.tst.sub <- loan.third.tst[,-which(names(loan.third.tst) %in% c("Loan_Amount_Funded","Loan_Amount_Funded_by_Investors",
                                                              "Current_Status","Initial_Status","Total_Payment","Total_Payment_Funded",
                                                              "Total_Received_Interest","Total_Received_Late_Fees","Next_Payment_Date",
                                                              "Recoveries","Collection_Recovery_Fee","Months_Since_Deliquency",
                                                              "Revolving_Balance","Total_Received","Last_Credit_Pulled",
                                                              "Collections_12Mo_Exclude_Med","Months_Since_Last_Derogatory",
                                                              "Home_Owner."))]
str(loan.third.tst.sub)
#Clean home owner
#loan.third.tst.sub$Home_Owner.[loan.third.tst.sub$Home_Owner.=="0"] <- "NONE"
#loan.third.tst.sub$Home_Owner.[loan.third.tst.sub$Home_Owner.==""] <- "MORTGAGE"
#loan.third.tst.sub$Home_Owner. <- as.factor(loan.third.tst.sub$Home_Owner.)
#Clean annual income ! COME BACK TO THIS !
ann.income <- loan.third.tst.sub[,-which(names(loan.third.tst.sub) %in% c("X"))]
ann.income$Annual_Income <- as.numeric(ann.income$Annual_Income)
library(rpart)
Annual_Income <- ann.income$Annual_Income[!is.na(ann.income$Annual_Income)]
rpanninc1 <- rpart(Annual_Income~.,
                  data=ann.income[!is.na(ann.income$Annual_Income),-which(names(ann.income) %in% c("Annual_Income"))],
                  method = "anova")
ann.income$Annual_Income[is.na(ann.income$Annual_Income)] <- predict(rpanninc1,
                                    ann.income[is.na(ann.income$Annual_Income),-which(names(ann.income) %in% c("Annual_Income"))])
sum(is.na(ann.income$Annual_Income))
loan.third.tst.sub$Annual_Income <- ann.income$Annual_Income

#Predict results
results.sub5 <- predict(rfmodel4,loan.third.tst.sub[,-1])
submit.data5 <- data.frame(Id=loan.third.tst.sub$X,Rate=results.sub5)
write.csv(submit.data5,file = "/home/awaz/Documents/UISCourses/DataScience/submit9.csv",row.names = FALSE)
