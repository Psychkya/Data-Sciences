loan.test <- read.csv("/home/awaz/Documents/UISCourses/DataScience/midterm_test.csv", stringsAsFactors = FALSE)
str(loan.test)
#First try with selected variable
loan.second.tst <- loan.test[,-which(names(loan.test) %in% c("Loan_ID", "Borrower_ID", "Employer", "Loan_Description",
                                                           "Purpose_Of_Loan", "Loan_Title", "Zip_3", "Month_Earliest_Credit", "Months_Since_Record",
                                                           "Public_Record_Count", "Outstanding_Principal", "Outstanding_Principal_Remaining", 
                                                           "Last_Payment_Received", "Last_Payment_Total", "Next_Payment_date", "Policy_Code",
                                                           "Length_Employed", "Month_Issued", "State", "Number_Delinqueny_2yrs"))]
str(loan.second.tst)
#remove $, % and ',' from data
intrtonum <- function(x){gsub("%|\\$|,|[a-zA-Z]|\\s+","",x)}
#mode function
Defmode <- function(x){ ux <- unique(x); ux[which.max(tabulate(match(x,ux)))]}
#clean loan_amount_requested
loan.second.tst$Loan_Amount_Requested <- sapply(loan.second.tst$Loan_Amount_Requested, intrtonum)
loan.second.tst$Loan_Amount_Requested <- as.numeric(loan.second.tst$Loan_Amount_Requested)
#clean loan amount funded
loan.second.tst$Loan_Amount_Funded <- sapply(loan.second.tst$Loan_Amount_Funded, intrtonum)
loan.second.tst$Loan_Amount_Funded <- as.numeric(loan.second.tst$Loan_Amount_Funded)
#Clean loan amount funded by investor
loan.second.tst$Loan_Amount_Funded_by_Investors <- sapply(loan.second.tst$Loan_Amount_Funded_by_Investors,intrtonum)
loan.second.tst$Loan_Amount_Funded_by_Investors <- as.numeric(loan.second.tst$Loan_Amount_Funded_by_Investors)
#clean initial loan term
loan.second.tst$Initial_Loan_Term_Months <- sapply(loan.second.tst$Initial_Loan_Term_Months, intrtonum)
loan.second.tst$Initial_Loan_Term_Months <- as.numeric(loan.second.tst$Initial_Loan_Term_Months)
#check loan grade
loan.second.tst$Loan_Grade[loan.second.tst$Loan_Grade==""] <- "0"
#One hot encoding for loan grade
convert_loan <- function(a,b){
  switch (b,
          A0 = {a <- ifelse(a=="A" | a=="0", "1", "0")},
          BCD = {a <- ifelse(a=="B" | a=="C" | a=="D", "1", "0")}
  )
}
loan.second.tst$GA0 <- sapply(loan.second.tst$Loan_Grade,convert_loan,b="A0")
loan.second.tst$GA0 <- as.numeric(loan.second.tst$GA0)
loan.second.tst$GBCD <- sapply(loan.second.tst$Loan_Grade,convert_loan,b="BCD")
loan.second.tst$GBCD <- as.numeric(loan.second.tst$GBCD)
#convert "" in loan sub grade to 0
#loan.second.tst$Sub_Loan_Grade[loan.second.tst$Sub_Loan_Grade==""] <- "0"
#loan.second.tst$Sub_Loan_Grade <- as.factor(loan.second.tst$Sub_Loan_Grade)
#Home owner cleanup
#Drop bad homes
convert_home <- function(a,b){
  switch (b,
          OWN = {a <- ifelse(a=="OWN" | a=="MORTGAGE", "1", "0")},
          RENT= {a <- ifelse(a=="RENT", "1", "0")}
  )
}
loan.second.tst$Home_own <- sapply(loan.second.tst$Home_Owner.,convert_home,b="OWN")
loan.second.tst$Home_own <- as.numeric(loan.second.tst$Home_own)
loan.second.tst$Home_Rent <- sapply(loan.second.tst$Home_Owner.,convert_home,b="RENT")
loan.second.tst$Home_Rent <- as.numeric(loan.second.tst$Home_Rent)
#Clean annual income
#loan.second.tst$Annual_Income <- sapply(loan.second.tst$Annual_Income, intrtonum)
#temp <- as.numeric(loan.second.tst$Annual_Income)
#loan.second.tst <- loan.second.tst[na.omit(loan.second.tst$Annual_Income),]
#Clean Income verification
#First will convert 'Verified' to 'yes' and 'not-verfied' to 'no'. Also convert all income values to 'no'
loan.second.tst$Income_Verified[grep("VERIFIED",loan.second.tst$Income_Verified)] <- "1"
loan.second.tst$Income_Verified[grep("not",loan.second.tst$Income_Verified)] <- "0"
loan.second.tst$Income_Verified <- as.numeric(loan.second.tst$Income_Verified)
loan.second.tst$Income_Verified[is.na(loan.second.tst$Income_Verified)] <- 0
#-------
#clean current status
#There are very few obs with month-yr date and one 'VERIFIED-income'. Best to get rid of these obs
#index_stat <- grep("Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec|VERIFIED",loan.second.tst$Current_Status)
#loan.second.tst <- loan.second.tst[-index_stat,]
#loan.second.tst$Current_Status <- as.factor(loan.second.tst$Current_Status)
#Payment plan cleanup
loan.second.tst$Payment_Plan_Ind[loan.second.tst$Payment_Plan_Ind=="y"] <- "1"
loan.second.tst$Payment_Plan_Ind[loan.second.tst$Payment_Plan_Ind=="n"] <- "0"
loan.second.tst$Payment_Plan_Ind <- as.numeric(loan.second.tst$Payment_Plan_Ind)
loan.second.tst$Payment_Plan_Ind[is.na(loan.second.tst$Payment_Plan_Ind)] <- 0
#Clean debt to income
loan.second.tst$Debt_To_Income <- sapply(loan.second.tst$Debt_To_Income, intrtonum)
temp <- as.numeric(loan.second.tst$Debt_To_Income)
temp[is.na(temp)] <- 0
loan.second.tst$Debt_To_Income <- temp
#Clean month since dilinquency
loan.second.tst$Months_Since_Deliquency[loan.second.tst$Months_Since_Deliquency==""] <- 0
temp <- floor(as.numeric(loan.second.tst$Months_Since_Deliquency))
temp[is.na(temp)] <- 0
loan.second.tst$Months_Since_Deliquency <- temp
#clean inquiries_last_6Mo
#first round, we will convert all alpha values to 0
loan.second.tst$Inquiries_Last_6Mo <- floor(as.numeric(loan.second.tst$Inquiries_Last_6Mo))
loan.second.tst$Inquiries_Last_6Mo[is.na(loan.second.tst$Inquiries_Last_6Mo)] <- 0
#clean number of open accounts. 
loan.second.tst$Number_Open_Accounts <- floor(as.numeric(loan.second.tst$Number_Open_Accounts))
loan.second.tst$Number_Open_Accounts[is.na(loan.second.tst$Number_Open_Accounts)] <- 0
#Clean revolving balance. Change NA to 0
temp <- as.numeric(loan.second.tst$Revolving_Balance)
temp[is.na(temp)] <- 0
loan.second.tst$Revolving_Balance <- temp
#Clean revolving util
loan.second.tst$Revolving_Utilization <- sapply(loan.second.tst$Revolving_Utilization, intrtonum)
temp <- as.numeric(loan.second.tst$Revolving_Utilization)/100
temp[is.na(temp)] <- 0
loan.second.tst$Revolving_Utilization <- temp
#Clean total accounts
temp <- as.numeric(loan.second.tst$Total_Accounts)
temp[is.na(temp)] <- 0
loan.second.tst$Total_Accounts <- temp
#Clean initial status
#There a few numeric values - we will assume these to be 'f'
#loan.second.tst$Initial_Status[grep("[0-9]",loan.second.tst$Initial_Status)] <- "f"
#loan.second.tst$Initial_Status <- as.factor(loan.second.tst$Initial_Status)
#Clean total payment
loan.second.tst$Total_Payment <- sapply(loan.second.tst$Total_Payment,intrtonum)
temp <- as.numeric(loan.second.tst$Total_Payment)
temp[is.na(temp)] <- 0
loan.second.tst$Total_Payment <- temp
#clean total payment funded
loan.second.tst$Total_Payment_Funded <- sapply(loan.second.tst$Total_Payment_Funded, intrtonum)
temp <- as.numeric(loan.second.tst$Total_Payment_Funded)
temp[is.na(temp)] <- 0
loan.second.tst$Total_Payment_Funded <- temp
#Clean Total Received
loan.second.tst$Total_Received <- sapply(loan.second.tst$Total_Received, intrtonum)
temp <- as.numeric(loan.second.tst$Total_Received)
temp[is.na(temp)] <- 0
loan.second.tst$Total_Received <- temp
#Clean total received interest
loan.second.tst$Total_Received_Interest <- sapply(loan.second.tst$Total_Received_Interest, intrtonum)
temp <- as.numeric(loan.second.tst$Total_Received_Interest)
temp[is.na(temp)] <- 0
loan.second.tst$Total_Received_Interest <- temp
#Clean total received late fees
loan.second.tst$Total_Received_Late_Fees <- sapply(loan.second.tst$Total_Received_Late_Fees,intrtonum)
temp <- as.numeric(loan.second.tst$Total_Received_Late_Fees)
temp[is.na(temp)] <- 0
loan.second.tst$Total_Received_Late_Fees <- temp
#Clean recoveries
loan.second.tst$Recoveries <- sapply(loan.second.tst$Recoveries,intrtonum)
temp <- as.numeric(loan.second.tst$Recoveries)
temp[is.na(temp)] <- 0
loan.second.tst$Recoveries <- temp
#Clean collection fee
loan.second.tst$Collection_Recovery_Fee <- sapply(loan.second.tst$Collection_Recovery_Fee,intrtonum)
temp <- as.numeric(loan.second.tst$Collection_Recovery_Fee)
temp[is.na(temp)] <- 0
loan.second.tst$Collection_Recovery_Fee <- temp
#Derogatory record
convert_der <- function(x){x <- ifelse(x=="", "0", "1")}
loan.second.tst$Derogatory <- sapply(loan.second.tst$Months_Since_Last_Derogatory, convert_der)
loan.second.tst$Derogatory <- as.numeric(loan.second.tst$Derogatory)
#Collection
loan.second.tst$Collections <- as.numeric(loan.second.tst$Collections_12Mo_Exclude_Med)
loan.second.tst$Collections[is.na(loan.second.tst$Collections)] <- 0
  #We will omit a few more columns
loan.second.tst <- loan.second.tst[,-which(names(loan.second.tst) %in% c("Loan_Grade","Sub_Loan_Grade","Home_Owner.","Annual_Income",
                                                               "Current_Status","Initial_Status","Next_Payment_Date",
                                                               "Last_Credit_Pulled","Collections_12Mo_Exclude_Med",
                                                               "Months_Since_Last_Derogatory","Collections"))]
str(loan.second.tst)
#Remove a few more columns to build model
loan.second.tst.sub <- loan.second.tst[,-which(names(loan.second.tst) %in% c("Total_Payment","Total_Payment_Funded","Total_Received",
                                                                 "Total_Received_Interest","Total_Received_Late_Fees",
                                                                 "Recoveries","Collection_Recovery_Fee","Months_Since_Deliquency",
                                                                 "Revolving_Balance"))]
str(loan.second.tst.sub)

#Predict results
results.sub <- predict(rmodel,loan.second.tst.sub[,-1])

submit.data <- data.frame(Id=loan.second.tst.sub$X, Rate=results.sub)
write.csv(submit.data,file = "/home/awaz/Documents/UISCourses/DataScience/submit4.csv",row.names = FALSE)

#Predict results
results.sub1 <- predict(rfmodel,loan.second.tst.sub[,-1])
submit.data1 <- data.frame(Id=loan.second.tst.sub$X,Rate=results.sub1)
write.csv(submit.data1,file = "/home/awaz/Documents/UISCourses/DataScience/submit5.csv",row.names = FALSE)
