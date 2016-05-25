maddie <- read.csv('/home/awaz/Documents/UISCourses/DataScience/AnimalShelter/Maddies_Dataset.csv',stringsAsFactors = FALSE)
cols.d <- c("Community_State", "Total_Intake_D", "Total_Adoptions_D", "Transfer_In_Comm_Total_D", "Transfer_Out_Comm_Total_D",
          "RTO_D", "Total_Euth_Adj_D", "Died_Lost_D", "Total_Outcomes_D", "End_Count_D")
cols.c <- c("Community_State", "Total_Intake_C", "Total_Adoptions_C", "Transfer_In_Comm_Total_C", "Transfer_Out_Comm_Total_C",
            "RTO_C", "Total_Euth_Adj_C", "Died_Lost_C", "Total_Outcomes_C", "End_Count_C")
maddie.dogs <- maddie[,which(colnames(maddie) %in% cols.d)]
maddie.cats <- maddie[,which(colnames(maddie) %in% cols.c)]
#Dogs first
maddie.dogs$Community_State <- as.factor(maddie.dogs$Community_State)
chartonum <- function(x){gsub("%|\\$|,|[a-zA-Z]|\\s+","",x)}
maddie.dogs$Total_Intake_D <- sapply(maddie.dogs$Total_Intake_D,chartonum)
maddie.dogs$Total_Intake_D <- as.numeric(maddie.dogs$Total_Intake_D)
maddie.dogs$Total_Adoptions_D <- sapply(maddie.dogs$Total_Adoptions_D,chartonum)
maddie.dogs$Total_Adoptions_D <- as.numeric(maddie.dogs$Total_Adoptions_D)
maddie.dogs$Total_Adoptions_D[is.na(maddie.dogs$Total_Adoptions_D)] <- 0
maddie.dogs$Transfer_In_Comm_Total_D <- sapply(maddie.dogs$Transfer_In_Comm_Total_D,chartonum)
maddie.dogs$Transfer_In_Comm_Total_D <- as.numeric(maddie.dogs$Transfer_In_Comm_Total_D)
maddie.dogs$Transfer_Out_Comm_Total_D <- sapply(maddie.dogs$Transfer_Out_Comm_Total_D, chartonum)
maddie.dogs$Transfer_Out_Comm_Total_D <- as.numeric(maddie.dogs$Transfer_Out_Comm_Total_D)
maddie.dogs$Transfer_Out_Comm_Total_D[is.na(maddie.dogs$Transfer_Out_Comm_Total_D)] <- 0
maddie.dogs$RTO_D <- sapply(maddie.dogs$RTO_D,chartonum)
maddie.dogs$RTO_D <- as.numeric(maddie.dogs$RTO_D)
maddie.dogs$RTO_D[is.na(maddie.dogs$RTO_D)] <- 0
maddie.dogs$Total_Euth_Adj_D <- sapply(maddie.dogs$Total_Euth_Adj_D,chartonum)
maddie.dogs$Total_Euth_Adj_D <- as.numeric(maddie.dogs$Total_Euth_Adj_D)
maddie.dogs$Total_Euth_Adj_D[is.na(maddie.dogs$Total_Euth_Adj_D)] <- 0
maddie.dogs$Died_Lost_D <- sapply(maddie.dogs$Died_Lost_D,chartonum)
maddie.dogs$Died_Lost_D <- as.numeric(maddie.dogs$Died_Lost_D)
maddie.dogs$Died_Lost_D[is.na(maddie.dogs$Died_Lost_D)] <- 0
maddie.dogs$Total_Outcomes_D <- sapply(maddie.dogs$Total_Outcomes_D,chartonum)
maddie.dogs$Total_Outcomes_D <- as.numeric(maddie.dogs$Total_Outcomes_D)
maddie.dogs$End_Count_D <- sapply(maddie.dogs$End_Count_D,chartonum)
maddie.dogs$End_Count_D <- as.numeric(maddie.dogs$End_Count_D)
#total intake by state
maddie.dogs$final_total_in <- maddie.dogs$Total_Intake_D + maddie.dogs$Transfer_In_Comm_Total_D
maddie.dogs$final_total_in[is.na(maddie.dogs$final_total_in)] <- 0
c_state <- unique(maddie.dogs$Community_State)
c_count <- rep(NA,29)
for (i in 1:length(c_state)) {
  c_count[i] <- sum(maddie.dogs$final_total_in[maddie.dogs$Community_State==c_state[i]])
}
library(ggplot2)
df_d <- data.frame(c_state, c_count)
ggplot(df_d,aes(x = c_state, y =  c_count)) + geom_bar(stat = "identity") + xlab("State") + ylab("count") + 
      ggtitle("Dog Intake by State")  +
      theme(axis.text.x=element_text(angle = 90,hjust = 1))
#Adopted, transfered, RTO, Died, Transfered across all states
outcomes <- c("Adopted", "Died", "RTO", "Transfer", "Euthanasia")
o_counts <- rep(NA,5)
o_counts[1] <- sum(maddie.dogs$Total_Adoptions_D)
o_counts[2] <- sum(maddie.dogs$Died_Lost_D)
o_counts[3] <- sum(maddie.dogs$RTO_D)
o_counts[4] <- sum(maddie.dogs$Transfer_Out_Comm_Total_D)
o_counts[5] <- sum(maddie.dogs$Total_Euth_Adj_D)
odf_d <- data.frame(outcomes,o_counts)
ggplot(odf_d,aes(x = outcomes, y =  o_counts)) + geom_bar(stat = "identity") + xlab("Outcomes") + ylab("count") + 
      ggtitle("Dog outcomes")  +
      theme(axis.text.x=element_text(angle = 90,hjust = 1))

#cats next
maddie.cats$Community_State <- as.factor(maddie.cats$Community_State)
maddie.cats$Total_Intake_C <- sapply(maddie.cats$Total_Intake_C,chartonum)
maddie.cats$Total_Intake_C <- as.numeric(maddie.cats$Total_Intake_C)
maddie.cats$Total_Adoptions_C <- sapply(maddie.cats$Total_Adoptions_C,chartonum)
maddie.cats$Total_Adoptions_C <- as.numeric(maddie.cats$Total_Adoptions_C)
maddie.cats$Total_Adoptions_C[is.na(maddie.cats$Total_Adoptions_C)] <- 0
maddie.cats$Transfer_In_Comm_Total_C <- sapply(maddie.cats$Transfer_In_Comm_Total_C,chartonum)
maddie.cats$Transfer_In_Comm_Total_C <- as.numeric(maddie.cats$Transfer_In_Comm_Total_C)
maddie.cats$Transfer_Out_Comm_Total_C <- sapply(maddie.cats$Transfer_Out_Comm_Total_C, chartonum)
maddie.cats$Transfer_Out_Comm_Total_C <- as.numeric(maddie.cats$Transfer_Out_Comm_Total_C)
maddie.cats$Transfer_Out_Comm_Total_C[is.na(maddie.cats$Transfer_Out_Comm_Total_C)] <- 0
maddie.cats$RTO_C <- sapply(maddie.cats$RTO_C,chartonum)
maddie.cats$RTO_C <- as.numeric(maddie.cats$RTO_C)
maddie.cats$RTO_C[is.na(maddie.cats$RTO_C)] <- 0
maddie.cats$Total_Euth_Adj_C <- sapply(maddie.cats$Total_Euth_Adj_C,chartonum)
maddie.cats$Total_Euth_Adj_C <- as.numeric(maddie.cats$Total_Euth_Adj_C)
maddie.cats$Total_Euth_Adj_C[is.na(maddie.cats$Total_Euth_Adj_C)] <- 0
maddie.cats$Died_Lost_C <- sapply(maddie.cats$Died_Lost_C,chartonum)
maddie.cats$Died_Lost_C <- as.numeric(maddie.cats$Died_Lost_C)
maddie.cats$Died_Lost_C[is.na(maddie.cats$Died_Lost_C)] <- 0
maddie.cats$Total_Outcomes_C <- sapply(maddie.cats$Total_Outcomes_C,chartonum)
maddie.cats$Total_Outcomes_C <- as.numeric(maddie.cats$Total_Outcomes_C)
maddie.cats$End_Count_C <- sapply(maddie.cats$End_Count_C,chartonum)
maddie.cats$End_Count_C <- as.numeric(maddie.cats$End_Count_C)
library(ggplot2)
#total intake by state
maddie.cats$final_total_in <- maddie.cats$Total_Intake_C + maddie.cats$Transfer_In_Comm_Total_C
maddie.cats$final_total_in[is.na(maddie.cats$final_total_in)] <- 0
cc_state <- unique(maddie.cats$Community_State)
cc_count <- rep(NA,29)
for (i in 1:length(cc_state)) {
  cc_count[i] <- sum(maddie.cats$final_total_in[maddie.cats$Community_State==cc_state[i]])
}
df_c <- data.frame(cc_state,cc_count)
ggplot(df_c,aes(x = cc_state, y =  cc_count)) + geom_bar(stat = "identity") + xlab("State") + ylab("count") + 
        ggtitle("Cat Intake by State")  +
        theme(axis.text.x=element_text(angle = 90,hjust = 1))
#Adopted, transfered, RTO, Died, Transfered across all states
c_outcomes <- c("Adopted", "Died", "RTO", "Transfer", "Euthanasia")
oc_counts <- rep(NA,5)
oc_counts[1] <- sum(maddie.cats$Total_Adoptions_C)
oc_counts[2] <- sum(maddie.cats$Died_Lost_C)
oc_counts[3] <- sum(maddie.cats$RTO_C)
oc_counts[4] <- sum(maddie.cats$Transfer_Out_Comm_Total_C)
oc_counts[5] <- sum(maddie.cats$Total_Euth_Adj_C)
odf_c <- data.frame(c_outcomes, oc_counts)
ggplot(odf_d,aes(x = c_outcomes, y =  oc_counts)) + geom_bar(stat = "identity") + xlab("Outcomes") + ylab("count") + 
      ggtitle("Cat outcomes")  +
       theme(axis.text.x=element_text(angle = 90,hjust = 1))