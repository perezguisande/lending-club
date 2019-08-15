library(data.table)
data2 <- fread("Loan-knn-222-50k.csv")
library(dplyr)

data2 %>% group_by(loan_status) %>% summarise(no_rows = length(loan_status))

#Converting variables  
require(ggplot2)


#Good vs Bad loans
counts <- table(data2$loan_status)
barplot(counts, main="Loan_status", 
        xlab="Total count")
#Converting into 2 categories
data2<-data2[!(data2$loan_status=="Current"),]
data2<-data2[!(data2$loan_status=="In Grace Period"),]
data2<-data2[!(data2$loan_status=="Late (16-30 days)"),]
data2<-data2[!(data2$loan_status=="Late (31-120 days)"),]


data2$loan_status[data2$loan_status == "Charged Off"] <- 1  #Charged of means more than 180 days past due or unlikely to collect
data2$loan_status[data2$loan_status == "Default"] <- 1
data2$loan_status[data2$loan_status == "Fully Paid"] <- 0


#Into 3
#data2$loan_status[data2$loan_status == "Charged Off"] <- "Bad Loan"
#data2$loan_status[data2$loan_status == "Default"] <- "Bad Loan"
#data2$loan_status[data2$loan_status == "In Grace Period"] <- "Bad Loan"
#.data2$loan_status[data2$loan_status == "Late (16-30 days)"] <- "Bad Loan"
#data2$loan_status[data2$loan_status == "Late (31-120 days)"] <- "Bad Loan"

counts <- table(data2$loan_status)
barplot(counts, main="Loan_status", 
        xlab="Total count")

#Term

counts <- table(data2$term)
barplot(counts, main="term", 
        xlab="Total")   

#Converting into numerical
data2$term[data2$term == "36 months"] <- 36
data2$term[data2$term == "60 months"] <- 60

#converting into factor
data2$term <- factor(data2$term)
data2$grade <- factor(data2$grade)
data2$sub_grade <- factor(data2$sub_grade)
data2$emp_title <- factor(data2$emp_title)
data2$emp_length <- factor(data2$emp_length)
data2$home_ownership <- factor(data2$home_ownership)
data2$verification_status <- factor(data2$verification_status)
data2$loan_status <- factor(data2$loan_status)
data2$purpose <- factor(data2$purpose)
data2$title <- factor(data2$title)
data2$addr_state <- factor(data2$addr_state)
data2$initial_list_status <- factor(data2$initial_list_status)
data2$application_type <- factor(data2$application_type)
data2$disbursement_method<- factor(data2$disbursement_method)
data2$debt_settlement_flag<- factor(data2$debt_settlement_flag)
data2$issue_d<- factor(data2$issue_d)

#Dropping non-necessary features

drops <- c("funded_amnt", "funded_amnt_inv", "title", "zip_code", "pymnt_plan", "earliest_cr_line","recoveries", "out_prncp", "out_prncp_inv","V1", "total_pymnt", "total_pymnt_inv", "total_rec_prncp", "total_rec_int", "total_rec_late_fee", "collection_recovery_fee", "last_pymnt_amnt", "last_pymnt_d", "next_pymnt_d", "last_credit_pull_d", "collections_12_mths_ex_med", "debt_settlement_flag")

data2 <- select(data2, -drops)


#Creating a variable based on purpose
data2 %>% group_by(purpose) %>% summarise(no_rows = length(purpose))
data2$purpose_debt <- NA

data2$purpose_debt[data2$purpose == "car"] <- 0 
data2$purpose_debt[data2$purpose == "credit_card"] <- 1
data2$purpose_debt[data2$purpose == "debt_consolidation"] <- 1
data2$purpose_debt[data2$purpose == "home_improvement"] <- 0
data2$purpose_debt[data2$purpose == "house"] <- 0
data2$purpose_debt[data2$purpose == "major_purchase"] <- 0
data2$purpose_debt[data2$purpose == "medical"] <- 0
data2$purpose_debt[data2$purpose == "moving"] <- 0
data2$purpose_debt[data2$purpose == "other"] <- 0
data2$purpose_debt[data2$purpose == "renewable_energy"] <- 0
data2$purpose_debt[data2$purpose == "small_business"] <- 0
data2$purpose_debt[data2$purpose == "vacation"] <- 0
data2$purpose_debt[data2$purpose == "wedding"] <- 0

counts <- table(data2$purpose_debt)
barplot(counts, main="Borrowing for repaying other debt?", 
        xlab="Total count")

#Asked money over income

data2$relative_ammount <- ifelse(data2$annual_inc == 0, 0, (data2$loan_amnt)/(data2$annual_inc)) 

p <- ggplot(data2, aes(x=relative_ammount)) +geom_density(alpha=0.15) +xlim(0, 1.2)
p

#employment type
data2 %>% group_by(emp_title) %>% summarise(no_rows = length(emp_title))
#Only 1 observation for each employment title. Dropping column, much work to make it worthy
drops <- c("emp_title")
data2 <- select(data2, -drops)
#Modifying employment time
data2 %>% group_by(emp_length) %>% summarise(no_rows = length(emp_length))

data2$emp_length <- sapply(data2$emp_length, as.character) 
data2$emp_length[data2$emp_length == "n/a"] <- "Unemployed" 
data2$emp_length[data2$emp_length == "< 1 year"] <- "0-2"
data2$emp_length[data2$emp_length == "1 year"] <- "0-2"
data2$emp_length[data2$emp_length == "2 years"] <- "0-2"
data2$emp_length[data2$emp_length == "3 years"] <- "3-5"
data2$emp_length[data2$emp_length == "4 years"] <- "3-5"
data2$emp_length[data2$emp_length == "5 years"] <- "3-5"
data2$emp_length[data2$emp_length == "6 years"] <- "6-8"
data2$emp_length[data2$emp_length == "7 years"] <- "6-8"
data2$emp_length[data2$emp_length == "8 years"] <- "6-8"
data2$emp_length[data2$emp_length == "9 years"] <- "9+"
data2$emp_length[data2$emp_length == "10+ years"] <- "9+"
data2 %>% group_by(emp_length) %>% summarise(no_rows = length(emp_length))
data2$emp_length <- factor(data2$emp_length)


data2 %>% group_by(home_ownership) %>% summarise(no_rows = length(home_ownership))

#Interest reate into context
data2$issue_d <- as.Date(gsub("^", "01-", data2$issue_d), format="%d-%b-%Y")
libor <- read.csv("USD12M.csv") #Retrieved from FRED. Monthly average
libor$DATE <- as.Date(libor$DATE)

colnames(data2)[which(names(data2) == "issue_d")] <- "DATE"
data2 <- data2 %>% left_join(libor, by = "DATE")



data2$adjusted_rate <- data2$int_rate - data2$USD12MD156N

library(ggplot2)
status <- ggplot(data = data2, aes(loan_status), xlab = "Loan Status")
status + geom_bar() + xlab("Loan Status")

interest <- ggplot(data = data2, aes(x = adjusted_rate, color = loan_status))
interest + geom_histogram()

plot <- ggplot(data = data2, aes(x=grade, y=adjusted_rate)) + geom_boxplot(aes(fill=loan_status))
plot

plot <- ggplot(data = data2, aes(x=grade, y = loan_amnt)) + geom_boxplot(aes(fill=loan_status))
plot

plot <- ggplot(data = data2, aes(x=adjusted_rate, color = loan_status)) + geom_density(linetype=loan_status) + xlab("Interest Rate")
plot

p4 <- ggplot() + geom_bar(aes(y = "count", x = grade, fill = loan_status), data = data2,
                          stat="identity")
p4 + ylab("count")

#Scaling data before feature selection methods
drops <- c("DATE", "USD12MD156N", "int_rate")
data2 <- select(data2, -drops)
data2 <- as.data.frame(data2)
colnames(data2)

index_num <- sapply(data2, is.numeric)
index_int <- sapply(data2, is.integer)

data2[index_num] <- lapply(data2[index_num], scale)
data2[index_int] <- lapply(data2[index_int], scale)


#saving data
#write.csv(data2, file = "data_previous_var_selec-50k.csv")


#Variable Selection
drops <- c("revol_bal", "policy_code", "acc_now_delinq","avg_cur_bal","delinq_amnt","num_tl_120dpd_2m","purpose_debt","total_bal_ex_mort","application_type",
           "total_il_high_credit_limit",
           "application_type",
           "bc_util",
           "bc_open_to_buy",
           "mo_sin_old_il_acct",
           "total_bc_limit",
           "grade", "num_tl_30dpd", "mths_since_recent_bc"
)
data2 <- select(data2, -drops)



#Saving without one-hot encoding
#write.csv(data2, file = "prepared_data_nocurrent-15k-no-encoded-adrate.csv")



#One-hot encoding
library(caret)
dmy <- dummyVars(" ~ .", data = data2)
data2 <- data.frame(predict(dmy, newdata = data2))
drops <- c("loan_status.0")
data2 <- data2[ , !(names(data2) %in% drops)]

colnames(data2)[which(names(data2) == "loan_status.1")] <- "default"

data2$default <- factor(data2$default)


#Saving csv
write.csv(data2, file = "prepared_data_nocurrent-50k-adrate-2.csv")

