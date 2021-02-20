library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(caret)
library(lubridate)
library(zoo)
data <- read_csv("loanf.csv") #load data

# filter out unknown outcomes
data <- data%>%mutate(loan_outcome = ifelse(loan_status %in% c('Charged Off' , 'Default'), 1, ifelse(loan_status == 'Fully Paid' , 0 , 'No info')))%>%filter(loan_outcome != 'No info')
data$loan_outcome <- as.factor(data$loan_outcome)    
data.new2 <- data

# delete columns with more than 95% missing data
data <- data[, colSums(is.na(data)) < 581212]

# split data 70/30
set.seed(3456)
trainIndex <- createDataPartition(data$loan_outcome, p = .7,list = FALSE,times = 1)
Train <- data[ trainIndex,]
Valid <- data[-trainIndex,] # prep Train first, then valid

# removing useless variables useless 
Train <- Train%>%select(-c(last_pymnt_d, emp_title, url, earliest_cr_line, last_credit_pull_d, id, loan_status))

# convert ch to fctr
Train$title <- as.factor(Train$title)
Train$purpose <- as.factor(Train$purpose)
Train$term <- as.factor(Train$term)
Train$grade <- as.factor(Train$grade)
Train$sub_grade <- as.factor(Train$sub_grade)
Train$emp_length <- as.factor(Train$emp_length)
Train$home_ownership <- as.factor(Train$home_ownership)
Train$verification_status <- as.factor(Train$verification_status)
Train$addr_state <- as.factor(Train$addr_state)
Train$initial_list_status <- as.factor(Train$initial_list_status)
Train$debt_settlement_flag <- as.factor(Train$debt_settlement_flag)
Train$application_type <- as.factor(Train$application_type)
Train$pymnt_plan <- as.factor(Train$pymnt_plan)
Train$zip_code <- as.factor(Train$zip_code)

# imputation
Train.pp$disbursement_method <- as.factor(Train.pp$disbursement_method)
Train.pp$hardship_flag <- as.factor(Train.pp$hardship_flag)


# impute with 0
Train.pp$open_il_12m[is.na(Train.pp$open_il_12m)] <- 0
Train.pp$total_bal_il[is.na(Train.pp$total_bal_il)] <- 0
Train.pp$open_rv_24m[is.na(Train.pp$open_rv_24m)] <- 0
Train.pp$inq_last_12m[is.na(Train.pp$inq_last_12m)] <- 0
Train.pp$open_act_il[is.na(Train.pp$open_act_il)] <- 0
Train.pp$open_rv_12m[is.na(Train.pp$open_rv_12m)] <- 0
Train.pp$all_util[is.na(Train.pp$all_util)] <- 0
Train.pp$total_cu_tl[is.na(Train.pp$total_cu_tl)] <- 0
Train.pp$avg_cur_bal[is.na(Train.pp$avg_cur_bal)] <- 0
Train.pp$num_rev_accts[is.na(Train.pp$num_rev_accts)] <- 0
Train.pp$inq_fi[is.na(Train.pp$inq_fi)] <- 0
Train.pp$max_bal_bc[is.na(Train.pp$max_bal_bc)] <- 0
Train.pp$il_util[is.na(Train.pp$il_util)] <- 0
Train.pp$open_il_24m[is.na(Train.pp$open_il_24m)] <- 0
Train.pp$open_acc_6m[is.na(Train.pp$open_acc_6m)] <- 0
Train.pp$num_tl_120dpd_2m[is.na(Train.pp$num_tl_120dpd_2m)] <- 0
Train.pp$bc_util[is.na(Train.pp$bc_util)] <- 0

# impute with mean/median
Train.pp$dti[is.na(Train.pp$dti)] <- mean(Train.pp$dti, na.rm=T)
Train.pp$percent_bc_gt_75[is.na(Train.pp$percent_bc_gt_75)] <- mean(Train.pp$percent_bc_gt_75, na.rm=T)
Train.pp$revol_util[is.na(Train.pp$revol_util)] <- median(Train.pp$revol_util, na.rm=T)
Train.pp$bc_open_to_buy[is.na(Train.pp$bc_open_to_buy)] <- median(Train.pp$bc_open_to_buy, na.rm=T)

# impute with max value
Train.pp$mths_since_last_major_derog[is.na(Train.pp$mths_since_last_major_derog)] <- max(Train.pp$mths_since_last_major_derog, na.rm=T)
Train.pp$mths_since_last_record[is.na(Train.pp$mths_since_last_record)] <- max(Train.pp$mths_since_last_record, na.rm=T)
Train.pp$mths_since_recent_revol_delinq[is.na(Train.pp$mths_since_recent_revol_delinq)] <- max(Train.pp$mths_since_recent_revol_delinq, na.rm=T)
Train.pp$pct_tl_nvr_dlq[is.na(Train.pp$pct_tl_nvr_dlq)] <- max(Train.pp$pct_tl_nvr_dlq, na.rm=T)
Train.pp$mths_since_last_delinq[is.na(Train.pp$mths_since_last_delinq)] <- max(Train.pp$mths_since_last_delinq, na.rm=T)
Train.pp$mths_since_rcnt_il[is.na(Train.pp$mths_since_rcnt_il)] <- max(Train.pp$mths_since_rcnt_il, na.rm=T)
Train.pp$mths_since_recent_inq[is.na(Train.pp$mths_since_recent_inq)] <- max(Train.pp$mths_since_recent_inq, na.rm=T)
Train.pp$mths_since_recent_bc_dlq[is.na(Train.pp$mths_since_recent_bc_dlq)] <- max(Train.pp$mths_since_recent_bc_dlq, na.rm=T)
Train.pp$mo_sin_old_il_acct[is.na(Train.pp$mo_sin_old_il_acct)] <- max(Train.pp$mo_sin_old_il_acct, na.rm=T)
Train.pp$mths_since_recent_bc[is.na(Train.pp$mths_since_recent_bc)] <- max(Train.pp$mths_since_recent_bc, na.rm=T)


# imputing "Other"
Train.pp$title[is.na(Train.pp$title)] <- "Other"
Train.pp$emp_length <- as.character(Train.pp$emp_length)
Train.pp$emp_length[is.na(Train.pp$emp_length)] <- "Other"
Train.pp$emp_length <- as.factor(Train.pp$emp_length)

# remove hardship_flag bc it only has one level, "N" and pymnt_plan which only has "n"
Train.pp <- Train.pp%>%select(-c(hardship_flag, pymnt_plan))

# convert emp_length to numeric 
levels(Train.pp$emp_length) <- c(0, 1, 11, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# leave sub_grade in original form for later anaalysis
Train.pp <- Train.pp%>%mutate(sub_grade_pp = sub_grade)
levels(Train.pp$sub_grade_pp) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35)
Train.pp$sub_grade_pp<- as.numeric(Train.pp$sub_grade_pp)
Train.pp$emp_length <- as.numeric(Train.pp$emp_length)

# add potential transformation variables 
Train.pp <- Train.pp%>%mutate(inst_to_inc = installment/annual_inc, open_acc_ratio = open_acc/total_acc)

# convert categorical to dummy variables
# save dataset
Train.analysis <- Train.pp

Train.toadd <- Train.pp%>%select(c(issue_d, zip_code))
Train.pp <- fastDummies::dummy_cols(Train.pp%>%select(-c(issue_d, zip_code)))
Train.pp <- Train.pp%>%select(-c(grade, sub_grade))


# get rid of all the categorical variables in the dataset. info now exists in the dummy variables 
Train.cat <- Train.pp # save categorical vars in a dataset
for(i in 1:length(colnames(Train.pp))){
  if(class(Train.pp[,i]) == "factor") {
    Train.pp <- Train.pp[,-i] }
}
Train.pp <- dplyr::select(Train.pp, -c(purpose))
# now convert issue_d to a proper date variable
Train.pp <- cbind(Train.pp, Train.toadd)
Train.pp$issue_d <- paste(Train.pp$issue_d, "-01", sep = "")
Train.pp$issue_d <- str_replace(Train.pp$issue_d, "Dec", "12")
Train.pp$issue_d <- str_replace(Train.pp$issue_d, "Jan", "01")
Train.pp$issue_d <- str_replace(Train.pp$issue_d, "Feb", "02")
Train.pp$issue_d <- str_replace(Train.pp$issue_d, "Mar", "03")
Train.pp$issue_d <- str_replace(Train.pp$issue_d, "Apr", "04")
Train.pp$issue_d <- str_replace(Train.pp$issue_d, "May", "05")
Train.pp$issue_d <- str_replace(Train.pp$issue_d, "Jun", "06")
Train.pp$issue_d <- str_replace(Train.pp$issue_d, "Jul", "07")
Train.pp$issue_d <- str_replace(Train.pp$issue_d, "Aug", "08")
Train.pp$issue_d <- str_replace(Train.pp$issue_d, "Sep", "09")
Train.pp$issue_d <- str_replace(Train.pp$issue_d, "Oct", "10")
Train.pp$issue_d <- str_replace(Train.pp$issue_d, "Nov", "11")
Train.pp$issue_d <- myd(Train.pp$issue_d)


# feature scaling
Train.noscale <- Train.pp
Train.pp$loan_amnt <- as.vector(scale(Train.pp$loan_amnt))
Train.pp <- dplyr::select(Train.pp, -c(funded_amnt, funded_amnt_inv)) #collinear removal
Train.pp$int_rate <- as.vector(scale(Train.pp$int_rate))
Train.pp$installment <- as.vector(scale(Train.pp$installment))
Train.pp$sub_grade <- as.vector(scale(Train.pp$sub_grade))
Train.pp$emp_length <- as.vector(scale(Train.pp$emp_length))
Train.pp$annual_inc <- as.vector(scale(Train.pp$annual_inc))
Train.pp$dti <- as.vector(scale(Train.pp$dti))
Train.pp$delinq_2yrs <- as.vector(scale(Train.pp$delinq_2yrs))
Train.pp$fico_range_high <- as.vector(scale(Train.pp$fico_range_high))
Train.pp$fico_range_low <- as.vector(scale(Train.pp$fico_range_low))
Train.pp$inq_last_6mths <- as.vector(scale(Train.pp$inq_last_6mths))
Train.pp$mths_since_last_delinq <- as.vector(scale(Train.pp$mths_since_last_delinq))
Train.pp$mths_since_last_record <- as.vector(scale(Train.pp$mths_since_last_record))
Train.pp$open_acc <- as.vector(scale(Train.pp$open_acc))
Train.pp$pub_rec <- as.vector(scale(Train.pp$pub_rec))
Train.pp$revol_bal <- as.vector(scale(Train.pp$revol_bal))
Train.pp$revol_util <- as.vector(scale(Train.pp$revol_util))
Train.pp$total_acc <- as.vector(scale(Train.pp$total_acc))
Train.pp$out_prncp <- as.vector(scale(Train.pp$out_prncp))
Train.pp$out_prncp_inv <- as.vector(scale(Train.pp$out_prncp_inv))
Train.pp <- dplyr::select(Train.pp, -c(total_pymnt, total_pymnt_inv)) #future var removal
Train.pp$total_rec_prncp <- as.vector(scale(Train.pp$total_rec_prncp))
Train.pp$total_rec_int <- as.vector(scale(Train.pp$total_rec_int))
Train.pp$total_rec_late_fee <- as.vector(scale(Train.pp$total_rec_late_fee))
Train.pp$recoveries <- as.vector(scale(Train.pp$recoveries))
Train.pp$collection_recovery_fee <- as.vector(scale(Train.pp$collection_recovery_fee))
Train.pp <- dplyr::select(Train.pp, -c(last_pymnt_amnt)) #future var removal
Train.pp$last_fico_range_high <- as.vector(scale(Train.pp$last_fico_range_high))
Train.pp$last_fico_range_low <- as.vector(scale(Train.pp$last_fico_range_low))
Train.pp$collections_12_mths_ex_med <- as.vector(scale(Train.pp$collections_12_mths_ex_med))
Train.pp$mths_since_last_major_derog <- as.vector(scale(Train.pp$mths_since_last_major_derog))
Train.pp <- dplyr::select(Train.pp, -c(policy_code)) #var removal bc only one class
Train.pp$acc_now_delinq <- as.vector(scale(Train.pp$acc_now_delinq))
Train.pp$tot_coll_amt <- as.vector(scale(Train.pp$tot_coll_amt))
Train.pp$tot_cur_bal <- as.vector(scale(Train.pp$tot_cur_bal))
Train.pp$open_acc_6m <- as.vector(scale(Train.pp$open_acc_6m))
Train.pp$open_act_il <- as.vector(scale(Train.pp$open_act_il))
Train.pp$open_il_12m <- as.vector(scale(Train.pp$open_il_12m))
Train.pp$open_il_24m <- as.vector(scale(Train.pp$open_il_24m))
Train.pp$mths_since_rcnt_il <- as.vector(scale(Train.pp$mths_since_rcnt_il))
Train.pp$total_bal_il <- as.vector(scale(Train.pp$total_bal_il))
Train.pp$il_util <- as.vector(scale(Train.pp$il_util))
Train.pp$open_rv_12m <- as.vector(scale(Train.pp$open_rv_12m))
Train.pp$open_rv_24m <- as.vector(scale(Train.pp$open_rv_24m))
Train.pp$max_bal_bc <- as.vector(scale(Train.pp$max_bal_bc))
Train.pp$all_util <- as.vector(scale(Train.pp$all_util))
Train.pp$total_rev_hi_lim <- as.vector(scale(Train.pp$total_rev_hi_lim))
Train.pp$inq_fi <- as.vector(scale(Train.pp$inq_fi))
Train.pp$total_cu_tl <- as.vector(scale(Train.pp$total_cu_tl))
Train.pp$inq_last_12m <- as.vector(scale(Train.pp$inq_last_12m))
Train.pp$acc_open_past_24mths <- as.vector(scale(Train.pp$acc_open_past_24mths))
Train.pp$avg_cur_bal <- as.vector(scale(Train.pp$avg_cur_bal))
Train.pp$bc_open_to_buy <- as.vector(scale(Train.pp$bc_open_to_buy))
Train.pp$bc_util <- as.vector(scale(Train.pp$bc_util))
Train.pp$chargeoff_within_12_mths <- as.vector(scale(Train.pp$chargeoff_within_12_mths))
Train.pp$delinq_amnt <- as.vector(scale(Train.pp$delinq_amnt))
Train.pp$mo_sin_old_il_acct <- as.vector(scale(Train.pp$mo_sin_old_il_acct))
Train.pp$mo_sin_old_rev_tl_op <- as.vector(scale(Train.pp$mo_sin_old_rev_tl_op))
Train.pp$mo_sin_rcnt_rev_tl_op <- as.vector(scale(Train.pp$mo_sin_rcnt_rev_tl_op))
Train.pp$mo_sin_rcnt_tl <- as.vector(scale(Train.pp$mo_sin_rcnt_tl))
Train.pp$mort_acc <- as.vector(scale(Train.pp$mort_acc))
Train.pp$mths_since_recent_bc <- as.vector(scale(Train.pp$mths_since_recent_bc))
Train.pp$mths_since_recent_bc_dlq <- as.vector(scale(Train.pp$mths_since_recent_bc_dlq))
Train.pp$mths_since_recent_inq <- as.vector(scale(Train.pp$mths_since_recent_inq))
Train.pp$mths_since_recent_revol_delinq <- as.vector(scale(Train.pp$mths_since_recent_revol_delinq))
Train.pp$num_accts_ever_120_pd <- as.vector(scale(Train.pp$num_accts_ever_120_pd))
Train.pp$num_actv_bc_tl <- as.vector(scale(Train.pp$num_actv_bc_tl))
Train.pp$num_actv_rev_tl <- as.vector(scale(Train.pp$num_actv_rev_tl))
Train.pp$num_bc_sats <- as.vector(scale(Train.pp$num_bc_sats))
Train.pp$num_bc_tl <- as.vector(scale(Train.pp$num_bc_tl))
Train.pp$num_il_tl <- as.vector(scale(Train.pp$num_il_tl))
Train.pp$num_op_rev_tl <- as.vector(scale(Train.pp$num_op_rev_tl))
Train.pp$num_rev_accts <- as.vector(scale(Train.pp$num_rev_accts))
Train.pp$num_rev_tl_bal_gt_0 <- as.vector(scale(Train.pp$num_rev_tl_bal_gt_0))
Train.pp$num_bc_sats <- as.vector(scale(Train.pp$num_bc_sats))
Train.pp$num_tl_120dpd_2m <- as.vector(scale(Train.pp$num_tl_120dpd_2m))
Train.pp$num_tl_30dpd <- as.vector(scale(Train.pp$num_tl_30dpd))
Train.pp$num_tl_90g_dpd_24m <- as.vector(scale(Train.pp$num_tl_90g_dpd_24m))
Train.pp$num_tl_op_past_12m <- as.vector(scale(Train.pp$num_tl_op_past_12m))
Train.pp$pct_tl_nvr_dlq <- as.vector(scale(Train.pp$pct_tl_nvr_dlq))
Train.pp$percent_bc_gt_75 <- as.vector(scale(Train.pp$percent_bc_gt_75))
Train.pp$pub_rec_bankruptcies <- as.vector(scale(Train.pp$pub_rec_bankruptcies))
Train.pp$tax_liens <- as.vector(scale(Train.pp$tax_liens))
Train.pp$tot_hi_cred_lim <- as.vector(scale(Train.pp$tot_hi_cred_lim))
Train.pp$total_bal_ex_mort <- as.vector(scale(Train.pp$total_bal_ex_mort))
Train.pp$total_bc_limit <- as.vector(scale(Train.pp$total_bc_limit))
Train.pp$total_il_high_credit_limit <- as.vector(scale(Train.pp$total_il_high_credit_limit))
Train.pp <- dplyr::select(Train.pp, -c(debt_settlement_flag)) #factor removal
Train.pp$inst_to_inc <- as.vector(scale(Train.pp$inst_to_inc))
Train.pp$open_acc_ratio <- as.vector(scale(Train.pp$open_acc_ratio))

# Now we preprocess Valid 

# removing useless variables useless 
Valid <- Valid%>%select(-c(last_pymnt_d, emp_title, url, earliest_cr_line, last_credit_pull_d, id, loan_status))

# convert ch to fctr
Valid$title <- as.factor(Valid$title)
Valid$purpose <- as.factor(Valid$purpose)
Valid$term <- as.factor(Valid$term)
Valid$grade <- as.factor(Valid$grade)
Valid$sub_grade <- as.factor(Valid$sub_grade)
Valid$emp_length <- as.factor(Valid$emp_length)
Valid$home_ownership <- as.factor(Valid$home_ownership)
Valid$verification_status <- as.factor(Valid$verification_status)
Valid$addr_state <- as.factor(Valid$addr_state)
Valid$initial_list_status <- as.factor(Valid$initial_list_status)
Valid$debt_settlement_flag <- as.factor(Valid$debt_settlement_flag)
Valid$application_type <- as.factor(Valid$application_type)
Valid$pymnt_plan <- as.factor(Valid$pymnt_plan)
Valid$zip_code <- as.factor(Valid$zip_code)


# imputation
Valid.pp$disbursement_method <- as.factor(Valid.pp$disbursement_method)
Valid.pp$hardship_flag <- as.factor(Valid.pp$hardship_flag)


# impute with 0
Valid.pp$open_il_12m[is.na(Valid.pp$open_il_12m)] <- 0
Valid.pp$total_bal_il[is.na(Valid.pp$total_bal_il)] <- 0
Valid.pp$open_rv_24m[is.na(Valid.pp$open_rv_24m)] <- 0
Valid.pp$inq_last_12m[is.na(Valid.pp$inq_last_12m)] <- 0
Valid.pp$open_act_il[is.na(Valid.pp$open_act_il)] <- 0
Valid.pp$open_rv_12m[is.na(Valid.pp$open_rv_12m)] <- 0
Valid.pp$all_util[is.na(Valid.pp$all_util)] <- 0
Valid.pp$total_cu_tl[is.na(Valid.pp$total_cu_tl)] <- 0
Valid.pp$avg_cur_bal[is.na(Valid.pp$avg_cur_bal)] <- 0
Valid.pp$num_rev_accts[is.na(Valid.pp$num_rev_accts)] <- 0
Valid.pp$inq_fi[is.na(Valid.pp$inq_fi)] <- 0
Valid.pp$max_bal_bc[is.na(Valid.pp$max_bal_bc)] <- 0
Valid.pp$il_util[is.na(Valid.pp$il_util)] <- 0
Valid.pp$open_il_24m[is.na(Valid.pp$open_il_24m)] <- 0
Valid.pp$open_acc_6m[is.na(Valid.pp$open_acc_6m)] <- 0
Valid.pp$num_tl_120dpd_2m[is.na(Valid.pp$num_tl_120dpd_2m)] <- 0
Valid.pp$bc_util[is.na(Valid.pp$bc_util)] <- 0

# impute with mean/median
Valid.pp$dti[is.na(Valid.pp$dti)] <- mean(Valid.pp$dti, na.rm=T)
Valid.pp$percent_bc_gt_75[is.na(Valid.pp$percent_bc_gt_75)] <- mean(Valid.pp$percent_bc_gt_75, na.rm=T)
Valid.pp$revol_util[is.na(Valid.pp$revol_util)] <- median(Valid.pp$revol_util, na.rm=T)
Valid.pp$bc_open_to_buy[is.na(Valid.pp$bc_open_to_buy)] <- median(Valid.pp$bc_open_to_buy, na.rm=T)

# impute with max value
Valid.pp$mths_since_last_major_derog[is.na(Valid.pp$mths_since_last_major_derog)] <- max(Valid.pp$mths_since_last_major_derog, na.rm=T)
Valid.pp$mths_since_last_record[is.na(Valid.pp$mths_since_last_record)] <- max(Valid.pp$mths_since_last_record, na.rm=T)
Valid.pp$mths_since_recent_revol_delinq[is.na(Valid.pp$mths_since_recent_revol_delinq)] <- max(Valid.pp$mths_since_recent_revol_delinq, na.rm=T)
Valid.pp$pct_tl_nvr_dlq[is.na(Valid.pp$pct_tl_nvr_dlq)] <- max(Valid.pp$pct_tl_nvr_dlq, na.rm=T)
Valid.pp$mths_since_last_delinq[is.na(Valid.pp$mths_since_last_delinq)] <- max(Valid.pp$mths_since_last_delinq, na.rm=T)
Valid.pp$mths_since_rcnt_il[is.na(Valid.pp$mths_since_rcnt_il)] <- max(Valid.pp$mths_since_rcnt_il, na.rm=T)
Valid.pp$mths_since_recent_inq[is.na(Valid.pp$mths_since_recent_inq)] <- max(Valid.pp$mths_since_recent_inq, na.rm=T)
Valid.pp$mths_since_recent_bc_dlq[is.na(Valid.pp$mths_since_recent_bc_dlq)] <- max(Valid.pp$mths_since_recent_bc_dlq, na.rm=T)
Valid.pp$mo_sin_old_il_acct[is.na(Valid.pp$mo_sin_old_il_acct)] <- max(Valid.pp$mo_sin_old_il_acct, na.rm=T)
Valid.pp$mths_since_recent_bc[is.na(Valid.pp$mths_since_recent_bc)] <- max(Valid.pp$mths_since_recent_bc, na.rm=T)


# imputing "Other"
Valid.pp$title[is.na(Valid.pp$title)] <- "Other"
Valid.pp$emp_length <- as.character(Valid.pp$emp_length)
Valid.pp$emp_length[is.na(Valid.pp$emp_length)] <- "Other"
Valid.pp$emp_length <- as.factor(Valid.pp$emp_length)

# remove hardship_flag bc it only has one level, "N" and pymnt_plan which only has "n"
Valid.pp <- Valid.pp%>%select(-c(hardship_flag, pymnt_plan))

# convert emp_length to numeric 
levels(Valid.pp$emp_length) <- c(0, 1, 11, 2, 3, 4, 5, 6, 7, 8, 9, 10)
# leave sub_grade in original form for later anaalysis
Valid.pp <- Valid.pp%>%mutate(sub_grade_pp = sub_grade)
levels(Valid.pp$sub_grade_pp) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35)
Valid.pp$sub_grade_pp<- as.numeric(Valid.pp$sub_grade_pp)
Valid.pp$emp_length <- as.numeric(Valid.pp$emp_length)


# add transformation variables 
Valid.pp <- Valid.pp%>%mutate(inst_to_inc = installment/annual_inc, open_acc_ratio = open_acc/total_acc)

# convert categorical to dummy variables
# save dataset
Valid.analysis <- Valid.pp
Valid.toadd <- Valid.pp%>%select(c(issue_d, zip_code))
Valid.pp <- fastDummies::dummy_cols(Valid.pp%>%select(-c(issue_d, zip_code)))
Valid.pp <- Valid.pp%>%select(-c(grade, sub_grade))



# get rid of all the categorical variables in the dataset. info now exists in the dummy variables 
Valid.cat <- Valid.pp # save categorical vars in a dataset
for(i in 1:length(colnames(Valid.pp))){
  if(class(Valid.pp[,i]) == "factor") {
    Valid.pp <- Valid.pp[,-i] }
}
Valid.pp <- dplyr::select(Valid.pp, -c(purpose))
Valid.pp <- dplyr::select(Valid.pp, -c(purpose))

# now convert issue_d to a proper date variable
Valid.pp <- cbind(Valid.pp, Valid.toadd)
Valid.pp$issue_d <- paste(Valid.pp$issue_d, "-01", sep = "")
Valid.pp$issue_d <- str_replace(Valid.pp$issue_d, "Dec", "12")
Valid.pp$issue_d <- str_replace(Valid.pp$issue_d, "Jan", "01")
Valid.pp$issue_d <- str_replace(Valid.pp$issue_d, "Feb", "02")
Valid.pp$issue_d <- str_replace(Valid.pp$issue_d, "Mar", "03")
Valid.pp$issue_d <- str_replace(Valid.pp$issue_d, "Apr", "04")
Valid.pp$issue_d <- str_replace(Valid.pp$issue_d, "May", "05")
Valid.pp$issue_d <- str_replace(Valid.pp$issue_d, "Jun", "06")
Valid.pp$issue_d <- str_replace(Valid.pp$issue_d, "Jul", "07")
Valid.pp$issue_d <- str_replace(Valid.pp$issue_d, "Aug", "08")
Valid.pp$issue_d <- str_replace(Valid.pp$issue_d, "Sep", "09")
Valid.pp$issue_d <- str_replace(Valid.pp$issue_d, "Oct", "10")
Valid.pp$issue_d <- str_replace(Valid.pp$issue_d, "Nov", "11")
Valid.pp$issue_d <- myd(Valid.pp$issue_d)

# feature scaling
Valid.noscale <- Valid.pp
Valid.pp$loan_amnt <- as.vector(scale(Valid.pp$loan_amnt))
Valid.pp <- dplyr::select(Valid.pp, -c(funded_amnt, funded_amnt_inv)) #collinear removal
Valid.pp$int_rate <- as.vector(scale(Valid.pp$int_rate))
Valid.pp$installment <- as.vector(scale(Valid.pp$installment))
Valid.pp$sub_grade <- as.vector(scale(Valid.pp$sub_grade))
Valid.pp$emp_length <- as.vector(scale(Valid.pp$emp_length))
Valid.pp$annual_inc <- as.vector(scale(Valid.pp$annual_inc))
Valid.pp$dti <- as.vector(scale(Valid.pp$dti))
Valid.pp$delinq_2yrs <- as.vector(scale(Valid.pp$delinq_2yrs))
Valid.pp$fico_range_high <- as.vector(scale(Valid.pp$fico_range_high))
Valid.pp$fico_range_low <- as.vector(scale(Valid.pp$fico_range_low))
Valid.pp$inq_last_6mths <- as.vector(scale(Valid.pp$inq_last_6mths))
Valid.pp$mths_since_last_delinq <- as.vector(scale(Valid.pp$mths_since_last_delinq))
Valid.pp$mths_since_last_record <- as.vector(scale(Valid.pp$mths_since_last_record))
Valid.pp$open_acc <- as.vector(scale(Valid.pp$open_acc))
Valid.pp$pub_rec <- as.vector(scale(Valid.pp$pub_rec))
Valid.pp$revol_bal <- as.vector(scale(Valid.pp$revol_bal))
Valid.pp$revol_util <- as.vector(scale(Valid.pp$revol_util))
Valid.pp$total_acc <- as.vector(scale(Valid.pp$total_acc))
Valid.pp$out_prncp <- as.vector(scale(Valid.pp$out_prncp))
Valid.pp$out_prncp_inv <- as.vector(scale(Valid.pp$out_prncp_inv))
Valid.pp <- dplyr::select(Valid.pp, -c(total_pymnt, total_pymnt_inv)) #future var removal
Valid.pp$total_rec_prncp <- as.vector(scale(Valid.pp$total_rec_prncp))
Valid.pp$total_rec_int <- as.vector(scale(Valid.pp$total_rec_int))
Valid.pp$total_rec_late_fee <- as.vector(scale(Valid.pp$total_rec_late_fee))
Valid.pp$recoveries <- as.vector(scale(Valid.pp$recoveries))
Valid.pp$collection_recovery_fee <- as.vector(scale(Valid.pp$collection_recovery_fee))
Valid.pp <- dplyr::select(Valid.pp, -c(last_pymnt_amnt)) #future var removal
Valid.pp$last_fico_range_high <- as.vector(scale(Valid.pp$last_fico_range_high))
Valid.pp$last_fico_range_low <- as.vector(scale(Valid.pp$last_fico_range_low))
Valid.pp$collections_12_mths_ex_med <- as.vector(scale(Valid.pp$collections_12_mths_ex_med))
Valid.pp$mths_since_last_major_derog <- as.vector(scale(Valid.pp$mths_since_last_major_derog))
Valid.pp <- dplyr::select(Valid.pp, -c(policy_code)) #var removal bc only one class
Valid.pp$acc_now_delinq <- as.vector(scale(Valid.pp$acc_now_delinq))
Valid.pp$tot_coll_amt <- as.vector(scale(Valid.pp$tot_coll_amt))
Valid.pp$tot_cur_bal <- as.vector(scale(Valid.pp$tot_cur_bal))
Valid.pp$open_acc_6m <- as.vector(scale(Valid.pp$open_acc_6m))
Valid.pp$open_act_il <- as.vector(scale(Valid.pp$open_act_il))
Valid.pp$open_il_12m <- as.vector(scale(Valid.pp$open_il_12m))
Valid.pp$open_il_24m <- as.vector(scale(Valid.pp$open_il_24m))
Valid.pp$mths_since_rcnt_il <- as.vector(scale(Valid.pp$mths_since_rcnt_il))
Valid.pp$total_bal_il <- as.vector(scale(Valid.pp$total_bal_il))
Valid.pp$il_util <- as.vector(scale(Valid.pp$il_util))
Valid.pp$open_rv_12m <- as.vector(scale(Valid.pp$open_rv_12m))
Valid.pp$open_rv_24m <- as.vector(scale(Valid.pp$open_rv_24m))
Valid.pp$max_bal_bc <- as.vector(scale(Valid.pp$max_bal_bc))
Valid.pp$all_util <- as.vector(scale(Valid.pp$all_util))
Valid.pp$total_rev_hi_lim <- as.vector(scale(Valid.pp$total_rev_hi_lim))
Valid.pp$inq_fi <- as.vector(scale(Valid.pp$inq_fi))
Valid.pp$total_cu_tl <- as.vector(scale(Valid.pp$total_cu_tl))
Valid.pp$inq_last_12m <- as.vector(scale(Valid.pp$inq_last_12m))
Valid.pp$acc_open_past_24mths <- as.vector(scale(Valid.pp$acc_open_past_24mths))
Valid.pp$avg_cur_bal <- as.vector(scale(Valid.pp$avg_cur_bal))
Valid.pp$bc_open_to_buy <- as.vector(scale(Valid.pp$bc_open_to_buy))
Valid.pp$bc_util <- as.vector(scale(Valid.pp$bc_util))
Valid.pp$chargeoff_within_12_mths <- as.vector(scale(Valid.pp$chargeoff_within_12_mths))
Valid.pp$delinq_amnt <- as.vector(scale(Valid.pp$delinq_amnt))
Valid.pp$mo_sin_old_il_acct <- as.vector(scale(Valid.pp$mo_sin_old_il_acct))
Valid.pp$mo_sin_old_rev_tl_op <- as.vector(scale(Valid.pp$mo_sin_old_rev_tl_op))
Valid.pp$mo_sin_rcnt_rev_tl_op <- as.vector(scale(Valid.pp$mo_sin_rcnt_rev_tl_op))
Valid.pp$mo_sin_rcnt_tl <- as.vector(scale(Valid.pp$mo_sin_rcnt_tl))
Valid.pp$mort_acc <- as.vector(scale(Valid.pp$mort_acc))
Valid.pp$mths_since_recent_bc <- as.vector(scale(Valid.pp$mths_since_recent_bc))
Valid.pp$mths_since_recent_bc_dlq <- as.vector(scale(Valid.pp$mths_since_recent_bc_dlq))
Valid.pp$mths_since_recent_inq <- as.vector(scale(Valid.pp$mths_since_recent_inq))
Valid.pp$mths_since_recent_revol_delinq <- as.vector(scale(Valid.pp$mths_since_recent_revol_delinq))
Valid.pp$num_accts_ever_120_pd <- as.vector(scale(Valid.pp$num_accts_ever_120_pd))
Valid.pp$num_actv_bc_tl <- as.vector(scale(Valid.pp$num_actv_bc_tl))
Valid.pp$num_actv_rev_tl <- as.vector(scale(Valid.pp$num_actv_rev_tl))
Valid.pp$num_bc_sats <- as.vector(scale(Valid.pp$num_bc_sats))
Valid.pp$num_bc_tl <- as.vector(scale(Valid.pp$num_bc_tl))
Valid.pp$num_il_tl <- as.vector(scale(Valid.pp$num_il_tl))
Valid.pp$num_op_rev_tl <- as.vector(scale(Valid.pp$num_op_rev_tl))
Valid.pp$num_rev_accts <- as.vector(scale(Valid.pp$num_rev_accts))
Valid.pp$num_rev_tl_bal_gt_0 <- as.vector(scale(Valid.pp$num_rev_tl_bal_gt_0))
Valid.pp$num_bc_sats <- as.vector(scale(Valid.pp$num_bc_sats))
Valid.pp$num_tl_120dpd_2m <- as.vector(scale(Valid.pp$num_tl_120dpd_2m))
Valid.pp$num_tl_30dpd <- as.vector(scale(Valid.pp$num_tl_30dpd))
Valid.pp$num_tl_90g_dpd_24m <- as.vector(scale(Valid.pp$num_tl_90g_dpd_24m))
Valid.pp$num_tl_op_past_12m <- as.vector(scale(Valid.pp$num_tl_op_past_12m))
Valid.pp$pct_tl_nvr_dlq <- as.vector(scale(Valid.pp$pct_tl_nvr_dlq))
Valid.pp$percent_bc_gt_75 <- as.vector(scale(Valid.pp$percent_bc_gt_75))
Valid.pp$pub_rec_bankruptcies <- as.vector(scale(Valid.pp$pub_rec_bankruptcies))
Valid.pp$tax_liens <- as.vector(scale(Valid.pp$tax_liens))
Valid.pp$tot_hi_cred_lim <- as.vector(scale(Valid.pp$tot_hi_cred_lim))
Valid.pp$total_bal_ex_mort <- as.vector(scale(Valid.pp$total_bal_ex_mort))
Valid.pp$total_bc_limit <- as.vector(scale(Valid.pp$total_bc_limit))
Valid.pp$total_il_high_credit_limit <- as.vector(scale(Valid.pp$total_il_high_credit_limit))
Valid.pp <- dplyr::select(Valid.pp, -c(debt_settlement_flag)) #factor removal
Valid.pp$inst_to_inc <- as.vector(scale(Valid.pp$inst_to_inc))
Valid.pp$open_acc_ratio <- as.vector(scale(Valid.pp$open_acc_ratio))


# save datasets
write.csv(Valid.pp, 'final_test_data_county.csv')
write.csv(Train.pp, 'final_train_data_county.csv')
write.csv(Valid.analysis, 'final_testA_data_county.csv')
write.csv(Train.analysis, 'final_trainA_data_county.csv')
