setwd("~/Desktop/Credit Default Prediction/R data")

# load packages
library(dplyr)
library(tidyverse)
library(caret)
library(glmnet)
library(e1071)
library(kernlab)
library(randomForest)
library(pROC)
library(MASS)
library(gbm)

# Run script called "County data files tidying" first to get the train and test set
# train: load country data files tidying 
# test: loan county data files tidying
train_analysis <- read_csv("final_trainA_data_county.csv")
test_analysis <- read_csv("final_testA_data_county.csv")

# eliminate variables in train not in test 
setdiff(names(train), names(test))
setdiff(names(test), names(train))
train <- dplyr::select(train, -c(home_ownership_NONE, purpose_educational, `title_Learning and training`, `title_New Baby and New House (CC Consolidate)`, `title_new kitchen for momma!`, title_odymeds, `title_Pay off Lowes Card`, `title_Paying off higher interest cards & auto`, title_SAVE, `title_Simple Loan Until Contract Is Completed`, `title_Student Loan`, `title_Trying to come back to reality!`))
test <- dplyr::select(test, -c(`title_Credit Card/Auto Repair`, title_DebtC, `title_Prescription Drug and Medical Costs`))
train <- train%>%mutate(loan_outcome = loan_outcome_1)
train <- dplyr::select(train, -c(loan_outcome_1, loan_outcome_0))
test <- test%>%mutate(loan_outcome = loan_outcome_1)
test <- dplyr::select(test, -c(loan_outcome_1, loan_outcome_0))

# drop post loan
train.new <- train
test.new <- test
train <- dplyr::select(train, -c(out_prncp, out_prncp_inv, total_rec_prncp, collection_recovery_fee, collections_12_mths_ex_med))
test <- dplyr::select(test, -c(out_prncp, out_prncp_inv, total_rec_prncp, collection_recovery_fee, collections_12_mths_ex_med))
train <- dplyr::select(train, -c(tot_hi_cred_lim, int_rate,  fico_range_low, mths_since_rcnt_il, il_util, num_actv_rev_tl, open_acc))
test <- dplyr::select(test, -c(tot_hi_cred_lim, int_rate, fico_range_low, mths_since_rcnt_il, il_util, num_actv_rev_tl, open_acc))

# remove NA vars
test <- dplyr::select(test, -c(inst_to_inc))
train <- dplyr::select(train, -c(inst_to_inc))

# remove features with originally two levels, making one binary redunant
train <- dplyr::select(train, -c(debt_settlement_flag_N, disbursement_method_Cash, initial_list_status_f))
test <- dplyr::select(test, -c(debt_settlement_flag_N, disbursement_method_Cash, initial_list_status_f))


# rename the variables becasue they have spaces in them
train$loan_outcome <- as.factor(train$loan_outcome)
train <- train%>%mutate(term_36_months = `term_36 months`)
train <- dplyr::select(train, -c(`term_36 months`))
train <- train%>%mutate(title_car_financing = `title_Car financing`, title_debt_consolidation = `title_Debt consolidation`, title_home_buying = `title_Home buying`, title_major_purchase = `title_Major purchase`, title_moving_and_relocation = `title_Moving and relocation`, application_type_joint_app = `application_type_Joint App`,  verification_status_not_verified = `verification_status_Not Verified`, title_credit_card_refinancing = `title_Credit card refinancing`, title_green_loan = `title_Green loan`, title_home_improvement = `title_Home improvement`, title_medical_expenses = `title_Medical expenses`)
train <- dplyr::select(train, -c(`title_Car financing`, `title_Debt consolidation`,  `title_Home buying`,  `title_Major purchase`, `title_Moving and relocation`,`application_type_Joint App`, `verification_status_Not Verified`,`title_Credit card refinancing`,  `title_Green loan`, `title_Home improvement`, `title_Medical expenses`))
train <- train%>%mutate(term_60_months = `term_60 months`)
train <- dplyr::select(train, -c(`term_60 months`))
train <- train%>%mutate(verification_status_verified = `verification_status_Source Verified`)
train <- dplyr::select(train, -c(`verification_status_Source Verified`))

test <- test%>%mutate(term_36_months = `term_36 months`)
test <- dplyr::select(test, -c(`term_36 months`))
test <- test%>%mutate(title_car_financing = `title_Car financing`, title_debt_consolidation = `title_Debt consolidation`, title_home_buying = `title_Home buying`, title_major_purchase = `title_Major purchase`, title_moving_and_relocation = `title_Moving and relocation`, application_type_joint_app = `application_type_Joint App`,  verification_status_not_verified = `verification_status_Not Verified`, title_credit_card_refinancing = `title_Credit card refinancing`, title_green_loan = `title_Green loan`, title_home_improvement = `title_Home improvement`, title_medical_expenses = `title_Medical expenses`)
test <- dplyr::select(test, -c(`title_Car financing`, `title_Debt consolidation`,  `title_Home buying`,  `title_Major purchase`, `title_Moving and relocation`,`application_type_Joint App`, `verification_status_Not Verified`,`title_Credit card refinancing`,  `title_Green loan`, `title_Home improvement`, `title_Medical expenses`))
test <- test%>%mutate(term_60_months = `term_60 months`)
test <- dplyr::select(test, -c(`term_60 months`))
test <- test%>%mutate(verification_status_verified = `verification_status_Source Verified`)
test <- dplyr::select(test, -c(`verification_status_Source Verified`))


train <- dplyr::select(train, -c(home_ownership_ANY, purpose_major_purchase,purpose_medical, purpose_moving, purpose_renewable_energy, purpose_vacation,purpose_wedding,title_Vacation, application_type_Individual, title_major_purchase, application_type_Individual, title_green_loan, title_medical_expenses))
test <- dplyr::select(test, -c(home_ownership_ANY, purpose_major_purchase,purpose_medical, purpose_moving, purpose_renewable_energy, purpose_vacation,purpose_wedding,title_Vacation, application_type_Individual, title_major_purchase, application_type_Individual, title_green_loan, title_medical_expenses))


# combine datasets until split later so they are not separated when we reduce train and test size
names(test_analysis) <- paste0(names(test_analysis), "_analysis")
names(train_analysis) <- paste0(names(train_analysis), "_analysis")

# put names into a vector 
name_vec <- as.vector(names(test_analysis))

# combine datasets
train <- cbind(train, train_analysis)
test <- cbind(test, test_analysis)

# remove high categorical vars
train = dplyr::select(train, -starts_with("addr_state"))
test = dplyr::select(test, -starts_with("addr_state"))
train = dplyr::select(train, -c(X1, term, home_ownership, verification_status, title))
test = dplyr::select(test, -c(X1, term, home_ownership, verification_status, title))
train = dplyr::select(train, -c(disbursement_method, initial_list_status, application_type))
test = dplyr::select(test, -c(disbursement_method, initial_list_status, application_type))




# further reduce train and test set size so that model will build
set.seed(7)
trainIndex <- createDataPartition(train$loan_outcome, p = .05,list = FALSE,times = 1)
traindata <- train[ trainIndex,]
trainIndex1 <- createDataPartition(traindata$loan_outcome, p = .7,list = FALSE,times = 1)
train1 <- traindata[ trainIndex1,]
#create test set 
trainIndex <- createDataPartition(test$loan_outcome, p = .116,list = FALSE,times = 1)
testdata <- test[ trainIndex,]
trainIndex1 <- createDataPartition(testdata$loan_outcome, p = .7,list = FALSE,times = 1)
test1 <- testdata[-trainIndex1,]

# delete the few observations that have NAs as a result of joining the local economic data
train1 = na.omit(train1)
test1 = na.omit(test1)

# separate the analysis data
train1_analysis <- dplyr::select(train1, ends_with("analysis"), gdp, growth, unemployment_rate, Population, per_capita_net_earnings, average_earnings, total_jobs_per_capita,
                                 average_earnings, total_jobs,gdp_per_capita, hs, bach, some_college, less_hs, poverty_rate, crime_rate)
test1_analysis <- dplyr::select(test1, ends_with("analysis"), gdp, growth, unemployment_rate, Population, per_capita_net_earnings, average_earnings, total_jobs_per_capita,
                                average_earnings, total_jobs,gdp_per_capita, hs, bach, some_college, less_hs, poverty_rate, crime_rate)

train1 <- dplyr::select(train1, -c(ends_with("analysis")), inst_to_inc_analysis)
test1 <- dplyr::select(test1, -c(ends_with("analysis")), inst_to_inc_analysis)

# further delete variables that are not present at time of the loan
train1 = dplyr::select(train1, -c(last_fico_range_high, last_fico_range_low, inq_last_6mths, inq_last_12m, inq_fi, total_rec_int, total_rec_late_fee, recoveries, acc_now_delinq, tot_coll_amt, tot_cur_bal, total_rev_hi_lim, acc_open_past_24mths, avg_cur_bal, bc_open_to_buy, bc_util, chargeoff_within_12_mths, delinq_amnt, mo_sin_old_il_acct, mo_sin_old_rev_tl_op, mo_sin_rcnt_rev_tl_op, mo_sin_rcnt_tl, mths_since_recent_bc, mths_since_recent_inq, num_accts_ever_120_pd, num_actv_bc_tl, num_rev_accts, num_il_tl, num_op_rev_tl, num_bc_sats, num_tl_op_past_12m, pct_tl_nvr_dlq, percent_bc_gt_75, tax_liens, total_bal_ex_mort, total_bc_limit, total_il_high_credit_limit, disbursement_method_DirectPay, debt_settlement_flag_Y , last_fico_range_high , last_fico_range_low))
test1 = dplyr::select(test1, -c(last_fico_range_high, last_fico_range_low, inq_last_6mths,inq_last_12m, inq_fi, total_rec_int, total_rec_late_fee, recoveries,  acc_now_delinq, tot_coll_amt, tot_cur_bal, total_rev_hi_lim, acc_open_past_24mths, avg_cur_bal, bc_open_to_buy, bc_util, chargeoff_within_12_mths, delinq_amnt, mo_sin_old_il_acct, mo_sin_old_rev_tl_op, mo_sin_rcnt_rev_tl_op, mo_sin_rcnt_tl, mths_since_recent_bc, mths_since_recent_inq, num_accts_ever_120_pd, num_actv_bc_tl, num_rev_accts, num_il_tl, num_op_rev_tl, num_bc_sats, num_tl_op_past_12m, pct_tl_nvr_dlq, percent_bc_gt_75, tax_liens, total_bal_ex_mort, total_bc_limit, total_il_high_credit_limit, disbursement_method_DirectPay, debt_settlement_flag_Y, last_fico_range_high, last_fico_range_low))

train1 = dplyr::select(train1, -c(sub_grade_pp))
test1 = dplyr::select(test1, -c(sub_grade_pp))
train1 = dplyr::select(train1, -c(zip_code))
test1 = dplyr::select(test1, -c(zip_code))
train1 = dplyr::select(train1, -c(issue_d))
test1 = dplyr::select(test1, -c(issue_d))

# remove problematic observations as a result of inst_to_inc
train1 = train1%>%filter(inst_to_inc_analysis<=100)
test1 = test1%>%filter(inst_to_inc_analysis<=100)

# scale the local economic data
train1$inst_to_inc_analysis <- as.vector(scale(train1$inst_to_inc_analysis))
train1$gdp <- as.vector(scale(train1$gdp))
train1$growth <- as.vector(scale(train1$growth))
train1$unemployment_rate <- as.vector(scale(train1$unemployment_rate))
train1$Population <- as.vector(scale(train1$Population))
train1$per_capita_net_earnings <- as.vector(scale(train1$per_capita_net_earnings))
train1$average_earnings <- as.vector(scale(train1$average_earnings))
train1$total_jobs_per_capita <- as.vector(scale(train1$total_jobs_per_capita))
train1$total_jobs <- as.vector(scale(train1$total_jobs))
train1$gdp_per_capita <- as.vector(scale(train1$gdp_per_capita))
train1$hs <- as.vector(scale(train1$hs))
train1$bach <- as.vector(scale(train1$bach))
train1$some_college <- as.vector(scale(train1$some_college))
train1$less_hs <- as.vector(scale(train1$less_hs))
train1$poverty_rate <- as.vector(scale(train1$poverty_rate))
train1$crime_rate <- as.vector(scale(train1$crime_rate))
 
test1$gdp <- as.vector(scale(test1$gdp))
test1$growth <- as.vector(scale(test1$growth))
test1$unemployment_rate <- as.vector(scale(test1$unemployment_rate))
test1$Population <- as.vector(scale(test1$Population))
test1$per_capita_net_earnings <- as.vector(scale(test1$per_capita_net_earnings))
test1$average_earnings <- as.vector(scale(test1$average_earnings))
test1$total_jobs_per_capita <- as.vector(scale(test1$total_jobs_per_capita))
test1$total_jobs <- as.vector(scale(test1$total_jobs))
test1$gdp_per_capita <- as.vector(scale(test1$gdp_per_capita))
test1$hs <- as.vector(scale(test1$hs))
test1$bach <- as.vector(scale(test1$bach))
test1$some_college <- as.vector(scale(test1$some_college))
test1$less_hs <- as.vector(scale(test1$less_hs))
test1$poverty_rate <- as.vector(scale(test1$poverty_rate))
test1$crime_rate <- as.vector(scale(test1$crime_rate))
test1$inst_to_inc_analysis <- as.vector(scale(test1$inst_to_inc_analysis))



# Logistic Regression
ctrl <- trainControl(method = "cv",summaryFunction = twoClassSummary,classProbs = TRUE,number = 5)
glmnGrid = expand.grid(.alpha = seq(0, 1, length = 10), .lambda = 0) #gird search for hyperparameters
which( colnames(train1)=="loan_outcome" ) 
levels(train1$loan_outcome) <- c("paid", "default")
#kappa statsitic used because the data is imbalanced
glmnTuned1 = caret::train(loan_outcome~., data=train1,method = "glmnet",tuneGrid = glmnGrid,metric = "Kappa",trControl = ctrl)
levels(train1$loan_outcome) <- c("0", "1")
model = cv.glmnet(x = as.matrix(train1[-35]),y = train1$loan_outcome,alpha = 0.3333,family = "binomial",standardize = FALSE)
lr_res = predict(model, as.matrix(test1[-35]), type = "response")
#view results
roc(as.factor(loan_outcome) ~ lr_res, data = test1)

#predict train
lr_train = predict(model, as.matrix(train1[-35]), type = "response")
lr_train = lr_train$`1`
lr_train = as.data.frame(lr_train)
lr_train = lr_train$`1`



# train Random Forest
set.seed(123)
trControl <- trainControl(method = "cv",
                          number = 5)
rf_tune <- train(loan_outcome~.,
                    data = train1,
                    method = "rf",
                    ntree = 500,
                    trControl = trControl, 
                 tuneGrid = data.frame(mtry = 5))
# predict test set
rf_res = predict(rf_tune, test1[-35], type = 'prob')
rf_res <- as.data.frame(rf_res)
rf_res <- rf_res$`default`


# predict train 
rf_train = predict(rf_tune, train1[-35], type = 'prob')
rf_train = as.data.frame(rf_train)
rf_train = rf_train$default

# gbm model (tuning process not shown)
# create weights 
class_weights <- ifelse(train1$loan_outcome == "0", 1, 10)
set.seed(123)
gbm.tuned <- gbm(
  formula = as.numeric(train1$loan_outcome)-1 ~ .,
  distribution = 'bernoulli',
  data = train1[-35],
  n.trees = 1000,
  interaction.depth = 4,
  shrinkage = 0.01,
  cv.folds = 5,
  verbose = FALSE, 
  n.cores = 1,
  weights = class_weights
) 

# predict test
gbm_res = predict(gbm.tuned, test1[-35], type = 'prob')
# observe results 
roc(as.factor(loan_outcome) ~ gbm_res, data = test1)
# predict train 
gbm_train = predict(gbm.tuned, train1[-35], type = 'response')

# lda model
set.seed(123)
ctrl = trainControl(method="cv", number = 5)
lda = caret::train(loan_outcome~., data=train1,method = "lda",metric = "Kappa",trControl = ctrl)
lda <- lda(as.factor(train1$loan_outcome)~., data = train1[-35])

# predict test set
lda_res <- predict(lda, test1[-35], type = 'prob')
lda_res <- as.data.frame(lda_res)

# observe results
roc(as.factor(loan_outcome) ~ as.numeric(lda_res$default), data = test1)

# predict lda train 
lda_train =  predict(lda, train1[-35], type = 'prob')
lda_train = as.data.frame(lda_train)
lda_train = lda_train$default

# create ensemble predictions
# turn into vector
lda_res = lda_res$default

predict_loan_status_ensemble = lr_res +
  rf_res +
  gbm_res +
  lda_res

predict_loan_status_ensemble = predict_loan_status_ensemble / 4
ensemble_res <- predict_loan_status_ensemble

#predict train 
ens_train = (lda_train + rf_train +gbm_train +lr_train)/4

#observe results
roc(as.factor(loan_outcome) ~ as.numeric(ensemble_res), data = test1)
roc(as.factor(loan_outcome) ~ as.numeric(ens_train), data = train1)



#create test dataset 
compare <- (test1_analysis)

compare = compare%>%filter(inst_to_inc_analysis<=100)
for ( col in 1:ncol(compare)){
  colnames(compare)[col] <-  sub("_analysis", "", colnames(compare)[col])
}

compare <- compare%>%mutate(actual_return = (total_pymnt+recoveries)/(loan_amnt))
#calculate loss given default based on defaulted loans 
compare%>%filter(loan_outcome==1)%>%group_by(grade)%>%summarize(lgd = 1-mean(actual_return))


compare <- compare%>%mutate(lgd = grade)
compare$lgd <- as.factor(compare$lgd)
levels(compare$lgd) <- c(".335",".357",".358",".317",".421",".429",".531")
compare$lgd <- as.numeric(as.character(compare$lgd))



