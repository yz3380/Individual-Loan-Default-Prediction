# read raw data
data_Q1 <- read.csv('LoanStats_securev1_2018Q1.csv', skip=1)
dnames <- colnames(data_Q1)

# BrowseNote (variables available to investors)
browsenote <- read.csv('BrowseNote.csv')
browsenote <- browsenote[1:120, ]
bnames <- as.vector(browsenote[,1])
# transform to lower case
bnames <- as.vector(sapply(bnames, tolower))

# remove punctuation characters and blanks
bnames <- gsub('[[:blank:]$]', '', bnames)
bnames_pure <- gsub('[[:punct:]]', '', bnames)
dnames_pure <- gsub('[[:punct:]]', '', dnames)

# check identical variable names
var_index <- dnames_pure %in% bnames_pure
dnames <- dnames[var_index]
dnames[104] <- 'loan_status'

# subset the data
data_Q1 <- subset(data_Q1, select=dnames)
dim(data_Q1) # now we have 104 variables (1 response)

# drop data with no response or multiple applicants
'%ni%' <- Negate('%in%')
data_Q1 <- data_Q1[data_Q1$loan_status %ni% c('Current', ''), ]
data_Q1 <- data_Q1[data_Q1$application_type %ni% c('Joint App', ''), ]
dim(data_Q1) # 104 variables (1 response)

# remove date time values
date_var <- c('earliest_cr_line', 'sec_app_earliest_cr_line')
data_Q1 <- data_Q1[, !(names(data_Q1) %in% date_var)]
dim(data_Q1) # 102 variables

# check missing values
missing <- colSums(is.na(data_Q1))
n <- dim(data_Q1)[1]
data_Q1 <- data_Q1[, missing/n<0.05]
dim(data_Q1) # 81 variables

# remove 'num_tl_120dpd_2m' because it has only 1 category
drop <- c('num_tl_120dpd_2m')

# multicolinearity, keep one with better interpretation
# 'grade', 'subgrade', 'int_rate'
drop = c(drop, 'grade', 'int_rate')
# 'purpose', 'title'
drop = c(drop, 'title')
# 'acc_now_delinq', 'num_tl_30dpd'
drop = c(drop, 'num_tl_30dpd')
# 'fico_range_high', 'fico_range_low'
drop = c(drop, 'fico_range_high')
# 'funded_amnt', 'installment', 'loan_amnt'
drop = c(drop, 'funded_amnt', 'loan_amnt')
# 'mo_sin_old_il_acct', 'mths_since_rcnt_il'
drop = c(drop, 'mo_sin_old_il_acct')
# 'num_actv_rev_tl', 'num_rev_tl_bal_gt_0'
drop = c(drop, 'num_rev_tl_bal_gt_0')
# 'num_sats', 'open_acc'
drop = c(drop, 'num_sats')
# 'tot_cur_bal', 'tot_hi_cred_lim'
drop = c(drop, 'tot_hi_cred_lim')
# 'total_bal_ex_mort', 'total_bal_il', 'total_il_high_credit_limit'
drop = c(drop, 'total_bal_ex_mort')
data_Q1 <- data_Q1[, !(names(data_Q1) %in% drop)]
dim(data_Q1) # 69 variables

# unrelated variables
drop2 <- c('id', 'emp_title', 'url', 'zip_code', 'addr_state', 'inital_list_status', 'collections_12_mths_ex_med', 'application_type', 'delinq_amnt')
data_Q1 <- data_Q1[, !(names(data_Q1) %in% drop2)]
dim(data_Q1) # 61 variables

# transformation
data_Q1$dti <- data_Q1$dti / 100
data_Q1$pct_tl_nvr_dlq <- data_Q1$pct_tl_nvr_dlq / 100
data_Q1$annual_inc <- log(data_Q1$annual_inc)
data_Q1$installment <- log(data_Q1$installment)
data_Q1$revol_bal <- ifelse(data_Q1$revol_bal==0, 0, log(data_Q1$revol_bal))
data_Q1$total_bal_il <- ifelse(data_Q1$total_bal_il==0, 0, log(data_Q1$total_bal_il))
data_Q1$tot_cur_bal <- ifelse(data_Q1$tot_cur_bal==0, 0, log(data_Q1$tot_cur_bal))
data_Q1$max_bal_bc <- ifelse(data_Q1$max_bal_bc==0, 0, log(data_Q1$max_bal_bc))
data_Q1$total_rev_hi_lim <- ifelse(data_Q1$total_rev_hi_lim==0, 0, log(data_Q1$total_rev_hi_lim))
data_Q1$avg_cur_bal <- ifelse(data_Q1$avg_cur_bal==0, 0, log(data_Q1$avg_cur_bal))
data_Q1$bc_open_to_buy <- ifelse(data_Q1$bc_open_to_buy==0, 0, log(data_Q1$bc_open_to_buy))
data_Q1$total_bc_limit <- ifelse(data_Q1$total_bc_limit==0, 0, log(data_Q1$total_bc_limit))
data_Q1$total_il_high_credit_limit <- ifelse(data_Q1$total_il_high_credit_limit==0, 0, log(data_Q1$total_il_high_credit_limit))
data_Q1$percent_bc_gt_75 <- data_Q1$percent_bc_gt_75 / 100
data_Q1$revol_util <- as.numeric(gsub('\\%','',data_Q1$revol_util)) / 100

# na values
data_Q1[is.na(data_Q1)] <- 0
# transform response variable and write into new csv
data_Q1$loan_status <- ifelse(data_Q1$loan_status=='Fully Paid', 1, 0)
dim(data_Q1) # 60 independent variables, 1 response, 34602 observation
write.csv(data_Q1,'2018Q1_processed.csv', row.names = FALSE)

