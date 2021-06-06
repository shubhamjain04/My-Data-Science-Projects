################################################################################
######################### SEGMENTATION CASE STUDY ##############################
################################################################################

# IMPORTING PACKAGES
require(dplyr)
require(psych)
require(Hmisc)
require(ggplot2)
require(GPArotation)
require(tables)

# IMPORTING DATASET 
credit_data <- read.csv("CC GENERAL.csv")
View(credit_data)

# GETTING SUMMARY THROUGH BASE PACKAGE
summary(credit_data)

# GETTING SUMMARY THROUGH PSYCH PACKAGE
psych::describe(credit_data)

# GETTING SUMMARY THROUGH HMISC PACKAGE
Hmisc::describe(credit_data)

# CHECKING DATA TYPES OF VARIABLES
sapply(credit_data, FUN = class)

# REMOVING CUST ID VARIABLE FROM DATA
credit_data <- credit_data[, -1]

# CREATING A USER DEFINED FUNCTION FOR UNIVARIATE ANALYSIS
univariate_analysis <- function(x){
  
  n = length(x)
  nmiss = sum(is.na(x))
  nmiss_pct = mean(is.na(x))
  sum = sum(x, na.rm = TRUE)
  mean = mean(x, na.rm = TRUE)
  median = quantile(x, p = 0.5, na.rm = TRUE)
  std = sd(x, na.rm = TRUE)
  var = var(x, na.rm = TRUE)
  range = max(x, na.rm = TRUE)-min(x, na.rm = TRUE)
  pctl = quantile(x, p = c(0, 0.01, 0.05,0.1,0.25,0.5, 0.75,0.9,0.95,0.99,1), na.rm = TRUE)
  return(c(NTotal = n, Nmiss = nmiss, Nmiss_pct = nmiss_pct, Sum = sum, Avg = mean, 
           Median = median, St.dv = std, Var = var, Range = range, Pctl = pctl))
}

# APPLYING UNIVARIATE ANALYSIS ON CREDIT DATA
summary_analysis_1 <- t(apply(credit_data, 2, FUN = univariate_analysis))
View(summary_analysis_1)
write.csv(summary_analysis_1, file = "summary_analysis_1.csv")

# OUTLIER TRETMENT 
# REPLACING 100TH PERCENTILE VALUE WITH 95TH PERCENTILE VALUE

outlier_treat <- function(x){
  
  UC = quantile(x, 0.95, na.rm = TRUE)
  x = ifelse(x > UC, UC, x)
}

# APPLYIG OUTLIER TREATMENT ON CREDIT DATA 
apply(credit_data, 2, FUN = outlier_treat)

# MISSING VALUE TREATMENT

## AFTER ANALYZING THE DATA OF THE CUSTOMER WITH MISSING VALUE OF CREDIT LIMIT,
## I DECIDED TO KEEP HIS CREDIT LIMIT 1000 UNITS, SINCE THIS CUSTOMER HAS SHOWN
## VERY LOW USAGE OF CREDIT CARD...
credit_data[is.na(credit_data$CREDIT_LIMIT), "CREDIT_LIMIT"] <- 1000

## REPLACING MISSING VALUES OF MINIMUM PAYMENTS WITH 10% VALUE OF BALANCE 
credit_data[is.na(credit_data$MINIMUM_PAYMENTS), "MINIMUM_PAYMENTS"] <- 0.1*credit_data[is.na(credit_data$MINIMUM_PAYMENTS), "BALANCE"]

# AGAIN APPLYING UNIVARIATE ANALYSIS ON CREDIT_DATA
summary_analysis_2 <- t(apply(credit_data, 2, FUN = univariate_analysis))
View(summary_analysis_2)
write.csv(summary_analysis_2, file = "summary_analysis_2.csv")

# DERIVING INTELLIGENT KPI'S
credit_data$monthly_avg_purchase <- credit_data$PURCHASES/credit_data$TENURE
credit_data$monthly_avg_cash_advance <- credit_data$CASH_ADVANCE/credit_data$TENURE
credit_data$limit_usage <- credit_data$BALANCE/credit_data$TENURE
credit_data$pay_to_min.pay <- credit_data$PAYMENTS/credit_data$MINIMUM_PAYMENTS
View(credit_data)

# CORRELATION MATRIX
cor_matrix <- data.frame(cor(credit_data))

## DROPPING PAY_TO_MIN.PAY VARIABLE SINCE IT IS HAVING INFNITE VALUES
## WHICH IS CAUSING PROBLEMS IN EIGEN VALUES CALCULATION
cor_matrix <- cor_matrix[-nrow(cor_matrix), -ncol(cor_matrix)]
View(cor_matrix)
write.csv(cor_matrix, file = "cor_matrix.csv")

# DECIDING NUMBER OF FACTORS USING SCREE PLOT & KAISER TEST(NUMBER OF EIGEN VALUES OVER 1)

## CLACULATING EIGEN VALUES
eigen(cor_matrix)$values                                                  

eigen_values <- mutate(data.frame(eigen(cor_matrix)$values)
                       ,cum_sum_eigen=cumsum(eigen.cor_matrix..values)
                       ,pct_var=eigen.cor_matrix..values/sum(eigen.cor_matrix..values)
                       ,cum_pct_var=cum_sum_eigen/sum(eigen.cor_matrix..values))
View(eigen_values)
write.csv(eigen_values, file = "eigen_values.csv")

## SCREE PLOT
plot.new()
scree(cor_matrix) 

plot(eigen_values$pct_var,type='b')


################################################################################
################### APPLYING FACTOR ANALYSIS ###################################
################################################################################

factor_analysis <- fa(r = cor_matrix, 7, rotate = "varimax", fm = "ml")
print(factor_analysis)

fa_sort <- fa.sort(factor_analysis)                                       
fa_sort

ls(fa_sort)                                                 
fa_sort$loadings
                                           
Loadings <- data.frame(fa_sort$loadings[1:ncol(cor_matrix),]) 
write.csv(Loadings, file = "Loadings.csv") 

# STANDARDIZING THE CREDIT_DATA
credit_data_final <- data.frame(scale(credit_data))
View(credit_data_final)

# STORING THE NAMES OF SELECTED VARIABLES AFTER FACTOR ANALYSIS
factor_variables <- c("ONEOFF_PURCHASES", "PURCHASES", "monthly_avg_purchase", "CASH_ADVANCE",
                      "monthly_avg_cash_advance", "BALANCE", "limit_usage", "CREDIT_LIMIT",
                      "MINIMUM_PAYMENTS", "PURCHASES_INSTALLMENTS_FREQUENCY", "ONEOFF_PURCHASES_FREQUENCY",
                      "INSTALLMENTS_PURCHASES", "TENURE")

# SUBSETTING THE CREDIT DATA
credit_data_final <- credit_data[, factor_variables]
View(credit_data_final)

################################################################################
############### APPLYING K-MEANS CLUSTERING ON CREDIT_DATA_FINAL ###############
################################################################################
cluster_three <- kmeans(credit_data_final, 3)
cluster_four  <- kmeans(credit_data_final, 4)
cluster_five  <- kmeans(credit_data_final, 5)
cluster_six   <- kmeans(credit_data_final, 6)
cluster_seven <- kmeans(credit_data_final, 7)
cluster_eight <- kmeans(credit_data_final, 8)

# COMBINING THE CLUSTER OBJECT OF VARIOUS CLUSTERS WITH CREDIT_DATA_FINAL

credit_data_final <- cbind(credit_data_final, km_clust_3 = cluster_three$cluster, 
                                              km_clust_4 = cluster_four$cluster,
                                              km_clust_5 = cluster_five$cluster,
                                              km_clust_6 = cluster_six$cluster,
                                              km_clust_7 = cluster_seven$cluster,
                                              km_clust_8 = cluster_eight$cluster)

credit_data <- cbind(credit_data, km_clust_3 = cluster_three$cluster, 
                                  km_clust_4 = cluster_four$cluster,
                                  km_clust_5 = cluster_five$cluster,
                                  km_clust_6 = cluster_six$cluster,
                                  km_clust_7 = cluster_seven$cluster,
                                  km_clust_8 = cluster_eight$cluster)

View(credit_data_final)

# PROFILING
# CONVERTING INTO FACTORS
credit_data_final$km_clust_3 = factor(credit_data_final$km_clust_3)
credit_data_final$km_clust_4 = factor(credit_data_final$km_clust_4)
credit_data_final$km_clust_5 = factor(credit_data_final$km_clust_5)
credit_data_final$km_clust_6 = factor(credit_data_final$km_clust_6)
credit_data_final$km_clust_7 = factor(credit_data_final$km_clust_7)
credit_data_final$km_clust_8 = factor(credit_data_final$km_clust_8)

credit_data$km_clust_3 = factor(credit_data$km_clust_3)
credit_data$km_clust_4 = factor(credit_data$km_clust_4)
credit_data$km_clust_5 = factor(credit_data$km_clust_5)
credit_data$km_clust_6 = factor(credit_data$km_clust_6)
credit_data$km_clust_7 = factor(credit_data$km_clust_7)
credit_data$km_clust_8 = factor(credit_data$km_clust_8)


profile <- tabular(1 + BALANCE + BALANCE_FREQUENCY + PURCHASES + ONEOFF_PURCHASES +              
                       INSTALLMENTS_PURCHASES + CASH_ADVANCE + PURCHASES_FREQUENCY +
                       ONEOFF_PURCHASES_FREQUENCY + PURCHASES_INSTALLMENTS_FREQUENCY +
                       CASH_ADVANCE_FREQUENCY + CASH_ADVANCE_TRX + PURCHASES_TRX +                 
                       CREDIT_LIMIT + PAYMENTS + MINIMUM_PAYMENTS + PRC_FULL_PAYMENT +                
                       TENURE + monthly_avg_purchase + monthly_avg_cash_advance +
                       limit_usage ~ mean + (mean*km_clust_3) + (mean*km_clust_4) + 
                                            (mean*km_clust_5) + (mean*km_clust_6) + 
                                            (mean*km_clust_7) + (mean*km_clust_8),
                   data = credit_data)

profile1 <- as.matrix(profile)

profile1 <- data.frame(profile1)
View(profile1)

profile <- tabular(1 ~ length + (length*km_clust_3) + (length*km_clust_4) + 
                                (length*km_clust_5) + (length*km_clust_6) +
                                (length*km_clust_7) + (length*km_clust_8),
                   data = credit_data)

profile2 <- as.matrix(profile)
profile2 <- data.frame(profile2)
View(profile2)

write.csv(profile1, "profile1.csv", row.names = F)
write.csv(profile2, "profile2.csv", row.names = F)



















