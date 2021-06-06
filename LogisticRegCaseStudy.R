##################################################################################
##################### LOGISTIC REGRESSION CASE STUDY #############################
##################################################################################

# IMPORTING PACKAGES  
require(dplyr)
require(psych)
require(e1071)
require(Hmisc)
require(ggplot2)
require(Metrics)
require(InformationValue)
require(pROC)
require(dgof)
require(tables)

# IMPORTING DATASET 
setwd("C:/Users/shubh/Desktop/Data Science Using R/Logistic Reg Case Study")
telecom_data <- read.csv("telecom data.csv")
#View(tail(telecom_data, 2000))
#View(telecom_data[is.na(telecom_data$REVENUE), ])

# GETTING A SUMMARY OF DATA THROUGH VARIOUS PACKAGES 
#summary_1 <- summary(telecom_data)
#summary_1

#summary_2 <- Hmisc::describe(telecom_data)
#summary_2

#summary_3 <- psych::describe(telecom_data)
#summary_3
#write.csv(summary_3, file = "summary_3.csv")

## AFTER ANALYZING DIFFERENT SUMMARIES OF DATASET I DECIDE TO 
## DROP ROWS WHERE VALUES OF 8 VARIABLES ARE MISSING VIZ. 
## REVENUE, MOU, RECCHRGE, DIRECTAS, OVERAGE, ROAM, CHANGEM, CHANGER
telecom_data <- telecom_data[!(is.na(telecom_data$REVENUE) &
                               is.na(telecom_data$MOU) &
                               is.na(telecom_data$RECCHRGE) &
                               is.na(telecom_data$DIRECTAS) &
                               is.na(telecom_data$OVERAGE) & 
                               is.na(telecom_data$ROAM) &
                               is.na(telecom_data$CHANGEM) &
                               is.na(telecom_data$CHANGER)), ]
# 216 ROWS ARE DROPPED FROM THE DATASET

# SEPERATING CATEGORICAL AND NUMERICAL COLUMNS
sapply(telecom_data, class)
num_vars <- names(telecom_data)[sapply(telecom_data, FUN = is.numeric)]
telecom_data_num <- telecom_data[,num_vars]

# CREATING A UNIVARIATE ANALYSIS FOR TELECOM_DATA_NUM
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

# APPLYING UNIVARIATE ANALYSIS ON TELECON_DATA_NUM
data_summary <- t(sapply(telecom_data_num, FUN = univariate_analysis))
#View(data_summary)
write.csv(data_summary, file = "data_summary_1.csv")

# OUTLIER TREATMENT AFTER ANALYZING RESULTS OF UNIVARIATE ANALYSIS

# USER DEFINED FUNCTION FOR OUTLIER TREATMENT

## REPLACING THE 100TH PERCENTILE VALUE WITH 95TH PERCENTILE VALUE AND
## 1ST PERCENTILE VALUE WITH 5TH PERCENTILE VALUE

out_treat <- function(x){
  
  UC1 = quantile(x, 0.95, na.rm = TRUE)
  LC1 = quantile(x, 0.05, na.rm = TRUE)
  
  x = ifelse(x > UC1, UC1, x)
  x = ifelse(x < LC1, LC1, x)
    
}

# APPLYING OUTLIER TREATMENT ON TELECOM DATA EXCEPT FOR DEPENDENT VARIABLE
telecom_data_num[ , -ncol(telecom_data_num)] <- apply(telecom_data_num[ , -ncol(telecom_data_num)], 2, FUN = out_treat)

# AGAIN APPLYING UNIVARIATE ANALYSIS ON TELECOM_DATA_NUM
data_summary <- t(sapply(telecom_data_num, FUN = univariate_analysis))
#View(data_summary)
write.csv(data_summary, file = "data_summary_2.csv")

# APPLYING MISSING VALUE TREATMENT AFTER ANALYZING UNIVARIATE ANALYSIS

# USER DEFINED FUNCTION FOR MISSING VALUE TREATMENT

## REPLACING MISSING VALUES WITH MEAN VALUES OF RESPECTIVE COLUMNS

miss_treat <- function(x){
  
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  return(x)
}

## APPLYING MISSING VALUE TREATMENT ON TELECOM DATA EXCEPT FOR DEPENDENT VARIABLE
## APPLYING MISSING VALUE TREATMENT ON CALIBRATION SAMPLE AND 
## VALIDATION SAMPLE SEPERATELY TO AVOID DATA LEAKAGE
telecom_data_num[telecom_data_num$CALIBRAT == 1 , -ncol(telecom_data_num)] <- 
apply(telecom_data_num[telecom_data_num$CALIBRAT == 1 , -ncol(telecom_data_num)], 2, FUN = miss_treat)

telecom_data_num[telecom_data_num$CALIBRAT == 0 , -ncol(telecom_data_num)] <- 
apply(telecom_data_num[telecom_data_num$CALIBRAT == 0 , -ncol(telecom_data_num)], 2, FUN = miss_treat)

# AGAIN APPLYING UNIVARIATE ANALYSIS ON TELECOM_DATA_NUM
data_summary <- t(sapply(telecom_data_num, FUN = univariate_analysis))
#View(data_summary)
write.csv(data_summary, file = "data_summary_3.csv")

# REPLACING THE MISSING VALUES OF INCOME WITH MEDIAN VALUE OF INCOME
telecom_data_num[telecom_data_num$INCOME == 0, "INCOME"] <- median(telecom_data_num$INCOME, na.rm = TRUE)

# REPLACING RAW DATA WITH MODIFIED DATA
telecom_data[ , num_vars] <- telecom_data_num[ , num_vars]

# REMOVING UNNECCESSARY COLUMNS 
telecom_data[ , c("INCMISS", "CSA")] <- NULL

# CORRELATION MATRIX
corr_matrix <- data.frame(cor(telecom_data, method = "pearson"))
#View(corr_matrix)
write.csv(corr_matrix, file = "corr_matrix.csv")
## FROM CORRELATION MATRIX WE CAN CONCLUDE THAT CORRELATION AMONG THE 
## INDEPENDENT VARIABLES IS VERY LOW WHICH IS A GOOD SIGN FOR OUR ANALYSIS

# SPLITTING THE DATA INTO TRAIN AND TEST
telecom_train <- telecom_data[telecom_data$CALIBRAT == 1, ]
telecom_test <- telecom_data[telecom_data$CALIBRAT == 0, ]

# CHECKING CORRELATION BETWEEN PREDICTOR VARIABLES AND Y VARIABLE
cor_xy <- cor(telecom_train, telecom_train$CHURNDEP)
#View(cor_xy)

# DEVELOPING THE MODEL 
fit <- glm(CHURNDEP ~ REVENUE + MOU + RECCHRGE + DIRECTAS + OVERAGE + ROAM + CHANGEM +
                      CHANGER + DROPVCE + BLCKVCE + UNANSVCE + CUSTCARE + THREEWAY +
                      MOUREC + OUTCALLS + INCALLS + PEAKVCE + OPEAKVCE + DROPBLK +
                      CALLFWDV + CALLWAIT + MONTHS + UNIQSUBS + ACTVSUBS + PHONES + MODELS +
                      EQPDAYS + AGE1 + AGE2 + CHILDREN + CREDITA + CREDITAA + CREDITB + CREDITC +
                      CREDITDE + CREDITGY + CREDITZ + PRIZMRUR + PRIZMUB + PRIZMTWN + REFURB + WEBCAP + TRUCK +
                      RV + OCCPROF + OCCCLER + OCCCRFT + OCCSTUD + OCCHMKR + OCCRET + OCCSELF + OWNRENT +
                      MARRYUN + MARRYYES + MARRYNO + MAILORD + MAILRES + MAILFLAG + TRAVEL + PCOWN + CREDITCD +
                      RETCALLS + RETACCPT + NEWCELLY + NEWCELLN + REFER + INCOME + MCYCLE + CREDITAD + SETPRCM +
                      SETPRC + RETCALL, 
            data = telecom_train, family = binomial(logit))
fit_summary_1 <- summary(fit)
fit_summary_1

# FINE TUNING MODEL BY REMOVING VARIABLES WITH NO IMPACT 
fit <- glm(CHURNDEP ~ REVENUE + MOU + RECCHRGE + DIRECTAS + OVERAGE + ROAM + CHANGEM +
             CHANGER + DROPVCE + BLCKVCE + UNANSVCE + CUSTCARE + THREEWAY +
             MOUREC + OUTCALLS + INCALLS + PEAKVCE + OPEAKVCE + DROPBLK +
             CALLWAIT + MONTHS + UNIQSUBS + ACTVSUBS + PHONES + MODELS +
             EQPDAYS + AGE1 + AGE2 + CHILDREN + CREDITA + CREDITAA + CREDITB + CREDITC +
             CREDITDE + CREDITGY + PRIZMRUR + PRIZMUB + PRIZMTWN + REFURB + WEBCAP + TRUCK +
             RV + OCCPROF + OCCCLER + OCCCRFT + OCCRET + OCCSELF + OWNRENT +
             MARRYUN + MARRYYES + MAILORD + MAILRES + MAILFLAG + TRAVEL + PCOWN + CREDITCD +
             RETCALLS + RETACCPT + NEWCELLY + NEWCELLN + REFER + INCOME + MCYCLE + CREDITAD + SETPRCM +
             SETPRC, 
           data = telecom_train, family = binomial(logit))
fit_summary_2 <- summary(fit)
fit_summary_2

# FURTHER FINE TUNING MODEL BY REMOVING VARIABLES WITH HIGH P-VALUE
fit <- glm(CHURNDEP ~ REVENUE + MOU + RECCHRGE + DIRECTAS + OVERAGE + ROAM + CHANGEM +
             CHANGER + BLCKVCE + UNANSVCE + CUSTCARE + THREEWAY +
             MOUREC + OUTCALLS + INCALLS + PEAKVCE + DROPBLK +
             CALLWAIT + MONTHS + UNIQSUBS + ACTVSUBS + PHONES +
             EQPDAYS + AGE1 + AGE2 + CHILDREN + CREDITAA + CREDITB + CREDITC +
             CREDITDE + CREDITGY + PRIZMRUR + PRIZMUB + PRIZMTWN + REFURB + WEBCAP + TRUCK +
             OCCPROF + OCCCLER + OCCCRFT + OCCRET + OCCSELF +
             MARRYUN + MARRYYES + MAILRES + MAILFLAG + PCOWN + CREDITCD +
             RETCALLS + RETACCPT + NEWCELLY + REFER + INCOME + MCYCLE + CREDITAD + SETPRCM +
             SETPRC, 
           data = telecom_train, family = binomial(logit))
fit_summary_3 <- summary(fit)
fit_summary_3

# PREDICTING VALUES USING THE FINAL MODEL 
telecom_test$CHURNDEP <- predict(fit, telecom_test, type = "response")
telecom_train$CHURNDEP_PRED <- predict(fit, telecom_train, type = "response")
#View(head(telecom_test, 20))

# EVALUATING AND VALIDATING THE MODEL

## CALCULATING CONCORDANCE
Concordance(telecom_test$CHURN, telecom_test$CHURNDEP)

## CALCULATING AUC SCORE
roc_obj <- roc(telecom_test$CHURN, telecom_test$CHURNDEP)
auc(roc_obj)

## CONFUSION MATRIX (TAKNG THRESHOLD = 0.5) 
telecom_test$CHURNDEP_PRED <- ifelse(telecom_test$CHURNDEP > 0.5, 1, 0)
confusionMatrix(telecom_test$CHURN, telecom_test$CHURNDEP_PRED)

## F1 SCORE
f1_1 <- Metrics::f1(telecom_test$CHURN, telecom_test$CHURNDEP_PRED)
f1_1

## ACCURACY
Accuracy_1 <- accuracy(telecom_test$CHURN, telecom_test$CHURNDEP_PRED)
Accuracy_1

## SENSITIVITY
Sensitivity_1 <- sensitivity(telecom_test$CHURN, telecom_test$CHURNDEP_PRED)
Sensitivity_1

## SPECIFICITY
Specificity_1 <- specificity(telecom_test$CHURN, telecom_test$CHURNDEP_PRED)
Specificity_1

## OPTIMIZING THRESHOLD USING ROC 
best_threshold <- coords(roc_obj, "best", "threshold", transpose = TRUE)
best_threshold
# --> USING COORDS METHOD OPTIMAL THRESHOLD IS COMING OUT TO BE 0.4966

## CONFUSION MATRIX (TAKNG THRESHOLD = 0.4966) 
telecom_test$CHURNDEP_PRED <- ifelse(telecom_test$CHURNDEP > 0.4966, 1, 0)
confusionMatrix(telecom_test$CHURN, telecom_test$CHURNDEP_PRED)

## F1 SCORE
f1_2 <- Metrics::f1(telecom_test$CHURN, telecom_test$CHURNDEP_PRED)
f1_2

## ACCURACY
Accuracy_2 <- accuracy(telecom_test$CHURN, telecom_test$CHURNDEP_PRED)
Accuracy_2

## SENSITIVITY
Sensitivity_2 <- sensitivity(telecom_test$CHURN, telecom_test$CHURNDEP_PRED)
Sensitivity_2

## SPECIFICITY
Specificity_2 <- specificity(telecom_test$CHURN, telecom_test$CHURNDEP_PRED)
Specificity_2

best_threshold2 <- optimalCutoff(telecom_test$CHURN, telecom_test$CHURNDEP_PRED, optimiseFor = "Both", returnDiagnostics = TRUE)
best_threshold2
# --> USING OPTIMALCUTOFF METHOD OPTIMAL THRESHOLD IS COMING OUT TO BE 1 

## CONFUSION MATRIX (TAKNG THRESHOLD = 1) 
telecom_test$CHURNDEP_PRED <- ifelse(telecom_test$CHURNDEP > 1, 1, 0)
confusionMatrix(telecom_test$CHURN, telecom_test$CHURNDEP_PRED)

## F1 SCORE
f1_3 <- Metrics::f1(telecom_test$CHURN, telecom_test$CHURNDEP_PRED)
f1_3

## ACCURACY
Accuracy_3 <- accuracy(telecom_test$CHURN, telecom_test$CHURNDEP_PRED)
Accuracy_3

## SENSITIVITY
Sensitivity_3 <- sensitivity(telecom_test$CHURN, telecom_test$CHURNDEP_PRED)
Sensitivity_3

## SPECIFICITY
Specificity_3 <- specificity(telecom_test$CHURN, telecom_test$CHURNDEP_PRED)
Specificity_3

## KS TABLE
ks_table <- InformationValue::ks_stat(telecom_test$CHURN, telecom_test$CHURNDEP_PRED, returnKSTable = TRUE)
write.csv(ks_table, file = "ks_table.csv")
#View(ks_table)

## KS CHART
## CREATING A NEW DATASET WITH ACTUAL Y LABEL AND PREDICTED PROBABILITIES
Y_and_Prob = as.data.frame(cbind(telecom_test$CHURN, telecom_test$CHURNDEP))
names(Y_and_Prob) <- c("Y","Prob")

## PERFORMING DECILING
decLocations <- quantile( Y_and_Prob$Prob, probs = seq(0.1,0.9,by = 0.1 ))
Y_and_Prob$decile <- findInterval(Y_and_Prob$Prob,c( -Inf, decLocations, Inf ))

decile_grp <- dplyr::group_by(Y_and_Prob,decile)
decile_summ <- dplyr::summarise(decile_grp, total_cnt = n(), min_prob = min(p = Prob), max_prob = max(Prob), churn_cnt = sum(Y),
                      non_churn_cnt = total_cnt - churn_cnt, 
                      churn_rate = (churn_cnt/total_cnt)*100)

decile_summ <- dplyr::arrange(decile_summ, desc(decile))
View(decile_summ)

# CALCULATING THE 1 AND 0 LABELS churn = 1, NON-churn = 0
sum1 <- sum(decile_summ$churn_cnt)
sum1
sum2 <- sum(decile_summ$non_churn_cnt)
sum2

decile_summ$churn_pct <- ((decile_summ$churn_cnt)/sum1)*100
decile_summ$non_churn_pct <- ((decile_summ$non_churn_cnt)/sum2)*100
decile_summ$cum_churn_pct <- cumsum(decile_summ$churn_pct)
decile_summ$cum_non_churn_pct <- cumsum(decile_summ$non_churn_pct)
decile_summ$ks_stats <- abs(decile_summ$cum_churn_pct-decile_summ$cum_non_churn_pct)
View(decile_summ)
write.csv(decile_summ, file = "decile_summ.csv")
# --> USING KS_STAT, OPTIMAL THRESHOLD IS COMING OUT TO BE 0.5017

## CONFUSION MATRIX (TAKNG THRESHOLD = 0.5017) 
telecom_test$CHURNDEP_PRED <- ifelse(telecom_test$CHURNDEP > 0.5017, 1, 0)
confusionMatrix(telecom_test$CHURN, telecom_test$CHURNDEP_PRED)

################################################################################
###################### METRICS FOR TEST DATASET ################################
################################################################################
## F1 SCORE
f1_4 <- Metrics::f1(telecom_test$CHURN, telecom_test$CHURNDEP_PRED)
f1_4

## ACCURACY
Accuracy_4 <- accuracy(telecom_test$CHURN, telecom_test$CHURNDEP_PRED)
Accuracy_4

## SENSITIVITY
Sensitivity_4 <- sensitivity(telecom_test$CHURN, telecom_test$CHURNDEP_PRED)
Sensitivity_4

## SPECIFICITY
Specificity_4 <- specificity(telecom_test$CHURN, telecom_test$CHURNDEP_PRED)
Specificity_4

################################################################################
###################### METRICS FOR TRAIN DATASET ###############################
################################################################################
telecom_train$CHURNDEP_PREDCLASS <- ifelse(telecom_train$CHURNDEP_PRED > 0.4966, 1, 0)
confusionMatrix(telecom_train$CHURNDEP, telecom_train$CHURNDEP_PREDCLASS)

## F1 SCORE
f1_5 <- Metrics::f1(telecom_train$CHURNDEP, telecom_train$CHURNDEP_PREDCLASS)
f1_5

## ACCURACY
Accuracy_5 <- accuracy(telecom_train$CHURNDEP, telecom_train$CHURNDEP_PREDCLASS)
Accuracy_5

## SENSITIVITY
Sensitivity_5 <- sensitivity(telecom_train$CHURNDEP, telecom_train$CHURNDEP_PREDCLASS)
Sensitivity_5

## SPECIFICITY
Specificity_5 <- specificity(telecom_train$CHURNDEP, telecom_train$CHURNDEP_PREDCLASS)
Specificity_5

# CONCORDANCE
Concordance(telecom_train$CHURNDEP, telecom_train$CHURNDEP_PREDCLASS)

## CALCULATING AUC SCORE
roc_obj <- roc(telecom_train$CHURNDEP, telecom_train$CHURNDEP_PREDCLASS)
auc(roc_obj)

best_threshold <- coords(roc_obj, "best", "threshold", transpose = TRUE)
best_threshold

################################################################################
######################### FACTOR ANALYSIS ######################################
################################################################################
require(GPArotation)
require(data.table)

# APPLYING FACTOR ANALYSIS FOR FEATURE REDUCTION
# USING DATASET COMPRISING ONLY OF CHURN CUSTOMERS(AS PREDICTED BY MODEL)
# DROPPING UNNECCESSARY VARIABLES FROM THE DATASET

telecom_data_churn <- telecom_test[telecom_test$CHURNDEP_PRED == 1, ]
#View(telecom_data_churn)
write.csv(telecom_data_churn, "telecom_data_churn.csv")

# READING TELECOM CHURN DATA
telecom_data_churn <- read.csv("telecom_data_churn.csv")

# REMOVING ALL NON-SIGNIFICANT VARIABLES FOR SEGMENTATION 
telecom_data_churn <- dplyr::mutate(telecom_data_churn, "CHURN" = NULL, "CHURNDEP" = NULL, "CHURNDEP_PRED" = NULL, "CALIBRAT" = NULL)


#str(telecom_data_churn, list.len = 150)

corr_matrix2 <- as.data.frame(cor(telecom_data_churn))
View(corr_matrix2)

# REMOVING ALL THE VARIABLES FROM THE DATASET WHERE CORRELATION IS 'NA'
telecom_data_churn <- dplyr::mutate(telecom_data_churn, "CALLFWDV" = NULL, "CREDITGY" = NULL, "CREDITZ" = NULL, "PRIZMRUR" = NULL,
                                                        "OCCCLER" = NULL, "OCCCRFT" = NULL, "OCCSTUD" = NULL, "OCCHMKR" = NULL,
                                                        "OCCRET" = NULL, "OCCSELF" = NULL, "MAILFLAG" = NULL, "RETCALLS" = NULL,
                                                        "RETACCPT" = NULL, "REFER" = NULL, "MCYCLE" = NULL, "CREDITAD" = NULL,
                                                        "RETCALL" = NULL,)

corr_matrix2 <- as.data.frame(cor(telecom_data_churn))
#corr_matrix2 <- abs(corr_matrix2[-1, -1])
View(corr_matrix2)
write.csv(corr_matrix2, file = "corr_matrix2.csv")


# CALCULATING EIGEN VALUES
eigen(corr_matrix2)$values
eigen_values <- dplyr::mutate(data.frame(eigen(corr_matrix2)$values), 
                              cum_sum_eigen = cumsum(eigen.corr_matrix2..values),
                              pct_var = eigen.corr_matrix2..values/sum(eigen.corr_matrix2..values),
                              cum_pct_var = cum_sum_eigen/sum(eigen.corr_matrix2..values))
View(eigen_values)
write.csv(eigen_values, file = "eigen_values.csv")

# SCREE PLOT
row_names <- colnames(corr_matrix2)
rownames(corr_matrix2) <- row_names
plot.new()
scree(corr_matrix2)


plot(eigen_values$pct_var, type = 'b')

################################################################################
################### APPLYING FACTOR ANALYSIS ###################################
################################################################################

factor_analysis <- fa(r = corr_matrix2, 16, rotate = "varimax", fm = "minres")
factor_analysis

fa_sort <- fa.sort(factor_analysis)                                       
fa_sort

ls(fa_sort)                                                 
fa_sort$loadings

Loadings <- data.frame(fa_sort$loadings[1:ncol(corr_matrix2),]) 
write.csv(Loadings, file = "Loadings.csv") 

# SCALING DATA TO APPLYING K-MEANS ALGORITHM
telecom_data_churn_scaled <- as.data.frame(scale(telecom_data_churn))
View(telecom_data_churn_scaled)

# STORING SELECTED VARIABLES FROM FACTOR ANALYSIS
factor_analysis <- c("OPEAKVCE", "MOUREC", "MOU", "PEAKVCE", "OUTCALLS", "CALLWAIT", "UNANSVCE", "DROPBLK",
                     "MAILRES", "AGE2", "OWNRENT", "MARRYYES", "AGE1", "CREDITCD", "MARRYUN",
                     "SETPRCM", "PHONES", "MODELS",
                     "MONTHS", "CUSTOMER", "EQPDAYS",
                     "ACTVSUBS", 
                     "MARRYNO", 
                     "CHANGEM", 
                     "TRUCK",
                     "CREDITB",
                     "REVENUE",
                     "CREDITA",
                     "CREDITC",
                     "BLCKVCE",
                     "PRIZMUB",
                     "CREDITDE")

# SUBSETTING THE TELECOM_DATA_CHURN_SCALED
telecom_data_churn_scaled <- telecom_data_churn_scaled[, factor_analysis]
View(telecom_data_churn_scaled)

################################################################################
############ APPLYING K-MEANS CLUSTERING ON TELECOM_DATA_CHURN_SCALED ##########
################################################################################
cluster_three <- kmeans(telecom_data_churn_scaled, 3)
cluster_four  <- kmeans(telecom_data_churn_scaled, 4)
cluster_five  <- kmeans(telecom_data_churn_scaled, 5)
cluster_six   <- kmeans(telecom_data_churn_scaled, 6)
cluster_seven <- kmeans(telecom_data_churn_scaled, 7)
cluster_eight <- kmeans(telecom_data_churn_scaled, 8)

# COMBINING THE CLUSTER OBJECT OF VARIOUS CLUSTERS WITH CREDIT_DATA_FINAL
telecom_data_churn_scaled <- cbind(telecom_data_churn_scaled, km_clust_3 = cluster_three$cluster, 
                                                              km_clust_4 = cluster_four$cluster,
                                                              km_clust_5 = cluster_five$cluster,
                                                              km_clust_6 = cluster_six$cluster,
                                                              km_clust_7 = cluster_seven$cluster,
                                                              km_clust_8 = cluster_eight$cluster)

telecom_data_churn <- cbind(telecom_data_churn, km_clust_3 = cluster_three$cluster, 
                                                km_clust_4 = cluster_four$cluster,
                                                km_clust_5 = cluster_five$cluster,
                                                km_clust_6 = cluster_six$cluster,
                                                km_clust_7 = cluster_seven$cluster,
                                                km_clust_8 = cluster_eight$cluster)

View(telecom_data_churn_scaled)
View(telecom_data_churn)

################################################################################
################################ PROFILING #####################################
################################################################################
telecom_data_churn$km_clust_3 = factor(telecom_data_churn$km_clust_3)
telecom_data_churn$km_clust_4 = factor(telecom_data_churn$km_clust_4)
telecom_data_churn$km_clust_5 = factor(telecom_data_churn$km_clust_5)
telecom_data_churn$km_clust_6 = factor(telecom_data_churn$km_clust_6)
telecom_data_churn$km_clust_7 = factor(telecom_data_churn$km_clust_7)
telecom_data_churn$km_clust_8 = factor(telecom_data_churn$km_clust_8)

profile <- tabular(1 + OPEAKVCE + MOUREC + MOU + PEAKVCE + OUTCALLS + CALLWAIT + UNANSVCE + 
                       DROPVCE + DROPBLK + INCALLS + REVENUE + CUSTCARE + DIRECTAS + THREEWAY + 
                       MAILRES + MAILORD + OWNRENT + MARRYYES + AGE1 + CREDITCD + MARRYUN + AGE2 + 
                       CHILDREN + PCOWN + OCCPROF + INCOME + TRAVEL + PRIZMTWN + PRIZMUB + SETPRCM + 
                       PHONES + MODELS + SETPRC + REFURB + MONTHS + CUSTOMER + EQPDAYS + CREDITA + 
                       WEBCAP +  NEWCELLY + NEWCELLN + UNIQSUBS + ACTVSUBS + CREDITAA + CREDITB + 
                       CREDITC + CREDITDE + CHANGEM + CHANGER + TRUCK + RV + RECCHRGE + ROAM + BLCKVCE + 
                       MARRYNO + OVERAGE ~ mean + (mean*km_clust_3) + (mean*km_clust_4) + 
                                                  (mean*km_clust_5) + (mean*km_clust_6) + 
                                                  (mean*km_clust_7) + (mean*km_clust_8),
                   data = telecom_data_churn)

profile1 <- as.matrix(profile)
profile1 <- data.frame(profile1)
View(profile1)

profile <- tabular(1 ~ length + (length*km_clust_3) + (length*km_clust_4) + 
                                (length*km_clust_5) + (length*km_clust_6) +
                                (length*km_clust_7) + (length*km_clust_8),
                   data = telecom_data_churn)

profile2 <- as.matrix(profile)
profile2 <- data.frame(profile2)
View(profile2)

write.csv(profile1, "profile1.csv", row.names = F)
write.csv(profile2, "profile2.csv", row.names = F)









