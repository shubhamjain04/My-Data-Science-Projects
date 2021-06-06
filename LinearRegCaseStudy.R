################################################################################
##################### LINEAR REGRESSION CASE STUDY #############################
################################################################################

# IMPORTING PACKAGES

require(dplyr)
require(hmisc)
require(readxl)
require(ggplot2)
require(plotly)
require(psych)
require(caret)
require(sqldf)
require(car)
require(MASS)
require(fastDummies)

# IMPORTING DATASET
setwd("C:/Users/shubh/Desktop/Data Science Using R/Linear Reg Case Study/Linear Regression Case")
customer_data <- read_excel("Linear Regression Case.xlsx", sheet = 1)
View(customer_data)

# GETTING SUMMARY USING BASE R PACKAGE 
summary(customer_data)

# GETTING SUMMARY USING HMISC PACKAGE
Hmisc::describe(customer_data)

# GETTING SUMMARY USING PSYCH PACKAGE
psych::describe(customer_data)

# EXTRACTING ALL THE NUMERICAL FEATURES
num_vars <- names(customer_data)[sapply(customer_data, FUN = is.numeric)]
cust_data_num <- customer_data[, num_vars]

# USER DEFINED FUNCTION FOR UNIVARIATE ANALYSIS FOR NUMERICAL FEATURES
univ_analysis <- function(x){
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
  return(c(NTotal = n, Nmiss = nmiss, Nmiss_pct = nmiss_pct, Sum = sum, Avg = mean, Median = median, St.dv = std, Var = var, Range = range, Pctl = pctl))
}


# APPLYING UNIVARIATE ANALYSIS ON NUMERICAL COLUMNS
summar_unv_anl <- t(apply(cust_data_num, 2, FUN = univ_analysis))
View(summar_unv_anl)
write.csv(summar_unv_anl, file = "summar_unv_anl_1.csv")


# OUTLIER TREATMENT AFTER ANALYZING SUMMARY OF UNIVARIATE ANALYSIS

## REPLACING THE 100TH PERCENTILE VALUE WITH 95TH PERCENTILE VALUE AND
## 1ST PERCENTILE VALUE WITH 5TH PERCENTILE VALUE

outlier_treat <- function(x){
  UC1 = quantile(x, p = 0.95, na.rm = T)
  LC1 = quantile(x, p = 0.05, na.rm = T)
  x = ifelse(x > UC1, UC1, x)
  x = ifelse(x < LC1, LC1, x)
  return(x)
}

# APPLYING OUTLIER TREATMENT ON NUMERICAL VARIABLES
cust_data_num <- apply(cust_data_num, 2, FUN = outlier_treat)

# AGAIN APPLYING UNIVARIATE ANALYSIS ON NUMERICAL COLUMNS
summar_unv_anl <- t(apply(cust_data_num, 2, FUN = univ_analysis))
View(summar_unv_anl)
write.csv(summar_unv_anl, file = "summar_unv_anl_2.csv")

# MISSING VALUE TREATMENT AFTER ANALYZING SUMMARY OF UNIVARIATE ANALYSIS

## REPLACING VALUE OF TOWNSIZE WITH MEDIAN SINCE IT IS CATEGORICAL VARIABLE
cust_data_num[is.na(cust_data_num[,"townsize"]), "townsize"] <- median(cust_data_num[,"townsize"], na.rm = TRUE)

## REPLACING VALUES OF FURTHER VARIABLES WITH MEAN SINCE THEY ARE NUMERIC VARIABLES
cust_data_num[is.na(cust_data_num[,"commutetime"]), "commutetime"] <- mean(cust_data_num[,"commutetime"], na.rm = TRUE)
cust_data_num[is.na(cust_data_num[,"cardten"]), "cardten"] <- mean(cust_data_num[,"cardten"], na.rm = TRUE)
cust_data_num[is.na(cust_data_num[,"longten"]), "longten"] <- mean(cust_data_num[,"longten"], na.rm = TRUE)

# REPLACING ALL THE UNDEFINED LOG VALUES WITH 0 SINCE LOG OF ZERO IS UNDEFINED
cust_data_num[is.na(cust_data_num)] <- 0

# AGAIN APPLYING UNIVARIATE ANALYSIS ON NUMERICAL COLUMNS
summar_unv_anl <- t(apply(cust_data_num, 2, FUN = univ_analysis))
View(summar_unv_anl)
write.csv(summar_unv_anl, file = "summar_unv_anl_3.csv")

# CREATING A NEW VARIABLE FOR TOTAL SPEND (PRIMARY + SECONDARY)
cust_data_num <- data.frame(cust_data_num)
cust_data_num$total_spend <- cust_data_num$cardspent + cust_data_num$card2spent
customer_data$total_spend <- cust_data_num$total_spend

# CHECKING THE DISTRIBUTION OF DEPENDENT VARIABLE I.E TOTAL_SPEND
hist_total_spend <- ggplot2::ggplot(data = cust_data_num) + 
                             aes(x = total_spend) + 
                             geom_histogram(bins = 30, fill = "skyblue", color = "black") +
                             theme_bw()

hist_total_spend <- plotly::ggplotly(hist_total_spend)
hist_total_spend

# APPLYING NATURAL LOG TRANSFORMATION TO REDUCE SKEWNESS IN TOTAL_SPEND
cust_data_num$lntotal_spend <- log(cust_data_num$total_spend)
customer_data$lntotal_spend <- cust_data_num$lntotal_spend

# CHECKING THE DISTRIBUTION OF DEPENDENT VARIABLE I.E LNTOTAL_SPEND
hist_lntotal_spend <- ggplot2::ggplot(data = cust_data_num) + 
                               aes(x = lntotal_spend) + 
                               geom_histogram(bins = 30, fill = "skyblue", color = "black") +
                               theme_bw()

hist_lntotal_spend <- plotly::ggplotly(hist_lntotal_spend)
hist_lntotal_spend

# SINCE THE TRANSFORMED TOTAL_SPEND I.E LNTOTAL_SPEND IS MORE 
# NORMALLY DISTRIBURED WE WILL USE IT FOR FURTHER ANALYSIS

# REPLACING THE VALUES IN THE RAW DATA WITH MODIFIED DATA
customer_data[, num_vars] <- cust_data_num[, num_vars]

# CHECKING CORRELATION MATRIX
corr_matrix <- data.frame(cor(cust_data_num, method = "pearson"))
View(corr_matrix)
write.csv(corr_matrix, file = "corr_matrix.csv")
# FROM CORRELATION MATRIX WE CAN CONCLUDE THAT CORRELATION B/W THE INDEPENDENT VARIABLES IS VERY LOW
# WHICH IS A GOOD SIGN FOR OUR ANALYSIS

# CREATING DUMMY VARIABLES FOR CATEGORICAL VARIABLES i.e. 
# agecat, edcat, inccat, jobcat, spousedcat

## CONVERTING CATEGORICAL VARIABLES TO FACTORS TO CREATE DUMMIES
customer_data$agecat <- as.factor(customer_data$agecat)
customer_data$edcat <- as.factor(customer_data$edcat)
customer_data$inccat <- as.factor(customer_data$inccat)
customer_data$jobcat <- as.factor(customer_data$jobcat)
customer_data$spousedcat <- as.factor(customer_data$spousedcat)

dummy_vars <- fastDummies::dummy_cols(customer_data, 
                                      select_columns = c("agecat", "edcat", "inccat", "jobcat", "spousedcat", "empcat", "cardtenurecat", "card2tenurecat"),
                                      remove_first_dummy = TRUE) 
View(dummy_vars)

# COMBINING ORIGINAL DATA AND DUMMY VARIABLES
customer_data <- as.data.frame(dummy_vars)
View(customer_data)
write.csv(customer_data, file = "customer_data.csv")

# CREATING DEVELOPMENT(70%) AND VAIDATION(30%) DATASETS
set.seed(123)
index <- sample(1:nrow(customer_data), floor(nrow(customer_data)*0.67))

dev_data <- customer_data[index, ]
val_data <- customer_data[-index, ]

nrow(dev_data)
nrow(val_data)

# SINCE THE VALUE OF R-SQUARE WAS LESS THAN 30% WHEN I USED LOG TRANSFORMATION OF TOTAL SPEND 
# THEREFORE TO IMPROVE R-SQUARE I AM DEVELOPING MY MODEL ON ORIGINAL VARIABLE i.e. TOTAL_SPEND

# DEVELOPING THE MODEL 
fit <- lm(total_spend ~ region + townsize + gender + age + agecat_3 + agecat_4 + agecat_5 + agecat_6 +
                          ed + edcat_2 + edcat_3 + edcat_4 + edcat_5 +
                          jobcat_2 + jobcat_3 + jobcat_4 + jobcat_5 + jobcat_6 +
                          union + inccat_2 + inccat_3 + inccat_4 + inccat_5 + employ +
                          empcat_2 + empcat_3 + empcat_4 + empcat_5 + 
                          retire + lninc + income + debtinc + lncreddebt +
                          othdebt + lnothdebt + default + jobsat + marital + spoused + 
                          spousedcat_1 + spousedcat_2 + spousedcat_3 + spousedcat_4 + 
                          reside + pets + pets_cats + pets_dogs + pets_birds + pets_reptiles +
                          pets_small + pets_saltfish + pets_freshfish + homeown + hometype +
                          addresscat + cars + carown + cartype + carvalue + carcatvalue +
                          carbought + carbuy + commutecat + commutetime + commutecar +
                          commutemotorcycle + commutecarpool + commutebus + commuterail +
                          commutepublic + commutebike + commutewalk + commutenonmotor +
                          telecommute + reason + polview + polparty + polcontrib + vote +
                          card + cardtype + cardbenefit + cardfee + cardtenure + 
                          cardtenurecat_2 + cardtenurecat_3 + cardtenurecat_4 + cardtenurecat_5 +
                          card2 + card2type + card2benefit + card2fee + card2tenure + 
                          card2tenurecat_2 + card2tenurecat_3 + card2tenurecat_4 + card2tenurecat_5 +
                        
                          active + bfast + tenure + churn + longmon + 
                          longten + tollfree + tollmon + tollten + equip + equipmon +
                          equipten + callcard + cardmon + cardten + wireless + wiremon +
                          wireten + multline + voice + pager + internet + callid + callwait + forward +
                          confer + ebill + owntv + hourstv + ownvcr + owndvd + owncd + ownpda + ownpc +
                          ownipod + owngame + ownfax + news + response_01 + response_02 + response_03,
                          data = dev_data) 

summary_fit_1 <- summary(fit)
summary_fit_1
# stepAIC(fit, direction = "both")
ls(fit)
alias(fit)
car::vif(fit)

# REMOVING VARIABLES WITH P-VALUE MORE THAN 0.9 AND NA'S / IMPROVING THE MODEL
fit <- lm(total_spend ~ region + townsize + gender + age + agecat_3 + agecat_4 + agecat_5 + agecat_6 +
            ed + edcat_2 + edcat_3 + edcat_4 +
            jobcat_2 + jobcat_3 + jobcat_4 + jobcat_5 + jobcat_6 +
            union + inccat_2 + inccat_3 + inccat_4 + inccat_5 + employ +
            empcat_2 + empcat_3 + empcat_4 + empcat_5 + 
            retire + lninc + income + debtinc + lncreddebt +
            othdebt + lnothdebt + default + jobsat + marital + spoused + 
            spousedcat_1 + spousedcat_2 + spousedcat_3 + 
            pets + pets_cats + pets_dogs + pets_birds +
            pets_freshfish + homeown + hometype +
            addresscat + cars + carown + cartype + carcatvalue +
            carbought + carbuy + commutecat + commutetime + commutecar +
            commutemotorcycle + commutecarpool + commutebus + commuterail +
            commutepublic + commutebike + commutewalk + commutenonmotor +
            telecommute + reason + polview + polparty + polcontrib + vote +
            card + cardtype + cardbenefit + cardfee + cardtenure + 
            cardtenurecat_2 + cardtenurecat_3 + cardtenurecat_4 + cardtenurecat_5 +
            card2 + card2type + card2benefit + card2fee + card2tenure + 
            card2tenurecat_3 + card2tenurecat_4  +
            
            active + bfast + tenure + churn + longmon + 
            longten + tollfree + tollmon + tollten + equipmon +
            equipten + cardmon + wireless + wiremon +
            multline + voice + internet + callid + callwait + forward +
            confer + ebill + owntv + hourstv + ownvcr + owndvd + owncd + ownpc +
            ownipod + owngame + ownfax + response_01 + response_02 + response_03,
          data = dev_data) 

summary_fit_2 <- summary(fit)
summary_fit_2
alias(fit)
car::vif(fit)

# REMOVING VARIABLES WITH P-VALUE MORE THAN 0.8 / FURTHER IMPROVING THE MODEL
fit <- lm(total_spend ~ region + townsize + gender + age + agecat_3 + agecat_4 + agecat_5 + agecat_6 +
            ed + edcat_2 + edcat_3 + edcat_4 +
            jobcat_2 + jobcat_3 + jobcat_4 + jobcat_5 + jobcat_6 +
            union + inccat_2 + inccat_4 + inccat_5 + employ +
            empcat_2 + empcat_5 + 
            retire + lninc + income + debtinc + lncreddebt +
            othdebt + default + jobsat + marital + spoused + 
            spousedcat_1 + spousedcat_2 + spousedcat_3 + 
            pets + pets_cats + pets_birds +
            pets_freshfish + homeown + hometype +
            carown + cartype + carcatvalue +
            carbought + carbuy + commutecat + commutetime + commutecar +
            commutecarpool + commutebus + commuterail +
            commutebike + commutewalk + commutenonmotor +
            telecommute + reason + polview + polparty + polcontrib + vote +
            card + cardtype + cardbenefit + cardtenure + 
            cardtenurecat_2 + cardtenurecat_3 + cardtenurecat_4 + cardtenurecat_5 +
            card2 + card2type + card2benefit + card2fee + card2tenure + 
            card2tenurecat_3 + card2tenurecat_4  +
            
            active + bfast + tenure + churn + longmon + 
            longten + tollfree + tollmon + tollten + equipmon +
            equipten + cardmon + wireless + wiremon +
            multline + voice + internet + callid + callwait + forward +
            confer + ebill + owntv + hourstv + ownvcr + owndvd + owncd + ownpc +
            ownipod + owngame + ownfax + response_01 + response_02 + response_03,
          data = dev_data) 

summary_fit_3 <- summary(fit)
summary_fit_3
car::vif(fit)

# REMOVING VARIABLES WITH P-VALUE MORE THAN 0.6 / FURTHER IMPROVING THE MODEL
fit <- lm(total_spend ~ region + townsize + gender + age + agecat_4 + agecat_5 + agecat_6 +
            ed + edcat_2 + edcat_4 +
            jobcat_2 + jobcat_4 + jobcat_5 + jobcat_6 +
            union + inccat_2 + inccat_4 + inccat_5 + employ +
            empcat_2 + empcat_5 + 
            retire + lninc + income + debtinc + lncreddebt +
            jobsat + marital + spoused + 
            spousedcat_1 + spousedcat_2 + spousedcat_3 + 
            pets + pets_birds +
            pets_freshfish + homeown + hometype +
            cartype + carcatvalue +
            carbuy + commutecat + commutetime +
            commutecarpool + commuterail +
            commutebike + commutewalk + commutenonmotor +
            telecommute + reason + polcontrib + vote +
            card + cardtype + cardbenefit + cardtenure + 
            cardtenurecat_2 + cardtenurecat_3 + cardtenurecat_4 + cardtenurecat_5 +
            card2 + card2benefit + card2fee + card2tenure + 
            card2tenurecat_3 + card2tenurecat_4  +
            
            bfast + tenure + churn + longmon + 
            longten + tollfree + tollmon + tollten +
            cardmon + wireless + wiremon +
            multline + voice + internet + callid + callwait + forward +
            ebill + owndvd + owncd + ownpc +
            owngame + ownfax + response_01 + response_02 + response_03,
          data = dev_data) 

summary_fit_4 <- summary(fit)
summary_fit_4
car::vif(fit)

# SELECTING ONLY THE SIGNIFICANT VARIABLES FROM THE MODEL
fit <- lm(total_spend ~ region + gender + age + agecat_4 + agecat_5 + agecat_6 +
            edcat_4 + internet + ed + 
            inccat_5 + lninc + lncreddebt +
            spoused + card2tenurecat_3 + cardtenure + cardtype + polcontrib +
            cartype + carcatvalue + reason + carbuy + spousedcat_1 + marital + 
            commutecarpool + 
            card + jobsat + debtinc + inccat_4 + jobcat_5 + jobcat_6 + 
            cardtenurecat_3 + cardtenurecat_4 + cardtenurecat_5 + 
            card2 + card2fee + 
            cardmon + wireless + wiremon + voice + response_03,
          data = dev_data) 

summary_fit_5 <- summary(fit)
summary_fit_5
car::vif(fit)

# SELECTING VARIABLES HAVING VIF < 5
fit <- lm(total_spend ~ region + gender + agecat_4 + agecat_6 +
            edcat_4 + internet + ed + 
            inccat_5 + lninc + lncreddebt +
            card2tenurecat_3 + cardtype + polcontrib +
            cartype + carcatvalue + reason + carbuy + spousedcat_1 + 
            commutecarpool + 
            card + jobsat + debtinc + inccat_4 + jobcat_5 + jobcat_6 + 
            cardtenurecat_3 + cardtenurecat_4 + 
            card2 + card2fee + 
            cardmon + voice + response_03,
          data = dev_data) 

summary_fit_6 <- summary(fit)
summary_fit_6

# APPLYING MODEL ON DEVELOPMENT DATASET
dev_data$ln_spend_pred <- predict(fit, dev_data)
ggplot2::ggplot(data = dev_data) + 
         aes(x = ln_spend_pred) + 
         geom_histogram(bins = 30, fill = "skyblue", color = "black")

# APPLYING MODEL ON VALIDATION DATASET
val_data$lntotal_spend_pred <- predict(fit, val_data) 
View(val_data)
ggplot2::ggplot(data = val_data) + 
         aes(x = lntotal_spend_pred) + 
         geom_histogram(bins = 30, fill = "skyblue", color = "black")
View(val_data)

# CALCULATING ACCURACY METRICS
## MAPE - MEAN ABSOLUTE PERCENTAGE ERROR

# FOR DEVELOPMENT DATASET
mape <- mean(abs(dev_data$total_spend - dev_data$ln_spend_pred)/dev_data$total_spend)
mape

# FOR VALIDATION DATASET
mape <- mean(abs(val_data$total_spend - val_data$lntotal_spend_pred)/val_data$total_spend)
mape

## RMSE - ROOT MEAN SQUARED ERROE 

# FOR DEVELOPMENT DATASET
rmse <- sqrt(mean((dev_data$total_spend - dev_data$ln_spend_pred)**2))
rmse

# FOR VALIDATION DATASET
rmse <- sqrt(mean((val_data$total_spend - val_data$lntotal_spend_pred)**2))
rmse

## CORRELATION BETWEEN ACTUAL AND PREDICTED VALUE

corr_act_pred <- cor(val_data$total_spend, val_data$lntotal_spend_pred)
corr_act_pred

## DECILE ANALYSIS

decLocations <- quantile(val_data$lntotal_spend_pred, probs = seq(0.1, 0.9, by = 0.1))
View(decLocations)
val_data$decile <- findInterval(val_data$total_spend, c(-Inf, decLocations, Inf))
summary(val_data$decile)
xtabs(~decile, val_data)

decile_analysis <- sqldf("select decile, count(decile) as Count, avg(lntotal_spend_pred) as Avg_lntotal_spend_pred,
                          avg(total_spend) as Avg_lntotal_spend
                          from val_data
                          group by decile
                          order by decile desc")
View(decile_analysis)
write.csv(decile_analysis, file = "decile_analysis.csv")

# HISTOGRAM OF RESIDUALS
hist(residuals(summary_fit_1))
hist(residuals(summary_fit_2))
hist(residuals(summary_fit_3))
hist(residuals(summary_fit_4))
hist(residuals(summary_fit_5))
hist(residuals(summary_fit_6))




