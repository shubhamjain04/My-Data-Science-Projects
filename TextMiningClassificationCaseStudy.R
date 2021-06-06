################################################################################
####### TEXT MINING CLASSIFICATION CASE STUDY - shubhamjainxyz@gmail.com #######
################################################################################

# IMPROTING PACKAGES
require(ggplot2)
require(plotly)
require(caret)
require(dplyr)
require(tm)
require(data.table)
require(nnet)
require(Metrics)
require(InformationValue)

# IMPORTING DATASET
text_data <- read.csv("yelp.csv")
View(text_data)

# EXPLORING DATA 
ncol(text_data)
nrow(text_data)

apply(text_data, 2, class)

# PROPROTION TABLE FOR STARS 
View(prop.table(table(text_data$stars)))

# CONVERTING STARS VARIABLE INTO FACTORS
text_data$stars <- as.factor(text_data$stars)

# FREQUENCY BAR CHART
bar_chart <- ggplot2::ggplot(text_data) + aes(x = stars) +
                      geom_bar(stat = "count") + 
                      xlab("STARS") + ylab("No. Of. Reviews") + 
                      theme_bw() + ggtitle("Distribution of STARS") 
                      
bar_chart <- plotly::ggplotly(bar_chart)                       
bar_chart

# TERM DOCUMENT MATRIX
## CREATE CORPUS
docs <- Corpus(VectorSource(text_data$text))
View(docs)
summary(docs)
docs

# CLEAN CORPUS
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

# FEATURE ENGINEERING 

## DOCUMENT TERM MATRIX
dtm <- DocumentTermMatrix(docs)
View(dtm)

## CREATE DTM WITH TF-IDF
dtm <- DocumentTermMatrix(docs, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))

## REMOVE SPARSE
new_docterm_corpus <- removeSparseTerms(dtm, sparse = 0.953)
################################################################################
##            KEEPING SPARSITY AT 0.953 I AM ABLE TO GET 197 FEATURES         ##
##         ON KEEPING SPARSITY AT 0.956 I AM ABLE TO GET 202 FEATURES         ##
##    BUT MY MODEL IS UNABLE TO EVALUATE 202 FEATURES, IT THROWS AN ERROR     ##
##                        i.e. TOO MANY WEIGHTS                               ##
################################################################################

colS <- colSums(as.matrix(new_docterm_corpus))

doc_features <- data.table(name = attributes(colS)$names, count = colS)

# CREATE DATASET FOR TRAINING 
processed_data <- as.data.table(as.matrix(new_docterm_corpus))
View(head(processed_data, 10))
# COMBINING DATA 
train_data <- cbind(data.table(ID = text_data$review_id,
                               Business_ID = text_data$business_id,
                               Stars = text_data$stars,
                               Text = text_data$text,
                               processed_data))

View(head(train_data, 10))

# MULTINOMIAL LOGISTIC REGRESSION
set.seed(123)

train_data$Stars <- as.factor(train_data$Stars)

# CREATING INDEXES FOR DATA PARTITION
# SPLITTING DATA 
set.seed(123)
indexes <- createDataPartition(train_data$Stars, times = 1, p = 0.7, list = FALSE)
train_text_data <- train_data[indexes, ] 
View(train_text_data)
test_text_data <- train_data[-indexes, ]
View(test_text_data)

# COMPARING PROPORTION OF STARS VARIABLE IN ORIGINAL AND SPLITTED DATA
View(cbind(as.data.frame(prop.table(table(train_data$Stars))), 
           as.data.frame(prop.table(table(train_text_data$Stars))),
           as.data.frame(prop.table(table(test_text_data$Stars)))))
View(head(train_text_data, 10))
View(head(train_data, 10))

# REMOVING UNNECCESSARY FEATURES FROM TRAIN_TEXT_DATA AND TEST_TEXT_DATA
train_text_data <- train_text_data[, -c(1,2,4)]
test_text_data <- test_text_data[, -c(1,2,4)]
View(head(train_text_data, 10))
View(head(test_text_data, 10))

# MODEL BUILDING
my_text_model <- nnet::multinom(Stars ~., data = train_text_data)

# APPLYING MODEL TO PREDICT STARS ON TRAINING DATA
train_text_data$predicted <- predict(my_text_model, train_text_data, type = "class")
View(head(train_text_data, 10))
predicted_train_data <- cbind(train_text_data[, c(1,2)], train_data[indexes, 4])
write.csv(predicted_train_data, file = "predicted_train_data.csv")

# REARRANGING COLUMNS
train_text_data <- train_text_data[, c(199, 1:198)]

# ACCURACY OF MODEL ON TRAINING DATA
accuracy(train_text_data$Stars, train_text_data$predicted)
train_pred <- table(train_text_data$Stars, train_text_data$predicted)
train_pred
accuracy <- sum(diag(train_pred))/sum(train_pred)
accuracy

################################################################################
##########  GETTING AN ACCURACY OF 52.01371% ON TRAINING DATASET  ##############
################################################################################

# APPLYING MODEL ON TEST DATA
test_text_data$predicted <- predict(my_text_model, test_text_data, type = "class")

# REARRANGING COLUMNS
test_text_data <- test_text_data[, c(199, 1:198)]
View(head(test_text_data, 10))

# ACCURACY OF MODEL ON TEST DATA
accuracy(test_text_data$Stars, test_text_data$predicted)
test_pred <- table(test_text_data$Stars, test_text_data$predicted)
test_pred
accuracy <- sum(diag(test_pred))/sum(test_pred)
accuracy

predicted_test_data <- cbind(test_text_data[, c(1,2)], train_data[-indexes, 4])
write.csv(predicted_test_data, file = "predicted_test_data.csv")

################################################################################
############  GETTING AN ACCURACY OF 47.06471% ON TEST DATASET  ################
################################################################################


################################################################################
## SINCE THE ACCURACY OF THIS MODEL IS COMING OUT TO BE VERY LOW, CONVERTING  ##
## THIS MULTINOMIAL PROBLEM TO A BINOMIAL PROBLEM BY CREATING A NEW DEPENDENT ##
## VARIABLE i.e. STARS = EITHER 0(BAD) IF STARS = 1/2/3 OR 1(GOOD) IF STARS   ##
## = 4/5.                                                                     ##
################################################################################

# ADDING NEW VARIABLE 'STARS2'
train_data$Stars2 <- ifelse(train_data$Stars == 1 | train_data$Stars == 2 | train_data$Stars == 3, 0, 1 )
View(head(train_data, 10))

# FREQUENCY BAR CHART
bar_chart <- ggplot2::ggplot(train_data) + aes(x = Stars2) +
                      geom_bar(stat = "count") + 
                      xlab("STARS") + ylab("No. Of. Reviews") + 
                      theme_bw() + ggtitle("Distribution of STARS") 
bar_chart <- plotly::ggplotly(bar_chart)                       
bar_chart

# REARRANGING VARIABLES
train_data <- train_data[, c(1:4, 202, 5:201)]
View(head(train_data, 10))

# MULTINOMIAL LOGISTIC REGRESSION
set.seed(123)
train_data$Stars2 <- as.factor(train_data$Stars2)

# CREATING INDEXES FOR DATA PARTITION
# SPLITTING DATA 
set.seed(123)
indexes <- createDataPartition(train_data$Stars2, times = 1, p = 0.7, list = FALSE)
train_text_data <- train_data[indexes, ] 
View(train_text_data)
test_text_data <- train_data[-indexes, ]
View(test_text_data)

# COMPARING PROPORTION OF STARS VARIABLE IN ORIGINAL AND SPLITTED DATA
View(cbind(as.data.frame(prop.table(table(train_data$Stars2))), 
           as.data.frame(prop.table(table(train_text_data$Stars2))),
           as.data.frame(prop.table(table(test_text_data$Stars2)))))
View(head(train_text_data, 10))
View(head(train_data, 10))

# REMOVING UNNECCESSARY FEATURES FROM TRAIN_TEXT_DATA AND TEST_TEXT_DATA
train_text_data <- train_text_data[, -c(1,2,3,4)]
test_text_data <- test_text_data[, -c(1,2,3,4)]
View(head(train_text_data, 10))
View(head(test_text_data, 10))

# MODEL BUILDING
my_text_model <- nnet::multinom(Stars2 ~., data = train_text_data)

# APPLYING MODEL TO PREDICT STARS ON TRAINING DATA
train_text_data$predicted2 <- predict(my_text_model, train_text_data, type = "class")
View(head(train_text_data, 10))
predicted_train_data <- cbind(train_text_data[, c(1,2)], train_data[indexes, 4])
write.csv(predicted_train_data, file = "predicted_train_data2.csv")

# REARRANGING COLUMNS
train_text_data <- train_text_data[, c(1, 199, 2:198)]
View(head(train_text_data, 10))

# ACCURACY OF MODEL ON TRAINING DATA
accuracy(train_text_data$Stars2, train_text_data$predicted2)
train_pred <- table(train_text_data$Stars2, train_text_data$predicted2)
train_pred
accuracy <- sum(diag(train_pred))/sum(train_pred)
accuracy

################################################################################
############  GETTING AN ACCURACY OF 79.67433% ON TRAINING DATASET  ############
################################################################################

# APPLYING MODEL ON TEST DATA
test_text_data$predicted2 <- predict(my_text_model, test_text_data, type = "class")

# REARRANGING COLUMNS
test_text_data <- test_text_data[, c(1, 199, 2:198)]
View(head(test_text_data, 10))

# ACCURACY OF MODEL ON TEST DATA
accuracy(test_text_data$Stars2, test_text_data$predicted2)
test_pred <- table(test_text_data$Stars2, test_text_data$predicted2)
test_pred
accuracy <- sum(diag(test_pred))/sum(test_pred)
accuracy

predicted_test_data <- cbind(test_text_data[, c(2,1)], train_data[-indexes, 4])
write.csv(predicted_test_data, file = "predicted_test_data2.csv")

################################################################################
############  GETTING AN ACCURACY OF 78.09269% ON TEST DATASET  ################
################################################################################


################################################################################
## TRYING THE SAME PROBLEM WITH THREE DIFFERENT RATINGS i.e. -1, 0, 1         ##
## IF STARS = 1 OR 2 THEN -1 (POOR)                                           ##
## IF STARS = 3 THEN 0       (NEUTRAL)                                        ##
## IF STARS = 4 OR 5 THEN 1  (GOOD)                                           ##
################################################################################

# CREATING NEW DEPENDENT VARIABLE STARS3 WITH ABOVE MENTIONED PROPERTIES
train_data$Stars3 <- ifelse(train_data$Stars == 1 | train_data$Stars == 2, -1, 
                            ifelse(train_data$Stars == 3, 0, 1))
train_data$Stars3 <- as.factor(train_data$Stars3)
View(head(train_data, 10))

# FREQUENCY BAR CHART
bar_chart <- ggplot2::ggplot(train_data) + aes(x = Stars3) +
                      geom_bar(stat = "count") + 
                      xlab("STARS") + ylab("No. Of. Reviews") + 
                      theme_bw() + ggtitle("Distribution of STARS") 
bar_chart <- plotly::ggplotly(bar_chart)                       
bar_chart

# REARRANGING VARIABLES
train_data <- train_data[, c(1:5, 202, 6:201)]
View(head(train_data, 10))

# MULTINOMIAL LOGISTIC REGRESSION
set.seed(123)
train_data$Stars3 <- as.factor(train_data$Stars3)

# CREATING INDEXES FOR DATA PARTITION
# SPLITTING DATA 
set.seed(123)
indexes <- createDataPartition(train_data$Stars3, times = 1, p = 0.7, list = FALSE)
train_text_data <- train_data[indexes, ] 
View(train_text_data)
test_text_data <- train_data[-indexes, ]
View(test_text_data)

# COMPARING PROPORTION OF STARS VARIABLE IN ORIGINAL AND SPLITTED DATA
View(cbind(as.data.frame(prop.table(table(train_data$Stars3))), 
           as.data.frame(prop.table(table(train_text_data$Stars3))),
           as.data.frame(prop.table(table(test_text_data$Stars3)))))
View(head(train_text_data, 10))
View(head(train_data, 10))

# REMOVING UNNECCESSARY FEATURES FROM TRAIN_TEXT_DATA AND TEST_TEXT_DATA
train_text_data <- train_text_data[, -c(1:4)]
test_text_data <- test_text_data[, -c(1:4)]
View(head(train_text_data, 10))
View(head(test_text_data, 10))

# MODEL BUILDING
my_text_model <- nnet::multinom(Stars3 ~., data = train_text_data)

# APPLYING MODEL TO PREDICT STARS ON TRAINING DATA
train_text_data$predicted3 <- predict(my_text_model, train_text_data, type = "class")
View(head(train_text_data, 10))
predicted_train_data <- cbind(train_text_data[, c(1,2)], train_data[indexes, 4])
write.csv(predicted_train_data, file = "predicted_train_data3.csv")

# REARRANGING COLUMNS
train_text_data <- train_text_data[, c(199, 1:198)]

# ACCURACY OF MODEL ON TRAINING DATA
accuracy(train_text_data$Stars3, train_text_data$predicted3)
train_pred <- table(train_text_data$Stars3, train_text_data$predicted3)
train_pred
accuracy <- sum(diag(train_pred))/sum(train_pred)
accuracy

################################################################################
############  GETTING AN ACCURACY OF 76.0068% ON TRAINING DATASET  #############
################################################################################

# APPLYING MODEL ON TEST DATA
test_text_data$predicted3 <- predict(my_text_model, test_text_data, type = "class")

# REARRANGING COLUMNS
test_text_data <- test_text_data[, c(199, 1:198)]
View(head(test_text_data, 10))

# ACCURACY OF MODEL ON TEST DATA
accuracy(test_text_data$Stars3, test_text_data$predicted3)
test_pred <- table(test_text_data$Stars3, test_text_data$predicted3)
test_pred
accuracy <- sum(diag(test_pred))/sum(test_pred)
accuracy

predicted_test_data <- cbind(test_text_data[, c(1,2)], train_data[-indexes, 4])
write.csv(predicted_test_data, file = "predicted_test_data3.csv")

################################################################################
############  GETTING AN ACCURACY OF 73.94923% ON TEST DATASET  ################
################################################################################


