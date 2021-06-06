################################################################################
################ TIME SERIES FORECASTING CASE STUDY ############################
################################################################################

# IMPORTING PACKAGES
require(readxl)
require(dplyr)
require(reshape2)
require(forecast)

# IMPORTING DATASET
passenger_data <- read_excel("TimeSeries.xlsx")
View(passenger_data)

# CREATING A TIME SERIES OBJECT
ts_passenger_data <- ts(passenger_data[ , -c(1,2)], start = c(1996,1), end = c(2005,4), frequency = 4)
View(ts_passenger_data)
plot(ts_passenger_data)

# DECOMPOSING TIME SERIES DATA
plot(decompose(ts_passenger_data[,1] , type = c("multiplicative")))
plot(decompose(ts_passenger_data[,2] , type = c("multiplicative")))
plot(decompose(ts_passenger_data[,3] , type = c("multiplicative")))
plot(decompose(ts_passenger_data[,4] , type = c("multiplicative")))
plot(decompose(ts_passenger_data[,5] , type = c("multiplicative")))

# SEASONAL DECOMPOSITION OF TIME SERIES DATA

## DECOMPOSING PASSENGER MOVEMENT OF IRELAND
fit1 <- stl(ts_passenger_data[,1], s.window = "period")
summary(fit1)
plot(fit1)
ls(fit1)
print(fit1$time.series)
fit1$win

## DECOMPOSING PASSENGER MOVEMENT OF OTHER EU NOT IRELAND
fit2 <- stl(ts_passenger_data[,2], s.window = "period")
summary(fit2)
plot(fit2)

## DECOMPOSING PASSENGER MOVEMENT OF REST OF EUROPE AND MED
fit3 <- stl(ts_passenger_data[,3], s.window = "period")
summary(fit3)
plot(fit3)

## DECOMPOSING PASSENGER MOVEMENT OF REST OF WORLD
fit4 <- stl(ts_passenger_data[,4], s.window = "period")
summary(fit4)
plot(fit4)

## DECOMPOSING TOTAL PASSENGER MOVEMENT
fit5 <- stl(ts_passenger_data[,5], s.window = "period")
summary(fit5)
plot(fit5)

########################################
# USING HOLTSWINTER() TO FIT THE MODEL #
########################################

## FITTING MODEL ON PASSENGER MOVEMENT OF IRELAND
fit1 <- HoltWinters(ts_passenger_data[,1], beta = FALSE, gamma = FALSE)
plot(fit1)
accuracy(fit1$fitted, ts_passenger_data[,1])
fit1 <- HoltWinters(ts_passenger_data[,1], gamma = FALSE)
plot(fit1)
accuracy(fit1$fitted, ts_passenger_data[,1])
fit1 <- HoltWinters(ts_passenger_data[,1])
plot(fit1)
accuracy(fit1$fitted, ts_passenger_data[,1])

## FITTING MODEL ON PASSENGER MOVEMENT OF OTHER EU NOT IRELAND
fit2 <- HoltWinters(ts_passenger_data[,2], beta = FALSE, gamma = FALSE)
plot(fit2)
accuracy(fit2$fitted, ts_passenger_data[,2])
fit2 <- HoltWinters(ts_passenger_data[,2], gamma = FALSE)
plot(fit2)
accuracy(fit2$fitted, ts_passenger_data[,2])
fit2 <- HoltWinters(ts_passenger_data[,2])
plot(fit2)
accuracy(fit2$fitted, ts_passenger_data[,2])

## FITTING MODEL ON PASSENGER MOVEMENT OF REST OF EUROPE AND MED
fit3 <- HoltWinters(ts_passenger_data[,3], beta = FALSE, gamma = FALSE)
plot(fit3)
accuracy(fit3$fitted, ts_passenger_data[,3])
fit3 <- HoltWinters(ts_passenger_data[,3], gamma = FALSE)
plot(fit3)
accuracy(fit3$fitted, ts_passenger_data[,3])
fit3 <- HoltWinters(ts_passenger_data[,3])
plot(fit3)
accuracy(fit3$fitted, ts_passenger_data[,3])

## FITTING MODEL ON PASSENGER MOVEMENT OF REST OF WORLD
fit4 <- HoltWinters(ts_passenger_data[,4], beta = FALSE, gamma = FALSE)
plot(fit4)
accuracy(fit4$fitted, ts_passenger_data[,4])
fit4 <- HoltWinters(ts_passenger_data[,4], gamma = FALSE)
plot(fit4)
accuracy(fit4$fitted, ts_passenger_data[,4])
fit4 <- HoltWinters(ts_passenger_data[,4])
plot(fit4)
accuracy(fit4$fitted, ts_passenger_data[,4])

## FITTING MODEL TOTAL PASSENGER MOVEMENT
fit5 <- HoltWinters(ts_passenger_data[,5], beta = FALSE, gamma = FALSE)
plot(fit5)
accuracy(fit5$fitted, ts_passenger_data[,5])
fit5 <- HoltWinters(ts_passenger_data[,5], gamma = FALSE)
plot(fit5)
accuracy(fit5$fitted, ts_passenger_data[,5])
fit5 <- HoltWinters(ts_passenger_data[,5])
plot(fit5)
accuracy(fit5$fitted, ts_passenger_data[,5])

################################
# USING ETS() TO FIT THE MODEL #
################################

## FITTING MODEL ON PASSENGER MOVEMENT OF IRELAND
fit1 <- forecast::ets(ts_passenger_data[,1])
plot(fit1)
accuracy(fit1$fitted, ts_passenger_data[,1])

## FITTING MODEL ON PASSENGER MOVEMENT OF OTHER EU NOT IRELAND
fit2 <- forecast::ets(ts_passenger_data[,2])
plot(fit2)
accuracy(fit2$fitted, ts_passenger_data[,2])

## FITTING MODEL ON PASSENGER MOVEMENT OF REST OF EUROPE AND MED
fit3 <- forecast::ets(ts_passenger_data[,3])
plot(fit3)
accuracy(fit3$fitted, ts_passenger_data[,3])

## FITTING MODEL ON PASSENGER MOVEMENT OF REST OF WORLD
fit4 <- forecast::ets(ts_passenger_data[,4])
plot(fit4)
accuracy(fit4$fitted, ts_passenger_data[,4])

## FITTING ON TOTAL PASSENGER MOVEMENT 
fit5 <- forecast::ets(ts_passenger_data[,5])
plot(fit5)
accuracy(fit5$fitted, ts_passenger_data[,5])

##############################
# USING ARIMA AND AUTO ARIMA #
##############################

## FITTING MODEL ON PASSENGER MOVEMENT OF IRELAND
fit1 <- arima(ts_passenger_data[,1], order = c(1,0,1))
summary(fit1)
fit1 <- auto.arima(ts_passenger_data[,1])
summary(fit1)

## FITTING MODEL ON PASSENGER MOVEMENT OF OTHER EU NOT IRELAND
fit2 <- arima(ts_passenger_data[,2], order = c(1,0,1))
summary(fit2)
fit2 <- auto.arima(ts_passenger_data[,2])
summary(fit2)

## FITTING MODEL ON PASSENGER MOVEMENT OF REST OF EUROPE AND MED
fit3 <- arima(ts_passenger_data[,3], order = c(1,0,1))
summary(fit3)
fit3 <- auto.arima(ts_passenger_data[,3])
summary(fit3)

## FITTING MODEL ON PASSENGER MOVEMENT OF REST OF WORLD
fit4 <- arima(ts_passenger_data[,4], order = c(1,0,1))
summary(fit4)
fit4 <- auto.arima(ts_passenger_data[,4])
summary(fit4)

## FITTING MODEL ON TOTAL PASSENGER MOVEMENT
fit5 <- arima(ts_passenger_data[,5], order = c(1,0,1))
summary(fit5)
fit5 <- auto.arima(ts_passenger_data[,5])
summary(fit5)

# PREDICTING USING ETS() METHOD SINCE IT HAS LOWEST MAPE 

## FORECASTING VALUES FOR NEXT TWO QUARTERS FOR IRELAND
fit1 <- forecast::ets(ts_passenger_data[,1])
forecast::forecast(fit1, 2, level = c(90,95))
plot(forecast::forecast(fit1, 2, level = c(90,95)), main = "IRELAND", xlab = "YEAR-QTR", ylab = "EXPECTED PASSENGERS")

## FORECASTING VALUES FOR NEXT TWO QUARTERS FOR OTHER EUROPE NOT EU
fit2 <- forecast::ets(ts_passenger_data[,2])
forecast::forecast(fit2, 2, level = c(90,95))
plot(forecast::forecast(fit2, 2, level = c(90,95)), main = "OTHER EU NOT IRELAND", xlab = "YEAR-QTR", ylab = "EXPECTED PASSENGERS")

## FORECASTING VALUES FOR NEXT TWO QUARTERS FOR REST OF EUROPE AND MED
fit3 <- forecast::ets(ts_passenger_data[,3])
forecast::forecast(fit3, 2, level = c(90,95))
plot(forecast::forecast(fit3, 2, level = c(90,95)),main = "REST OF EUROPE AND MED", xlab = "YEAR-QTR", ylab = "EXPECTED PASSENGERS")

## FORECASTING VALUES FOR NEXT TWO QUARTERS FOR REST OF WORLD
fit4 <- forecast::ets(ts_passenger_data[,4])
forecast::forecast(fit4, 2, level = c(90,95))
plot(forecast::forecast(fit4, 2, level = c(90,95)), main = "REST OF WORLD", xlab = "YEAR-QTR", ylab = "EXPECTED PASSENGERS")

## FORECASTING VALUES FOR NEXT TWO QUARTERS FOR TOTAL PASSENGER MOVEMENT
fit5 <- forecast::ets(ts_passenger_data[,5])
forecast::forecast(fit5 , 2, level = c(90,95))
plot(forecast::forecast(fit5, 2, level = c(90,95)), main = "TOTAL PASSENGER MOVEMENT", xlab = "YEAR-QTR", ylab = "EXPECTED PASSENGERS")















