require(forecast)
require(tseries)
require(tidyverse)

# Import Data
CaseShiller <- read.csv("C:/Users/91754/Desktop/SPCS20RPSNSA.csv")
# Check data import
head(CaseShiller)

# Change name of data column to Units
names(CaseShiller)[2] <- "Units"
# Creates a Units Object as intermediate to transform into time series
CSUnits <- CaseShiller$Units
# Creates Time Series Object that starts in January 2000 with a monthly frequency
tCaseShiller <- ts(CSUnits, start = c(2000, 1), frequency = 12)
# Check Data Transformation


tCaseShiller

# Automatically create ARIMA model
fit <- auto.arima(tCaseShiller)
fit


# Check Accuracy
accuracy(fit)

# Create a Simple Plot with a forecast for the next year
plot(forecast(fit, 12), xlab = "Date", ylab = "Units", main = "ARIMA Forecast for Case-Shiller Index")

# Get table of forecasted values. Check back next year to see whether this was close!
pred_values <- forecast(fit, 12)
pred_values

# Check assumptions of normality & Autocorrelation
qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals, type = "Ljung-Box")
# Has high p-value, so autocorrelations not significantly different than 0
# There are a few possible outliers, but most of the data is pretty normally distributed

ltCaseShiller <- log(tCaseShiller)
head(ltCaseShiller)


fit2 <- stl(ltCaseShiller, s.window = "period")

plot(fit2, main = "Seasonal Decomposition of log(Case-Shiller Units)")


seasonplot(tCaseShiller, year.labels = "TRUE", main = " Season Plot of Case-Shiller Units by Month", xlab = "Month", ylab = "Units", col = c("red", "orange", "gray", "green", "blue", "purple", "black"))

# Create a Season Plot
ggseasonplot(tCaseShiller, year.labels = TRUE, col = rainbow(20))

# Automatically create ARIMA model
fit3 <- auto.arima(ltCaseShiller)
fit3
# Check Accuracy
fitAccuracy <- data.frame(accuracy(fit))
fitAccuracy2 <- data.frame(accuracy(fit3))
fitAccuracyFinal <- rbind(fitAccuracy, fitAccuracy2)
fitAccuracyFinal

# Create a Simple Plot with a logged forecast for the next year
plot(forecast(fit3, 12), xlab = "Date", ylab = "Units", main = "ARIMA Forecast for Case-Shiller Index")

# Get table of forecasted values. Check back next year to see whether this was close!
# Original Data
pred_values <- data.frame(forecast(fit, 12))
# Log transformed data
pred_values2 <- data.frame(forecast(fit3, 12))
pred_values2[,1:5] <- exp(pred_values2[,1:5])
# Merge forecast predictions!
mergedDF <- data.frame(Date = rownames(pred_values), Original_Data_Forecast = pred_values$Point.Forecast, Log_Transformed_Data_Forecast = pred_values2$Point.Forecast, Difference = round(pred_values$Point.Forecast - pred_values2$Point.Forecast, 2))
mergedDF

