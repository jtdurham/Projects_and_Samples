## Our objective: make a linear regression forecast model for temperature on CO2
## and build a confidence/prediction band around it

# a

# Read in CO2 data from both sources

co2_1 = read.csv("CO2_Smoothed.csv")
co2_2 = read.csv("CO2_Mean.csv")

# Read in temperature data

temp = read.csv("Temperature.csv")

# Only keep relevant columns

temp[which(!(names(temp) %in% c("Year", "Use")))] = NULL

# Prepare CO2 datasets for merging by making the variable names consistent

co2_2$unc = NULL
co2_names = c("year", "co2")
names(co2_1) = co2_names
names(co2_2) = co2_names

# Join the two CO2 datasets by year, keeping only years common to both

co2_same = merge(co2_1, co2_2, by=("year"))
co2_years = co2_same$year

# Calculate the mean CO2 for each year and eliminate unnecesary columns

co2_same[["co2"]] = (co2_same$co2.x + co2_same$co2.y) / 2
co2_same$co2.x = NULL
co2_same$co2.y = NULL

# Combine the two CO2 datasets, keeping only years they do not have in common

co2_full = rbind(co2_1, co2_2)
co2_full= co2_full[-which(co2_full$year %in% co2_years),]

# Combine the unshared and shared years of CO2 data

all_co2 = rbind(co2_full, co2_same)
all_co2 = all_co2[order(all_co2$year),]

# Separate the CO2 and temperature data into training and test sets

train_years = 1850:1974
test_years = 1975:2016

train_co2 = all_co2[which(all_co2$year %in% train_years),]
test_co2 = all_co2[which(all_co2$year %in% test_years),]

train_temp = temp[which(temp$Year %in% train_years),]
test_temp = temp[which(temp$Year %in% test_years),]

train = cbind(train_co2[order(match(train_temp$Year, train_co2$year)),], Temp = train_temp$Use) #order the CO2 data by the indices in the temperature data for an accurate join
test = cbind(test_co2[order(match(test_temp$Year, test_co2$year)),], Temp = test_temp$Use)

# b

# Create a linear regression model of temperature on CO2 over the training set

train_model = lm(Temp ~ co2, data=train)
alpha_hat = train_model$coefficients[[1]]
beta_hat = train_model$coefficients[[2]]

# c

# Prepare variables for interval calculations

xs = train$co2
expectation_co2 = mean(xs)
n = length(xs)

# Calculate predicted temperature values given test CO2 values using the linear model

test_preds = alpha_hat + beta_hat * test$co2

# Calculate the square root portion of the standard error

squared_deviations = (test$co2 - expectation_co2)^2
s_xx = var(xs) * (n - 1)
root_conf = sqrt((1 / n) + (squared_deviations / s_xx))

# Calculate the square root of the MSE portion of the standard error

errors = (alpha_hat + beta_hat * train$co2) - train$Temp
s_squared = (1 / (n - 2)) * sum(errors^2)
s = sqrt(s_squared)

# Calculate the t value at 95% confidence

alpha = 0.05
t = qt(1 - (alpha / 2), n - 2)

# Calculate the high and low values of the confidence interval

s_y_est = s * root_conf
conf_interval_high = test_preds + (t * s_y_est)
conf_interval_low = test_preds - (t * s_y_est)

plot(test$co2, test_preds, ylim = c(-.25 ,0.82), main = "Confidece Interval - With Parameter Uncertainty")
lines(test$co2, conf_interval_high, type = "l")
lines(test$co2, conf_interval_low, type = "l")

# Calculate the z value at 95% confidence

z = qnorm(1 - (alpha / 2))

# Calculate the high and low values of the confidence interval with no parameter uncertainty

conf_interval_high_no_unc = test_preds + (z * s)
conf_interval_low_no_unc = test_preds - (z * s)

plot(test$co2, test_preds, ylim = c(-.25 ,0.82), main = "Confidence Interval - No Parameter Uncertainty")
lines(test$co2, conf_interval_high_no_unc, type = "l")
lines(test$co2, conf_interval_low_no_unc, type = "l")

#predict(train_model, test_co2["co2"], interval = "confidence", se.fit = T)$fit

# d

# Calculate a new standard error for the prediction interval

root_pred = sqrt(1 + (1 / n) + (squared_deviations / s_xx))
s_y = s * root_pred

# Calculate the high and low values of the 95% prediction interval

pred_interval_high = test_preds + (t * s_y)
pred_interval_low = test_preds - (t * s_y)

plot(test$co2, test_preds, ylim = c(-.25 ,0.82), main = "Prediction Interval - With Parameter Uncertainty")
lines(test$co2, pred_interval_high, type = "l")
lines(test$co2, pred_interval_low, type = "l")

# Calculate the high and low values of the 95% prediction interval with no parameter uncertainty

pred_interval_high_no_unc = test_preds + (z * s)
pred_interval_low_no_unc = test_preds - (z * s)

plot(test$co2, test_preds, ylim = c(-.25 ,0.82), main = "Prediction Interval - No Parameter Uncertainty")
points(test$co2, test$Temp, col = "blue")
lines(test$co2, pred_interval_high_no_unc, type = "l")
lines(test$co2, pred_interval_low_no_unc, type = "l")

# Determine how many actual temperature values in the test set fall in the prediction interval

in_interval_flag = (test$Temp < pred_interval_high & test$Temp > pred_interval_low) * 1
in_interval = sum(in_interval_flag)
out_interval = length(in_interval_flag) - in_interval
interval_percentage = in_interval / length(in_interval_flag)

# e

# Calculate the point estimates and prediction interval for the training set

train_preds = alpha_hat + beta_hat * train$co2
train_pred_int_high = train_preds + (z * s)
train_pred_int_low = train_preds - (z * s)

# Prepare temperatures and years for plotting

temps = c(train$Temp, test$Temp)
years = c(train$year, test$year)

# Plot the temperature data over all years along with linear regression estimates and the prediction interval

dot_col = "#999999"
plot(years, temps, col = "white", main = "Prediction Interval - No Parameter Uncertainty")
points(train$year, train$Temp, pch = 21, col = dot_col, bg = dot_col)
points(test$year, test$Temp, col = "black")
lines(test$year, test_preds, col = "blue")
lines(test$year, pred_interval_high_no_unc, col = "blue", lty = "dashed")
lines(test$year, pred_interval_low_no_unc, col = "blue", lty = "dashed")
lines(train$year, train_preds, col = "orange")
lines(train$year, train_pred_int_high, col = "dark orange", lty = "dashed")
lines(train$year, train_pred_int_low, col = "dark orange", lty = "dashed")
legend("topleft", text.width = 30, legend=c("Training Data", "Test Data", "Predicted Values, Test", "Prediction Interval, Test", "Predicted Values, Training", "Prediction Interval, Training"), pch = c(21, 1, NA, NA, NA, NA), pt.bg = c(dot_col, NA, NA, NA, NA, NA), lty = c(NA, NA, 1, 2, 1, 2), col = c(dot_col, "black", "blue", "blue", "dark orange", "dark orange"))

# f

# Calculate the PIT values for the training set

pits = pnorm(test$Temp, test_preds, s)
b = seq(0, 1, 0.1)

# Plot the histogram and empirical CDF of the PIT values

hist(pits, breaks = b, freq = F, main = "Histogram of PIT Values - Test Data", xlab = "PIT Values")
plot(ecdf(pits), main = "Empirical CDF of PIT Values - Test Data")

# One could argue that there appears to be a slight "U" shape to the histogram, thus sugessing too narrow of a predeiction distribution
