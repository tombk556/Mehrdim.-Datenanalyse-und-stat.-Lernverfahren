# Load the cars dataset
data(cars)

# Fit a linear model
lm_model <- lm(dist ~ speed, data = cars)

# Basic plot of the data
plot(cars$speed, cars$dist,
     main = "Linear Regression Model with Confidence and Prediction Intervals",
     xlab = "Speed", ylab = "Distance", pch = 19)

# Add the regression line
abline(lm_model, col = "red")

# Add confidence interval
conf_int <- predict(lm_model, interval = "confidence")
lines(cars$speed, conf_int[, 2], col = "blue")
lines(cars$speed, conf_int[, 3], col = "blue")

# Add prediction interval
pred_int <- predict(lm_model, interval = "prediction")
lines(cars$speed, pred_int[, 2], col = "green")
lines(cars$speed, pred_int[, 3], col = "green")

legend("topleft",
       legend = c("Data", "Regression Line",
                  "Confidence Interval", "Prediction Interval"),
       col = c("black", "red", "blue", "green"),
       lty = c(NA, 1, 1, 1), pch = c(19, NA, NA, NA), bty = "n")
