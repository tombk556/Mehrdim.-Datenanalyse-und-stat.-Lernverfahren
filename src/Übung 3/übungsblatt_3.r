### Load the data set "cars" into R
data(cars)
data <- data.frame(cars)

# a)
summary(data)
plot(data$speed, data$dist, main="Speed vs. Distance", xlab="Speed", ylab="Distance", pch=19) # nolint

# b) mean, variance, standard deviation, correlation, covariance
x <- data$speed
y <- data$dist

# mean
mean(x)
mean(y)

# variance
var(x)
var(y)

# covariance & correlation # nolint
cov(x, y)
cor(x, y)

# c) linear regression
# (i) alpha and beta
beta <- cov(x, y) / var(x)
alpha <- mean(y) - beta * mean(x)
print("alpha and beta formulary approach")
cat("alpha: ", alpha, "\n")
cat("beta: ", beta, "\n")

# (ii) design matrix
X <- cbind(1, x) # nolint
coefficients <- solve(t(X) %*% X) %*% t(X) %*% y
print("design matrix approach")
cat("alpha: ", coefficients[1], "\n")
cat("beta: ", coefficients[2], "\n")

# (iii) R build in Function
lm(y ~ x)
print("LRM - R build in function")
cat("alpha: ", lm(y ~ x)$coefficients[1], "\n")
cat("beta: ", lm(y ~ x)$coefficients[2], "\n")

# plot the linear regression model and the x and y
plot(x, y, main="Linear Regression Model", xlab="Speed", ylab="Distance", pch=19) # nolint
abline(lm(y ~ x), col="red") # nolint


# d) residuals variance
sum <- 0
for (i in seq_along(x)) {
  sum <- sum + (y[i] - alpha - beta * x[i])^2
}
residual_var <- sum / (length(x) - 2)
print(residual_var)

# e) confidence intervals e=0.05
print("Confidence Intervals")
t_value <- qt(0.975, df = length(x) - 2)

# alpha
s_alpha_2 <- ((1 / length(x)) + (mean(x)^2 / ((length(x) - 1) * var(x)))) * residual_var  # nolint

alpha_0 <- (alpha - t_value * sqrt(s_alpha_2))
alpha_1 <- (alpha + t_value * sqrt(s_alpha_2))
cat("alpha: [", alpha_0, " ; ", alpha_1, "]\n")

# beta
s_beta_2 <- residual_var / ((length(x) - 1) * var(x)) # nolint

beta_0 <- (beta - t_value * sqrt(s_beta_2))
beta_1 <- (beta + t_value * sqrt(s_beta_2))
cat("beta: [", beta_0, " ; ", beta_1, "]\n")

# sigma
sigma <- sqrt(residual_var)
chi_quantile0 <- qchisq(0.975, df = length(x) - 2)
chi_quantile1 <- qchisq(0.025, df = length(x) - 2)

sigma_0 <- (length(x) - 2) * residual_var / chi_quantile0
sigma_1 <- (length(x) - 2) * residual_var / chi_quantile1
cat("sigma: [", sigma_0, " ; ", sigma_1, "]\n")

# f) SSE, SSR, SST
# SSE
SSE <- sum((y - alpha - beta * x)^2) # nolint
cat("SSE:", SSE, "\n")

# SST
SST <- sum(y^2) # nolint
cat("SST:", SST, "\n")

# SSR
SSR <- SST - SSE # nolint
cat("SSR:", SSR, "\n")

# g)
# i)
lm_model <- lm(y ~ x)
standardized_residuals <- rstandard(lm_model)

# ii)
# Plot standardized residuals
plot(fitted(lm_model), standardized_residuals, xlab = "Fitted Values", ylab = "Standardized Residuals", #nolint
     main = "Standardized Residuals vs. Fitted Values")
abline(h = 0, col = "red")

# iii)
# Histogram of standardized residuals
hist(standardized_residuals, main = "Histogram of Standardized Residuals", xlab = "Standardized Residuals") #nolint

# iv)
# Q-Q plot of standardized residuals
qqnorm(standardized_residuals, main = "Q-Q Plot of Standardized Residuals")
qqline(standardized_residuals, col = "red")


# Kolmogorov-Smirnov test
unique_residuals <- unique(standardized_residuals)
ks_result <- ks.test(unique_residuals, "pnorm", mean(unique_residuals), sd(unique_residuals)) # nolint
print(ks_result)
