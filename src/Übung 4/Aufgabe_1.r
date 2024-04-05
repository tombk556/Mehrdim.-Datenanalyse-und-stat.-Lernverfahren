# library(RColorBrewer)

# ### Aufgabe 1

# # a)
# alpha <- 2
# beta <- 3
# sigma <- 8
# x <- seq(1, 100)

# # b)
# u <- rnorm(100, mean = 0, sd = sigma)

# # c)
# y <- alpha + beta * x + u

# # d)
# plot(x, y, main = "Scatter Plot", xlab = "x", ylab = "y")

# # e)
# model <- lm(y ~ x)

# abline(model, col = "red")
# coefficients <- coef(model)

# intercept <- coefficients[1]
# slope <- coefficients[2]

# cat("Linear Equation: y =", intercept, "+", slope, "* x \n")

# # f)
# alpha <- 2
# beta <- 3
# sigma <- 10

# plot(x = NULL, y = NULL, xlim = c(1, 100),
#      ylim = c(min(y), max(y)), main = "Linear Regression Models",
#      xlab = "x", ylab = "y")


# colors <- brewer.pal(20, "Set1")

# for (i in 1:20) {
#   u <- rnorm(1, mean = 0, sd = sigma)
#   x <- seq(1, 100)
#   y <- alpha + beta * x + u
#   model <- lm(y ~ x)
#   abline(model, col = colors[i])
# }

# g)
samples <- 50
alpha <- 2
beta <- 3
sigma <- 8
x <- seq(1, samples)
u <- rnorm(samples, mean = 0, sd = sigma)
y <- alpha + beta * x + u
model <- lm(y ~ x)
plot(x, y, main = "2nd Scatter Plot", xlab = "x", ylab = "y")
abline(model, col = "red")

# add the confidence interval and prediction interval
conf_int <- predict(model, interval = "confidence", level = 0.90)
pred_int <- predict(model, interval = "prediction", level = 0.90)

# plot the confidence interval
lines(x, conf_int[, 2], col = "blue", lty = 2)
lines(x, conf_int[, 3], col = "blue", lty = 2)

# plot the prediction interval
lines(x, pred_int[, 2], col = "green", lty = 2)
lines(x, pred_int[, 3], col = "green", lty = 2)

# h)
alpha <- 1.5
beta <- 0.5
sigma2 <- 2
sigma <- sqrt(sigma2)
x <- seq(from = 1, to = 100, by = 1)
u <- rnorm(n = 100, mean = 0, sd = sigma)
y <- alpha + beta * x + u
x0 <- 50 # Beispielwert

# Prognoseintervall für y an x0
predicted_y <- alpha + beta * x0
se <- sigma * sqrt(1 + 1 / length(x))
predict_interval <- predicted_y + c(-1, 1) * qt(0.95, df = length(x) - 2) * se

# Simulieren von neuen y-Werten
n_simulations <- 1000
new_y <- alpha + beta * x0 + rnorm(n_simulations, mean = 0, sd = sigma)
inside_interval <- sum(new_y >= predict_interval[1] & new_y <= predict_interval[2]) #nolint
inside_proportion <- inside_interval / n_simulations

print(paste("Anteil der Werte im Prognoseintervall:", inside_proportion))


# i)
alpha <- 2
beta <- 0.5
sigma_squared <- 4
sigma <- sqrt(sigma_squared)
n <- 100
x0 <- 5
iterations <- 200
contained_in_CI <- 0 # nolint
x <- seq(0, 10, length.out = n)

for (i in 1:iterations) {
  u <- rnorm(n, mean = 0, sd = sigma)
  y <- alpha + beta * x + u
  y_mean <- mean(y)
  y_se <- sigma / sqrt(n)
  t_value <- qt(0.90, df = n-1) 
  CI <- c(y_mean - t_value * y_se, y_mean + t_value * y_se) # nolint
  y0 <- alpha + beta * x0
  if (y0 >= CI[1] && y0 <= CI[2]) {
    contained_in_CI <- contained_in_CI + 1 #nolint
  }
}
percent_contained <- (contained_in_CI / iterations)
print(paste("Prozentsatz der Konfidenzintervalle, die y0 enthalten:", percent_contained)) #nolint


# j)
modell <- lm(y ~ x)

# Zusammenfassung des Modells, um den p-Wert zu erhalten
modell_zusammenfassung <- summary(modell)

# p-Wert für β extrahieren
p_wert_beta <- modell_zusammenfassung$coefficients["x", "Pr(>|t|)"]

# Geschätzten Wert für β extrahieren
beta_geschaetzt <- modell_zusammenfassung$coefficients["x", "Estimate"]

# Ausgabe des geschätzten β und des p-Werts
cat("Geschätztes Beta:", beta_geschaetzt, "\n")
cat("p-Wert für den Test, dass Beta = 0.5 ist:", p_wert_beta, "\n")
if(p_wert_beta < 0.05) {
  cat("H0 wird verworfen: Beta ist signifikant verschieden von 0.5.\n")
} else {
  cat("H0 kann nicht verworfen werden: Keine signifikante Abweichung von Beta = 0.5.\n")
}
