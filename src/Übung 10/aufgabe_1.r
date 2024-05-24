# Internen R Datensatz einlesen
data <- data(warpbreaks)

# a) Poission-Regressionsmodell
model <- glm(breaks ~ wool + tension, data = warpbreaks, family = poisson) # nolint
# Model: lambda(i) = exp(3.69 -0.21*xi1 -0.32*xi2 - 0.52*xi3) nolint

# b) Erwartete Anzahl an Brüchen für Wolle A und Spannung L
newdata <- data.frame(wool = "A", tension = "L")
expected_breaks <- predict(model, newdata, type = "response")
cat("Erwartete Anzahl an Brüchen für Wolle A und Spannung L: ", expected_breaks, "\n") # nolint

# Wahrscheinlichkeit, dass mehr als 30 Garnrisse auftreten
prob_more_than_30 <- 1 - ppois(30, lambda = expected_breaks)
cat("Wahrscheinlichkeit, dass mehr als 30 Garnrisse auftreten: ", prob_more_than_30, "\n") # nolint

# c) Koeffizienten ansehen
summary(model)$coefficients
# -0.20599 -> exp(-0.20599) = 0.814 ...  18.6% weniger Garnrisse bei Wolle vom Typ B als bei Wolle vom Typ A # nolint

# d)
dispersion_index <- sum(residuals(model, type = "pearson")^2) / df.residual(model) # nolint
cat("Dispersion Index: ", dispersion_index, "\n") # nolint
