### Aufgabe 1

path <- "/Users/tom/Documents/AWI Msc./2. Semester/Mehrdim. Datenanalyse und stat. Lernverfahren/Code/Daten/golf.csv" # nolint
daten <- read.csv(path, header = TRUE, sep = ",")


# a) Scatterplot
plot(daten$preis, daten$alter,
    xlab = "Preis",
    ylab = "Alter", main = "Scatterplot Preis vs. Alter"
)

plot(daten$preis, daten$km,
    xlab = "Preis",
    ylab = "km", main = "Scatterplot Preis vs. Km"
)

plot(daten$preis, daten$tuev,
    xlab = "Tüv",
    ylab = "Tüv", main = "Scatterplot Preis vs. Tüv"
)

plot(daten$preis, daten$abs,
    xlab = "Preis",
    ylab = "abs", main = "Scatterplot Preis vs. Abs"
)

plot(daten$preis, daten$sdach,
    xlab = "Preis",
    ylab = "sdach", main = "Scatterplot Preis vs. sdach"
)


# b) multivariate Lineare Regression
### Cp
model <- lm(preis ~ alter + km + tuev + abs + sdach, data = daten)

rss <- sum(resid(model)^2)

s_squared <- sum(resid(model)^2) / model$df.residual

n <- length(daten$preis)

p <- length(coef(model))

cp <- rss / s_squared - n + 2 * p

cat("Cp: ", cp, "\n")

### AIC
aic <- n * log(rss / n) + 2 * (p + 1)
cat("AIC: ", aic, "\n")

### BIC
bic <- n * log(rss / n) + log(n) * (p + 1)
cat("BIC: ", bic, "\n")


vergleich <- function(model) {
    rss <- sum(resid(model)^2)
    s_squared <- sum(resid(model)^2) / model$df.residual
    n <- length(daten$preis)
    p <- length(coef(model))
    cp <- rss / s_squared - n + 2 * p
    aic <- n * log(rss / n) + 2 * (p + 1)
    bic <- n * log(rss / n) + log(n) * (p + 1)
    return(c(cp, aic, bic))
}

# c)
basis_vars <- "alter + km"

zusatz_vars <- c("tuev", "abs", "sdach")

kombinationen <- expand.grid(tuev = 0:1, abs = 0:1, sdach = 0:1)

for (i in 1:nrow(kombinationen)) { # nolint
    akt_kombi <- kombinationen[i, ]
    formel <- paste("preis ~", basis_vars, ifelse(akt_kombi$tuev, "+ tuev", ""), ifelse(akt_kombi$abs, "+ abs", ""), ifelse(akt_kombi$sdach, "+ sdach", "")) # nolint
    model <- lm(as.formula(formel), data = daten)
    cat(formel, "Metriken: ", vergleich(model), "\n")
}

best_model <- lm(preis ~ alter + km + abs, data = daten)
new_data <- data.frame(alter = 62, km = 96, abs = 1)
print(predict(best_model, newdata = new_data) * 1000)