library(ggplot2)

### Aufgabe 1

# Schritt 1: Daten laden
path <- "/Users/tom/Documents/AWI Msc./2. Semester/Mehrdim. Datenanalyse und stat. Lernverfahren/Code/Daten/data-pcw-const.csv" # nolint
daten <- read.csv(path, header = TRUE, sep = ",")

# Schritt 2: Kategoriale Variable für die Intervalle erstellen
daten$Intervall <- cut(daten$x,
    breaks = c(0, 10, 20, 40, 50), right = FALSE,
    labels = c("[0,10)", "[10,20)", "[20,40)", "[40,50]")
)

# Schritt 3: Lineares Modell anpassen
modell <- lm(y ~ Intervall, data = daten)

# Schritt 4: Modellzusammenfassung und Interpretation der Koeffizienten
summary(modell)


### Aufgabe 2
# a)
# Schritt 1: Daten laden
path <- "/Users/tom/Documents/AWI Msc./2. Semester/Mehrdim. Datenanalyse und stat. Lernverfahren/Code/Daten/sin-data.csv" # nolint
daten <- read.csv(path, header = TRUE, sep = ",")

# Schritte 3: Daten visualisieren
plot(daten$x, daten$y, main = "sin-data", xlab = "x", ylab = "y")

# Schritt 3: Lineares Modell erstellen
modell <- lm(y ~ x, data = daten)

# Schritt 4: Modell in Plot einfügen
abline(modell, col = "red")

# b)
f <- function(x) {
    5 * sin(x) - 2 * sin(4 * x) + 15 * exp(-0.1 * x)
}

# Schritt 5: Funktion in Plot einfügen
curve(f, from = 0, to = 20, add = TRUE, col = "blue")


### Aufgabe 3
# Schritt 1: Daten laden
path_train <- "/Users/tom/Documents/AWI Msc./2. Semester/Mehrdim. Datenanalyse und stat. Lernverfahren/Code/Daten/poly-train.csv" # nolint
path_test <- "/Users/tom/Documents/AWI Msc./2. Semester/Mehrdim. Datenanalyse und stat. Lernverfahren/Code/Daten/poly-test.csv" # nolint
train_data <- read.csv(path_train, header = TRUE, sep = ",")
test_data <- read.csv(path_test, header = TRUE, sep = ",")
errors_train <- numeric(8)
errors_test <- numeric(8)

# Schritt 2: Modelle anpassen
plot_data <- data.frame(x = train_data$x, y = train_data$y, Model = "Data")

for (d in 1:8) {
    formula <- as.formula(paste("y ~ poly(x, ", d, ", raw=TRUE)", sep = ""))
    model <- lm(formula, data = train_data)
    train_predictions <- predict(model, train_data)
    errors_train[d] <- mean((train_predictions - train_data$y)^2)
    test_predictions <- predict(model, test_data)
    errors_test[d] <- mean((test_predictions - test_data$y)^2)
    model_data <- data.frame(x = train_data$x, y = train_predictions, Model = paste("Degree", d)) # nolint
    plot_data <- rbind(plot_data, model_data)
}

ggplot(plot_data, aes(x, y, color = Model)) +
    geom_point(data = subset(plot_data, Model == "Data")) +
    geom_line() +
    theme_minimal()

error_data <- data.frame(Degree = 1:8, Training = errors_train, Testing = errors_test) # nolint
ggplot(error_data, aes(x = Degree)) +
    geom_line(aes(y = Training, color = "Training")) +
    geom_line(aes(y = Testing, color = "Testing")) +
    theme_minimal() +
    labs(y = "Mean Squared Error")

print(error_data)
