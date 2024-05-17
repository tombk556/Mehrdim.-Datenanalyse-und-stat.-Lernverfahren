library(MASS)

### Aufgabe 1

# Daten laden und zusammenführen
path_rw <- "/Users/tom/Documents/AWI Msc./2. Semester/Mehrdim. Datenanalyse und stat. Lernverfahren/Code/Daten/redwines.csv" # nolint
path_ww <- "/Users/tom/Documents/AWI Msc./2. Semester/Mehrdim. Datenanalyse und stat. Lernverfahren/Code/Daten/whitewines.csv" # nolint

data_rw <- read.csv(path_rw, sep = ";")
data_ww <- read.csv(path_ww, sep = ",")

data_rw$type <- 0 # red wine
data_ww$type <- 1 # white wine

data <- rbind(data_rw, data_ww)

# a) Logitisches Regressionsmodell
model <- glm(type ~ ., data = data, family = binomial(link = "logit"))

# b) Wahrscheinlichkeit für Predictions
new_data <- data.frame(fixed_acidity = 8.1,
                       volatile_acidity = 0.28,
                       citric_acid = 0.40,
                       residual_sugar = 6.9,
                       chlorides = 0.050,
                       free_sulfur_dioxide = 30,
                       total_sulfur_dioxide = 97,
                       density = 0.9951,
                       pH = 3.26,
                       sulphates = 0.40,
                       alcohol = 9.9, quality = 6.0)

cat("Wahrscheinlichkeit: ", predict(model, new_data, type = "response"), "\n")

# c) Rückwärtsselektion mit AIC
model_reduced <- stepAIC(model, direction = "backward", trace = 0)

original_variables <- names(coef(model))

reduced_variables <- names(coef(model_reduced))

cat("Verbleibende Variablen im reduzierten Modell: ", reduced_variables, "\n")
