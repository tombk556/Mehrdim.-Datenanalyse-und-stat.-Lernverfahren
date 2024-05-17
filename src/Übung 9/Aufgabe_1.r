library(MASS)
library(pROC)


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


# d) Kreuzvalidierung
n <- nrow(data)
n <- 4

# Initialisierung der Konfusionsmatrix
confusion_matrix <- matrix(0, nrow = 2, ncol = 2)
rownames(confusion_matrix) <- colnames(confusion_matrix) <- c("Predicted_Red", "Predicted_White") # nolint

# LOOCV durchführen
for (i in 1:n) {
  train_data <- data[-i, ]
  test_data <- data[i, ]
  model_loocv <- glm(type ~ residual_sugar + alcohol, data = train_data,
                     family = binomial(link = "logit"))

  model_reduced_loocv <- stepAIC(model_loocv,
                                 direction = "backward", 
                                 trace = 0)

  prediction_prob <- predict(model_reduced_loocv,
                             newdata = test_data, 
                             type = "response")
  prediction <- ifelse(prediction_prob >= 0.5, 1, 0)

  actual <- test_data$type
  if (actual == 0 && prediction == 0) {
    confusion_matrix["Predicted_Red", "Predicted_Red"] <- confusion_matrix["Predicted_Red", "Predicted_Red"] + 1 # nolint
  } else if (actual == 0 && prediction == 1) {
    confusion_matrix["Predicted_White", "Predicted_Red"] <- confusion_matrix["Predicted_White", "Predicted_Red"] + 1 # nolint
  } else if (actual == 1 && prediction == 0) {
    confusion_matrix["Predicted_Red", "Predicted_White"] <- confusion_matrix["Predicted_Red", "Predicted_White"] + 1 # nolint
  } else if (actual == 1 && prediction == 1) {
    confusion_matrix["Predicted_White", "Predicted_White"] <- confusion_matrix["Predicted_White", "Predicted_White"] + 1 # nolint
  }
}

# Konfusionsmatrix anzeigen
print(confusion_matrix)

# Anzahl der falsch klassifizierten Weißweine
false_white <- confusion_matrix["Predicted_Red", "Predicted_White"]
cat("Anzahl der falsch klassifizierten Weißweine: ", false_white, "\n")

# f) ROC-Kurve für reduced_model
prediction_prob <- predict(model_reduced, newdata = data, type = "response")
roc <- roc(data$type, prediction_prob)
plot(roc, main = "ROC-Kurve für reduced_model", col = "blue")
# ROC und AUC Wert
cat("AUC Wert: ", auc(roc), "\n")
