### Aufgabe 1

path <- "/Users/tom/Documents/AWI Msc./2. Semester/Mehrdim. Datenanalyse und stat. Lernverfahren/Code/Daten/insurance.csv" # nolint
daten <- read.csv(path, header = TRUE, sep = ",")


# a)
daten$sex <- ifelse(daten$sex == "male", 1, 0) # male -> 1, female -> 0
daten$smoker <- ifelse(daten$smoker == "yes", 1, 0) # yes -> 1, no -> 0


# b)
daten$region <- as.factor(daten$region)
model1 <- lm(charges ~ age + region, data = daten)
# summary(model1)


model2 <- lm(charges ~ age + region - 1, data = daten)
# summary(model2)

# c) Kreuzvalidierung
cross_validation <- function(data, formula, k) {
  n <- nrow(data)
  indices <- sample(1:n)
  folds <- split(indices, cut(indices, breaks = k, labels = FALSE))
  squared_errors <- numeric(k)

  for (i in 1:k) {
    train_indices <- unlist(folds[-i])
    train_data <- data[train_indices, ]
    test_indices <- folds[[i]]
    test_data <- data[test_indices, ]
    model <- lm(formula, data = train_data)
    predictions <- predict(model, newdata = test_data)
    squared_errors[i] <- sum((test_data$charges - predictions)^2)
  }

  cv <- sum(squared_errors) / n
  return(cv)
}

cv <- cross_validation(daten, formula = charges ~ ., k = 20)
print(cv)
