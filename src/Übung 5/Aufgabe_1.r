library(ggplot2)
library(dplyr)
library(gridExtra)

### csv Daten einlesen
path <- "/Users/tom/Documents/AWI Msc./2. Semester/Mehrdim. Datenanalyse und stat. Lernverfahren/Code/Daten/Concrete_Data.csv" #nolint
data <- read.csv(path, header = TRUE, sep = ",")
data <- as.data.frame(data)

# a)
pairs(data)

# b)
model <- lm(csMPa ~ ., data = data)

data <- data %>%
  mutate(
    std_resid = rstandard(model),
    predicted_csMPa = predict(model)
  )

plot_list <- lapply(names(data)[1:8], function(variable) {
  ggplot(data, aes(x = .data[[variable]], y = std_resid)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red")
})

plot_list[[length(plot_list) + 1]] <- ggplot(data, aes(x = predicted_csMPa, y = std_resid)) + # nolint
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

grid.arrange(grobs = plot_list, ncol = 3)

# c)
ggplot(data, aes(x = 1:nrow(data), y = std_resid)) +
  geom_point(size = 1) +  # Adjust the size of the points to make them smaller
  geom_line() +           # Add this line to connect the points
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Index", y = "Standardisierte Residuen")


# d) ### Konditionszahl = 238,26, Variance Inflation Factor = 7.48, 7.27 ...