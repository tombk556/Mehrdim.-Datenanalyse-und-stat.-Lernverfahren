library(dplyr)
library(epitools)

### Aufgabe 3

# a)
f <- "/Users/tom/Documents/AWI Msc./2. Semester/Mehrdim. Datenanalyse und stat. Lernverfahren/Code/Daten/whitewines.csv" # nolint
wine <- read.csv(f, header = TRUE, sep = ",")

# b)
head(wine)
tail(wine)
wine[c(1, 4, 5), 1:4]
wine[c(1, 4, 5), -c(3, 4, 7, 8)]

# c)
filtered_wine <- wine %>% filter(alcohol > 10)
print(nrow(filtered_wine))

# d)
filtered_wine <- wine %>% filter(alcohol > 10 & quality < 5)
head(filtered_wine)
print(nrow(filtered_wine))

# e)
print(which.max(filtered_wine$pH))

# f)
print(mean(wine$alcohol))
print(sd(wine$alcohol))
print(var(wine$alcohol))

# g) -> 5
table(wine$quality)

# h)
wine <- wine %>%
  mutate(strong = alcohol > 10,
         tasty = quality > 5)

cross_table <- table(wine$strong, wine$tasty)

odds_ratio <- oddsratio(cross_table)

print(odds_ratio)