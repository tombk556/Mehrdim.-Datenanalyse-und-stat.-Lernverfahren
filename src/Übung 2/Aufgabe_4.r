### Aufgabe 4

# a)
f <- "/Users/tom/Documents/AWI Msc./2. Semester/Mehrdim. Datenanalyse und stat. Lernverfahren/Code/Daten/insurance.csv" # nolint
raw_data <- read.csv(f, header = TRUE, sep = ",")
insurance <- data.frame(raw_data)

# b)
summary(insurance$age)
summary(insurance)

# c) Boxplot age
boxplot(insurance$age)

# c) Boxplot charges by age groups
boxplot(insurance$charges ~ cut(insurance$age, breaks = c(0, 35, 60, Inf)),
    xlab = "Alters Gruppe",
    ylab = "Charges",
    main = "Boxplot by Alters Gruppe"
)

# e) Histogram by charges
hist(insurance$charges, xlab = "Charges", ylab = "Frequency", main = "Histogram by Charges") # nolint

# f)
table(insurance$region)
table(insurance$region, insurance$smoker)

# g) Plot age by charges filtered by sex
plot(insurance$age ~ insurance$charges,
    plot(insurance$age ~ insurance$charges,
        col = ifelse(insurance$sex == "female", "pink", "blue"),
        xlab = "Charges", ylab = "Age", main = "Age by Charges filtered")
)
