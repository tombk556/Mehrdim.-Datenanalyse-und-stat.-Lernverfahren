# Laden der Daten
path <- "/Users/tom/Documents/AWI Msc./2. Semester/Mehrdim. Datenanalyse und stat. Lernverfahren/Code/Daten/melb.csv" #nolint
data <- read.csv(path, header = TRUE, sep = ",")
data <- as.data.frame(data)

# a)
data_a <- data
model_a <- lm(Price ~ ., data = data_a)

#  Coefficients:
#                 Estimate nolint
#  (Intercept)   9.579e+06 nolint
#  Rooms         1.773e+05 nolint
#  Distance     -2.793e+04 nolint
#  Car           4.370e+04 nolint
#  Bathroom      2.087e+05 nolint
#  BuildingArea  1.652e+03 nolint
#  Landsize      1.180e+01 nolint
#  YearBuilt    -4.781e+03 nolint
#### Je mehr Autos desto teur das Haus: 43700 $ pro Auto


# b)
data_b <- data
data_b$Distance <- data_b$Distance^2
model_b <- lm(Price ~ ., data = data_b)

# Coefficients:
#                Estimate # nolint
# (Intercept)   1.039e+07 # nolint
# Rooms         1.514e+05 # nolint
# Distance     -6.611e+02 # nolint
# Car           3.088e+04 # nolint
# Bathroom      2.262e+05 # nolint
# BuildingArea  1.678e+03 # nolint
# Landsize      1.221e+01 # nolint
# YearBuilt    -5.259e+03 # nolint
### Je weier das Haus weg ist, desto billiger wird es



# c)
data_c <- data
data_c$Car <- as.integer(data_c$Car > 1)
data_c$Bathroom <- as.integer(data_c$Bathroom > 1)
model_c <- lm(Price ~ ., data = data_c)

# Coefficients:
#                Estimate #nolint
# (Intercept)   9.230e+06 #nolint
# Rooms         1.958e+05 #nolint
# Distance     -3.018e+04 #nolint
# Car           1.215e+05 #nolint
# Bathroom      1.903e+05 #nolint
# BuildingArea  1.838e+03 #nolint
# Landsize      1.312e+01 #nolint
# YearBuilt    -4.508e+03 #nolint