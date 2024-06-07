library(ggbiplot)

# Daten laden und vorbereiten
path <- "/Users/tom/Documents/AWI Msc./2. Semester/Mehrdim. Datenanalyse und stat. Lernverfahren/Code/Daten/cars.csv" # nolint
data <- read.csv(path, sep = " ")

ct <- data$ct
df <- data[, -which(names(data) == "ct")]


# a) PCA
cars.pca <- prcomp(df, scale = TRUE) # nolint

# Output:
#                           PC1    PC2     PC3     PC4     PC5     PC6     PC7
# Standard deviation     2.3782 1.4429 0.71008 0.51481 0.42797 0.35184 0.32413
# Proportion of Variance 0.6284 0.2313 0.05602 0.02945 0.02035 0.01375 0.01167
# Cumulative Proportion  0.6284 0.8598 0.91581 0.94525 0.96560 0.97936 0.99103
#                           PC8     PC9
# Standard deviation     0.2419 0.14896
# Proportion of Variance 0.0065 0.00247
# Cumulative Proportion  0.9975 1.00000

# Standard Deviation: square root der Eigenwerte der Kovarianzmatrix # nolint
# Proportion of Variance: Anteil der Varianz, der durch die jeweilige Hauptkomponente erklärt wird # nolint

# Eigenwerte -> Varianz der Hauptkomponenten ^2 -> PC1: 5.65, PC2: 2.08, PC3: 0.51, PC4: 0.27, PC5: 0.17, PC6: 0.10, PC7: 0.09, PC8: 0.06, PC9: 0.02 # nolint

eigenvalues <- cars.pca$sdev^2
cat("Eigenvalues: ", eigenvalues, "\n")

# Extracting the eigenvectors (loadings)
eigenvectors <- cars.pca$rotation
cat("Eigenvectors: \n")
print(eigenvectors)


# b)
df_centered <- scale(df, center = TRUE, scale = TRUE)

X <- t(df_centered) # nolint

# Berechnen der Kovarianzmatrix
n <- nrow(df)
cov_matrix <- (X %*% t(X)) / (n - 1)

# Berechnen der Eigenwerte und Eigenvektoren
eigen_result <- eigen(cov_matrix)
eigenvalues <- eigen_result$values
eigenvectors <- eigen_result$vectors

# Ausgabe der Ergebnisse
print("Eigenvalues:")
print(eigenvalues)

print("Eigenvectors:")
print(eigenvectors)

# Die Kovarianzmatrix ist eine Matrix, die die Kovarianzen zwischen den verschiedenen Variablen eines Datensatzes darstellt. # nolint

# c)
ggbiplot(cars.pca, labels = rownames(mtcars), groups = ct, ellipse = TRUE)
