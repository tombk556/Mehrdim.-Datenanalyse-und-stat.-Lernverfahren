### Aufgabe 2
library(Matrix)
library(MASS)

x_data <- c(1, 0, 0, 1, 0,
            1, 1, 4, 1, 0,
            1, 2, 3, 1, 0,
            1, 3, 1, 0, 1,
            1, 4, 0, 0, 1,
            1, 0, 0, 0, 1,
            1, 3, 2, 0, 1,
            1, 1, 1, 0, 1)

y_data <- c(19, 17, 31, 30, 16, 16, 22, 21)

x_matrix <- matrix(x_data, nrow = 8, ncol = 5, byrow = TRUE)
y_matrix <- matrix(y_data, nrow = 8, ncol = 1, byrow = TRUE)

# a)
x_matrix_t <- t(x_matrix)
xtx <- x_matrix_t %*% x_matrix
rank <- qr(xtx)$rank

if (rank < ncol(xtx)) {
  print("Matrix ist singulär und daher nicht invertierbar.")
} else {
  print("Matrix ist invertierbar.")
}

xtx_inv_1 <- ginv(xtx)

# b)
beta <- xtx_inv_1 %*% x_matrix_t %*% y_matrix
print(beta)
linear_equation <- paste0("y = ", paste0(beta, " * x", 1:ncol(x_matrix), collapse = " + ")) # nolint
print(linear_equation)

# c)
sse <- sum((y_matrix - x_matrix %*% beta)^2)
print(sse)

# d)
model <- lm(y_matrix ~ x_matrix - 1)
coef <- coefficients(model)
print(coef)