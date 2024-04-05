### Aufgabe 3
library(MASS)

# a)
x_data <- c(1, 0, 0, 1,
            1, 1, 4, 1,
            1, 2, 3, 1,
            1, 3, 1, 0,
            1, 4, 0, 0,
            1, 0, 0, 0,
            1, 3, 2, 0,
            1, 1, 1, 0)
y_data <- c(19, 17, 31, 30, 16, 16, 22, 21)

x_matrix <- matrix(x_data, nrow = 8, byrow = TRUE, ncol = 4)
y_matrix <- matrix(y_data, nrow = 8, byrow = TRUE, ncol = 1)

xtx <- t(x_matrix) %*% x_matrix
xtx_inv <- ginv(xtx)

beta <- xtx_inv %*% t(x_matrix) %*% y_matrix
linear_equation <- paste0("y = ", paste0(beta, " * x", 1:ncol(x_matrix), collapse = " + ")) # nolint
print(linear_equation)

# b)
new_data <- matrix(c(1, 2, 1, 1), nrow = 1) 
predicted_yield <- new_data %*% beta
print(predicted_yield)


# c)
data_frame <- data.frame(y = y_data, 
                         Intercept = rep(1, 8), 
                         DüngerA = c(0, 1, 2, 3, 4, 0, 3, 1), 
                         DüngerB = c(0, 4, 3, 1, 0, 0, 2, 1), 
                         Boden = c(1, 1, 1, 0, 0, 0, 0, 0))

model <- lm(y ~ DüngerA + DüngerB + Boden, data = data_frame)
new_data <- data.frame(DüngerA = 2, DüngerB = 1, Boden = 1)
predicted_yield_lm <- predict(model, newdata = new_data)
print(predicted_yield_lm)
