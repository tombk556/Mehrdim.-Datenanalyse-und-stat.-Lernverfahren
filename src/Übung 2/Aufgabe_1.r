library("expm")

### Aufgabe 1

a_data <- c(2, -3, 4, -3, 0.5, 1, 4, 1, -2)
a_matrix <- matrix(a_data, nrow = 3, ncol = 3, byrow = TRUE)

b_data <- c(8, 2, -2, 3, 1, -3, 4, -1, 12)
b_matrix <- matrix(b_data, nrow = 3, ncol = 3, byrow = TRUE)

c_data <- c(5, -3, 4, 3, -8, 3, -5, 1, 4, 3, 2, -3)
c_matrix <- matrix(c_data, nrow = 3, ncol = 4, byrow = TRUE)

# a)
# A^T  # nolint
# print(t(a_matrix)) # nolint

# A + B^T # nolint
# print(a_matrix + t(b_matrix)) # nolint

# (A + B^T)*C # nolint
print((a_matrix + t(b_matrix)) %*% c_matrix)

# A^7 # nolint
a_matrix_power <- a_matrix %^% 7
print(a_matrix_power)

# b)
ctc <- t(c_matrix) %*% c_matrix
cct <- c_matrix %*% t(c_matrix)

# ctc
print(dim(ctc))
print(qr(ctc)$rank)
print(det(ctc))

# cct
print(dim(cct))
print(qr(cct)$rank)
print(det(cct))

# c)
# check if B is invertible
print(round(solve(b_matrix) %*% b_matrix), 1)

# B^(-1) # nolint
b_matrix_inv <- solve(b_matrix)
print(b_matrix_inv)

# d)
# Y = (100, 200, 300)^T # nolint
# C(C^T*A -3*C^T*B)*x = y # nolint
y <- matrix(c(100, 200, 300), nrow = 3, ncol = 1)

x <- solve(cct %*% a_matrix - 3 * cct %*% b_matrix, y)
x <- round(t(x), 2)
print(x)

# f)
# Eigenwerte von A # nolint
print(eigen(a_matrix)$values)

