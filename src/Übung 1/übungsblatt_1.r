### Aufgabe 1

# a)
print(-20 * 15 + 5 * 74)

# b)
var1 <- -20 * 15 + 5 * 74

# c)
print(sqrt(var1))

# d)
var2 <- var1^(1 / 5)
print(var2)

# e)
print(var2 < pi)

# f)
var3 <- (exp(5) - pi^2) / log(10)
print(var3)

# g)
print(as.integer(var3))



### Aufgabe 2

# a)
xi <- c(2, -3, 5, 7, 8, 6, 5, 6, 9, 12)
yi <- c(4, 7, 6, 9, 14, 17, 23, 20, 17, 30)

# b)
print(mean(xi))
print(var(xi))

# c)
print(cov(xi, yi))

# d)
print(cor(xi, yi))


### Aufgabe 3

# a)
x1 <- c(-2, 4, 0.5, -4, 1, 2, -0.1, 5, 0.2, -0.4)

# b)
x2 <- 1 / rep(x1, length.out = length(1:11)) + 1:11
print(x2)

# c)
x3 <- sort(x2, decreasing = TRUE)
print(x3)

# d)
x4 <- seq(-2, 15, length.out = 11)
print(x4)

# e)
x5 <- pmin(x1, x2, x3, x4)
print(x5)


### Aufgabe 4

# a)
x1 <- rnorm(100, mean = 4, sd = 4)

# b)
print(summary(x1))

# c)
print(table(x1 < 0))

# d)
hist(x1, freq = FALSE, main = "Histogram")
curve(dnorm(x, mean = mean(x1), sd = sd(x1)), add = TRUE, col = "red")
