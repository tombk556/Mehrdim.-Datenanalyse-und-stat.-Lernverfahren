library("expm")

### Aufgabe 2
# a)
a_func <- function(n, a0) {
    a <- a0
    for (i in 1:n) {
        a <- 4 * a^0.5 + a
    }
    return(a)
}

print(a_func(8, 6.5))

# b)
# fib function
fib <- function(n) {
    if (n == 0) {
        return(0)
    } else if (n == 1) {
        return(1)
    } else {
        return(fib(n - 1) + fib(n - 2))
    }
}

fib_run <- function(n) {
    for (i in 0:n) {
        print(fib(i))
    }
}

fib_run(6)

# c)
max_element_and_position <- function(M, k) {
  M_k = M %^% k
  
  max_element = max(M_k)
  position = which(M_k == max_element, arr.ind = TRUE)
  
  list("Max Element" = max_element, "Position" = position)
}

m <- matrix(c(1, -2, 2, -1), nrow = 2, ncol = 2, byrow = TRUE)
print(max_element_and_position(m, 5))