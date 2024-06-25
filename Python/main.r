### read csv
path <- "/Users/tom/Documents/AWI Msc./2. Semester/Mehrdim. Datenanalyse und stat. Lernverfahren/Code/Python/auto.csv" # nolint
auto.frame <- read.csv(path, header = TRUE, sep = ",")

### cov matrix
cov_matrix <- cov(auto.frame)

### correlation matrix
auto.cor <- cor(auto.frame)

### reduzierte korrelationsmatrix
auto.cor.redu <- auto.cor
rquadrat <- 1-(1/diag(solve(auto.cor)))
diag(auto.cor.redu) <- rquadrat

### wie viele Faktoren?
evals <- eigen(auto.cor.redu)$values
plot(evals, type = "b", xlab = "Faktor", ylab = "Eigenwert", main = "Scree-Plot")
evals;sign(evals)
sum(evals)


### faktorenanalyse
fa.auto <- factanal(covmat = auto.cor, factors = 2, rotation = "none")
fa.auto.lad <- fa.auto$loadings[,1:2]
print(fa.auto.lad)
