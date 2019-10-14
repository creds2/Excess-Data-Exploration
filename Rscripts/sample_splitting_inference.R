# generate some random data -----------------------------------------------
set.seed(1)
n <- 30000
p <- 5
X <- matrix(rnorm(n * p), nrow = n, ncol = p)

# I generate a Y variabile which is a linear combination of these 5 variables
Y <- 2 + rowSums(X * matrix(c(1, 2, -1, -2, 3), nrow = n, ncol = p, byrow = TRUE)) + rnorm(n)

# obviously the lm - estimates are super good
coefficients(lm(Y ~ ., data = data.frame(X)))

# I add to the X-variables some other variables related with them (like room
# number and bathroom number) that, actually, you'd like to exclude. 
X1_group <- X[, 1] * matrix(1:10, nrow = n, ncol = 10, byrow = TRUE) + matrix(rnorm(n * 10), ncol = 10)
X2_group <- X[, 2] * matrix(1:2, nrow = n, ncol = 2, byrow = TRUE) + matrix(rnorm(n * 2), ncol = 2)
X3_group <- X[, 3] * matrix(1:3, nrow = n, ncol = 3, byrow = TRUE) + matrix(rnorm(n * 3), ncol = 3)
X4_group <- X[, 4] * matrix(1:15, nrow = n, ncol = 15, byrow = TRUE) + matrix(rnorm(n * 15), ncol = 15)
X5_group <- X[, 5] * matrix(1:8, nrow = n, ncol = 8, byrow = TRUE) + matrix(rnorm(n * 8), ncol = 8)
X <- cbind(X, X1_group, X2_group, X3_group, X4_group, X5_group)
dim(X)

# now the lm-estimates are slightly worse
coefficients(lm(Y ~ ., data = data.frame(X)))[1:6]

# And, to conclude, I add some variables that are pure noise
noise <- matrix(rnorm(n * 25), ncol = 25)
X <- cbind(X, noise)
dim(X)

# the estimates are even worse
coefficients(lm(Y ~ ., data = data.frame(X)))[1:6]

# but the big problem is that several noise variable are considered "significant"
which(summary(lm(Y ~ ., data = data.frame(X)))$coefficients[, 4] < 0.05)

# The effect can be much much worse if we increase the correlation between X1,
# ..., X5 and the other variables

# sample splitting inference ----------------------------------------------

# Split data
indexes <- sample(1:nrow(X), size = nrow(X) / 2)

# indentify important variable using only train data
# YOU SHOULD DO THIS USING THE TREE ALGORITHM
lm_model <- lm(Y ~ ., data = data.frame(X), subset = indexes) 
signif_variables <- names(which(summary(lm_model)$coefficients[, 4] < 0.05))

# Estimate coef of these "important" variables using the other part of data
estimate_coefs_formula <- paste0("Y ~ ", paste(setdiff(signif_variables, "(Intercept)"), collapse = " + "))
estimate_coefs <- lm(as.formula(estimate_coefs_formula), data = data.frame(X), subset = -indexes)
sample_split_coefs <- data.frame(matrix(coefficients(estimate_coefs), nrow = 1))
colnames(sample_split_coefs) <- names(coefficients(estimate_coefs))
res <- sample_split_coefs

# Repeat the same idea again and again
n_replicates <- 1000
for(i in seq_len(n_replicates)) {
  indexes <- sample(1:nrow(X), size = nrow(X) / 2)
  
  lm_model <- lm(Y ~ ., data = data.frame(X), subset = indexes) 
  signif_variables <- names(which(summary(lm_model)$coefficients[, 4] < 0.05))
  
  estimate_coefs_formula <- paste0("Y ~ ", paste(setdiff(signif_variables, "(Intercept)"), collapse = " + "))
  estimate_coefs <- lm(as.formula(estimate_coefs_formula), data = data.frame(X), subset = -indexes)
  
  sample_split_coefs <- data.frame(matrix(coefficients(estimate_coefs), nrow = 1))
  colnames(sample_split_coefs) <- names(coefficients(estimate_coefs))
  res <- dplyr::bind_rows (res, sample_split_coefs)
  
  if (!i %% 100) print(i)
}

# results
library(dplyr)

foo <- res %>% 
  summarize_all(function(x) sum(!is.na(x)))

