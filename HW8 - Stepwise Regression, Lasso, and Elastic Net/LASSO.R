set.seed(123)
data <- read.table('uscrime.txt', header = TRUE)

library(glmnet)

# 1. Prepare the predictors (X) and response (y)
x <- as.matrix(data[, -ncol(data)])  # Exclude the response variable (Crime)
y <- data$Crime

# 2. Scale the predictors (X) and store scaling parameters
x_scaled <- scale(x)
x_means <- attr(x_scaled, "scaled:center")  # Mean of each column
x_sds <- attr(x_scaled, "scaled:scale")     # Std dev of each column

# 3. Fit the Lasso model using scaled data
lasso_model <- glmnet(x_scaled, y, family = "gaussian", alpha = 1)

# 4. Cross-validate to find the optimal lambda (penalty)
cv_lasso <- cv.glmnet(x_scaled, y, alpha = 1)

# 5. Extract the best lambda value
best_lambda <- cv_lasso$lambda.min

# 6. Get the coefficients from the model at the optimal lambda
scaled_coefs <- coef(cv_lasso, s = best_lambda)

# 7. Unscale the coefficients to get them in the original units
unscaled_coefs <- scaled_coefs[-1] / x_sds  # Exclude intercept, divide by SD
intercept <- as.numeric(scaled_coefs[1]) - sum(unscaled_coefs * x_means)

# 8. Display the final unscaled coefficients and intercept
print(intercept)
for (i in 1:length(unscaled_coefs)) {
  cat(" +", round(unscaled_coefs[i], 2), "*", colnames(data)[i], "\n")
}

