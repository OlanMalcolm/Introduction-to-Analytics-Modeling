set.seed(123)
data <- read.table('uscrime.txt', header = TRUE)
library(glmnet)

# 2. Prepare predictors (X) and response (y)
x <- as.matrix(data[, -ncol(data)])  # Exclude the response variable (Crime)
y <- data$Crime

# 3. Scale the predictors and store scaling parameters
x_scaled <- scale(x)
x_means <- attr(x_scaled, "scaled:center")  # Store column means
x_sds <- attr(x_scaled, "scaled:scale")     # Store column standard deviations

# 4. Fit the Elastic Net model with alpha = 0.5 (50% Lasso, 50% Ridge)
elastic_net_model <- glmnet(x_scaled, y, family = "gaussian", alpha = 0.3)

# 5. Cross-validate to find the optimal lambda (penalty)
cv_elastic_net <- cv.glmnet(x_scaled, y, alpha = 0.3)

# 6. Extract the best lambda value
best_lambda_elastic <- cv_elastic_net$lambda.min

# 7. Get the coefficients from the model at the optimal lambda
scaled_coefs <- coef(cv_elastic_net, s = best_lambda_elastic)

# 8. Unscale the coefficients
unscaled_coefs <- scaled_coefs[-1] / x_sds  # Exclude intercept, divide by SD
intercept <- as.numeric(scaled_coefs[1]) - sum(unscaled_coefs * x_means)

# 9. Display the final unscaled coefficients and intercept
print(intercept)
for (i in 1:length(unscaled_coefs)) {
  cat(" +", round(unscaled_coefs[i], 2), "*", colnames(data)[i], "\n")
}
