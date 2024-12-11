set.seed(123)
data <- read.table('uscrime.txt', header = TRUE)
# Build the initial full model
full_model <- lm(Crime ~ ., data = data)
summary(full_model)

# Apply stepwise regression
stepwise_model <- step(full_model, direction = "both")

# View the summary of the final model
summary(stepwise_model)

#stepwise model without non significant values
final <- lm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, data = data)
summary(final)
