set.seed(123)
library(tidyverse)
library(caret)
data <- read.table("breast-cancer-wisconsin.data.txt", 
                 header = FALSE, sep = ",", stringsAsFactors = FALSE, 
                 col.names = c("ID", "ClumpThickness", "CellSize", 
                               "CellShape", "Adhesion", "EpithelialSize", 
                               "BareNuclei", "BlandChromatin", 
                               "NormalNucleoli", "Mitoses", "Class"))

# Convert 'BareNuclei' to numeric, replacing '?' with NA
data$BareNuclei <- as.numeric(replace(data$BareNuclei, data$BareNuclei == "?", NA))

# Drop the 'ID' column (since it’s not needed for analysis)
data <- data %>% select(-ID)

# Inspect the data to confirm it’s loaded correctly
head(data)
summary(data)

# Mean imputation for numeric features (including BareNuclei)
data_mean_imputed <- data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Check the imputed dataset
summary(data_mean_imputed)

# Linear regression model to predict 'BareNuclei'
lm_model <- lm(BareNuclei ~ ., data = data, na.action = na.exclude)

# Predict the missing values
predicted_values <- predict(lm_model, newdata = data[is.na(data$BareNuclei), ])

# Impute the missing values
data_reg_imputed <- data
data_reg_imputed$BareNuclei[is.na(data$BareNuclei)] <- predicted_values

# Check the imputed dataset
summary(data_reg_imputed)

# Add noise to the predicted values
perturbation <- rnorm(length(predicted_values), mean = 0, 
                      sd = sd(data$BareNuclei, na.rm = TRUE) * 0.1)

data_reg_perturbed <- data_reg_imputed
data_reg_perturbed$BareNuclei[is.na(data$BareNuclei)] <- predicted_values + perturbation

# Check the imputed dataset
summary(data_reg_perturbed)

# Check for missing values
colSums(is.na(data))

# Option 1: Remove rows with missing values
data_clean <- na.omit(data)

# Use the imputed or cleaned data for training
final_data <- data_clean  

# Ensure the target variable 'Class' is a factor
final_data$Class <- factor(final_data$Class, levels = c(2, 4), labels = c("benign", "malignant"))

# Verify the structure of the data
str(final_data)

train_index <- createDataPartition(final_data$Class, p = 0.7, list = FALSE)

train_data <- final_data[train_index, ]
test_data <- final_data[-train_index, ]

# Train the SVM model with cross-validation
svm_model <- train(Class ~ ., data = train_data, method = "svmLinear", 
                   trControl = trainControl(method = "cv", number = 10))

# View the model details
print(svm_model)

# Predict on the test data
svm_predictions <- predict(svm_model, test_data)

# Evaluate the model performance
confusionMatrix(svm_predictions, test_data$Class)
