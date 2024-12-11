set.seed(123)

pacman::p_load(kernlab, kknn, caret )
data <- read.table("credit_card_data-headers.txt", header = TRUE)
data_part_1 <- createDataPartition(data$R1, p=.60, list = FALSE)
str(data_part_1)

train_data <- data[data_part_1,]
dim(train_data)

remaining_data <- data[-data_part_1,]
data_part_2 <- createDataPartition(y=remaining_data$R1, p=.40, list = FALSE)

validation_data <- remaining_data[data_part_2,]
dim(validation_data)

test_data <- remaining_data[-data_part_2,]
dim(test_data)

k_list <- c(seq(from = 1, to = 100, by = 2))
knn_prediction <- c()

for (k in 1:length(k_list)){
  knn_model <- kknn(R1~., train_data, validation_data, k=k_list[[k]], kernel = "optimal", distance = 2, scale = TRUE)
    knn_predict <- as.integer(fitted(knn_model)+0.5)
  knn_prediction[k] <- sum(knn_predict == validation_data$R1)/nrow(validation_data)
}

do.call(rbind, Map(data.frame, K=k_list, knn_prediction = knn_prediction))

sprintf("k = %f provides the best knn prediction of %f", k_list[which(knn_prediction == max(knn_prediction))[1]], max(knn_prediction))

knn_model_best <- kknn(R1~., train_data, test_data, k=k_list[which(knn_prediction == max(knn_prediction))[1]], kernel = "optimal", distance = 2, scale = TRUE)
knn_predict <- as.integer(fitted(knn_model_best)+0.5)
knn_predict_best <- sum(knn_predict == test_data$R1)/nrow(test_data)
sprintf("k = %f provides the best knn prediction of %f", k_list[which(knn_prediction == max(knn_prediction))[1]], max(knn_predict_best))
