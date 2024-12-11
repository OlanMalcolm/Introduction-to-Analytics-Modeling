set.seed(123)

pacman::p_load(kernlab, dplyr, kknn, caTools)

data <- read.table("credit_card_data-headers.txt", header = TRUE)

k_list <- c(seq(from = 1, to = 100, by = 2))

knn_model <- train.kknn(R1~., data, ks=k_list, kcv = 10, kernel = "optimal", distance = 2, scale = TRUE)
print(knn_model)
print(knn_model$MEAN.SQU)
