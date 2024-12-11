library(randomForest)
set.seed(123)
crimedata <- read.table("uscrime.txt", header = TRUE)

str(crimedata)

cd <- sample(2, nrow(crimedata), replace = TRUE, prob = c(0.8, 0.2))
train <- crimedata[cd == 1, ]
test <- crimedata[cd == 2, ]

randomForest_train <- randomForest(Crime ~., data = train, ntree = 1000, mtry = 6, importance = TRUE)
randomForest_train

pr <- predict(randomForest_train, test)

table1 <- table(pr, test$Crime)
table1

Acc1 <- sum(diag(table1))/sum(table1)
Acc1

