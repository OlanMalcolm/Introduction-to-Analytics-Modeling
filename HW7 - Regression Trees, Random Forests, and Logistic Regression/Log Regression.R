set.seed(123)
creditdata <- read.table("germancredit.txt", header = FALSE)

creditdata$V21 <- creditdata$V21 - 1

cd = sample(2, nrow(creditdata), replace = TRUE, prob = c(0.7, 0.3))
train <- creditdata[cd ==1, ]
test <- creditdata[cd ==2, ]

model.glm <- glm(V21 ~., data = train, family = 'binomial'(link = 'logit'))
summary(model.glm)

model.glm2 <- glm(V21 ~ V1+V2+V3+V4+V5+V6+V8+V10+V13+V14+V20, data = train, family = 'binomial'(link = 'logit'))
summary(model.glm2)

pr1 <- predict(model.glm, test, type = 'response')
t = 0.50
for (i in 1:length(pr1)){
  if (pr1[[i]] >= t){
    pr1[[i]] <- 1
  }else{
    pr1[[i]] <- 0
  }
  }
table1 <- table(pr1, test$V21)
table1

Acc1 <- sum(diag(table1))/sum(table1)
Acc1

library(dplyr)
testc = test%>%select('V1', 'V2', 'V3', 'V4', 'V5', 'V6', 'V8', 'V10', 'V13', 'V14', 'V20')
pr2 <- predict(model.glm2, testc, type = 'response')
t = 0.50
for (i in 1:length(pr2)){
  if (pr2[[i]] >= t){
    pr2[[i]] <- 1
  }else{
    pr2[[i]] <- 0
  }
}
table2 <- table(pr2, test$V21)
table2

Acc2 <- sum(diag(table2))/sum(table2)
Acc2

l <- c(seq(0,1, by = 0.05))
accuracy_model2 <- c()
false_positives <- c()
j = 0
for(k in seq(0,1, by = 0.05)){
  pr2 <- predict(model.glm2, testc, type = 'response')
  
  for(i in 1:length(pr2)){
    if(pr2[[i]] >= k){
      pr2[[i]] <- 1
    } else {
      pr2[[i]] <- 0
    }
  }
  
  table2 <- table(pr2, test$V21)
  Acc2 <- sum(diag(table2))/sum(table2)
  accuracy_model2[j] = Acc2
  if(j < length(l)-2 && j > 2){
    false_positives[j] = table2[1,2]/sum(table2)
  } else {
    false_positives[j] = NA
  }
  j = j + 1
}
cbind("Threshold" = l, accuracy_model2, false_positives)

library(pROC)
par(pty = 's')
roc_data <- roc(train$V21 , model.glm2$fitted.values, plot = TRUE, legacy.axes = TRUE, col = "blue", lwd = 4, print.auc = TRUE)
plot.roc(train$V21 , model.glm$fitted.values,legacy.axes = TRUE, col = "green", lwd = 4, print.auc = TRUE, add = TRUE, print.auc.y = 0.2)
