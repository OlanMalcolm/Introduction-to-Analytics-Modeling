library(kknn)

max_k <- 100
num_points <- nrow(CCdata)
accuracy_results <- numeric(max_k)


for (k in 1:max_k) {
  correct_predictions <- 0
  

  for (i in 1:num_points) {
    
    CCmodel_knn <- kknn(V11~., CCdata[-i,], CCdata[i,], k = k, distance = 2, kernel = "optimal", scale = TRUE)
    
    predicted_class <- round(fitted.values(CCmodel_knn))

    actual_class <- CCdata[i, 11]
    
    if (predicted_class == actual_class) {
      correct_predictions <- correct_predictions + 1
    }
  }
  
  accuracy_results[k] <- correct_predictions / num_points
}

best_k <- which.max(accuracy_results)
best_accuracy <- accuracy_results[best_k]

print(best_k)
print(best_accuracy)
