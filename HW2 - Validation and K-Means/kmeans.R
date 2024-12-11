pacman::p_load(kernlab, caret, ggplot2)

iris <- read.table("iris.txt", header = TRUE)

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

iris_normalized <- as.data.frame(lapply(iris[,c(1,2,3,4)],normalize))

center_list <- c(seq(from = 1, to = 10, by = 1))
total_distance <- c()

for(i in 1:length(center_list)){
  iris_cluster <- kmeans(iris_normalized, centers = center_list[[i]], nstart = 10)
  total_distance[i] <- iris_cluster$tot.withinss
}

plot(center_list, total_distance)

iris_cluster_3 <- kmeans(iris_normalized, centers = 3, nstart = 10)

table(iris$Species, iris_cluster_3$cluster)

accuracy <- (50+47+36)/150
print(accuracy)
