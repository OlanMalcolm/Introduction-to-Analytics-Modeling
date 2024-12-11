library(DAAG)
library(tree)
set.seed(123)
crimedata <- read.table('uscrime.txt', header = TRUE)

tree_model <- tree(Crime ~., data = crimedata)
summary(tree_model)

plot(tree_model)
text(tree_model)

pr <- predict(tree_model, crimedata)
SSE = sum((pr - crimedata$Crime)^2)
Stotal = sum((crimedata$Crime - mean(crimedata$Crime))^2)

R2_tree = 1 - SSE/Stotal
R2_tree

prune.tree(tree_model)$size
prune.tree(tree_model)$dev

str(tree_model)

leaf <- crimedata[which(tree_model$where == 6),]
leaf
