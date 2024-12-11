set.seed(123)
crimedata <- read.table("uscrime.txt", header = TRUE)

PCA_crimedata <- prcomp(crimedata[,-16], scale = TRUE)
summary(PCA_crimedata)

plot(PCA_crimedata, type = "l")

biplot(PCA_crimedata, scale = 0)

PCs <- PCA_crimedata$x[,1:6]

crime2 <- cbind(PCs, crimedata[,16])
crime2 <- as.data.frame(crime2)

model <- lm(V7~., data = crime2)
plot(model)
str(model)
summary(model)

intercept <- model$coefficients[1]
coefficents <- model$coefficients[2:7]
coefficents
intercept

(PCA_crimedata)

alpha <- PCA_crimedata$rotation[,1:6] %*% coefficents
alpha

sd <- sapply(crimedata[,1:15], sd)
mean <- sapply(crimedata[,1:15], mean)
alpha_org <- alpha/sd
alpha_org

a0_org <- intercept - sum(alpha*mean/sd)
a0_org

crime_value_predict <- as.matrix(crimedata[,-16]) %*% alpha_org + a0_org
SSE <- sum((crimedata[,16] - crime_value_predict)^2)
SSE
SStot <- sum((crimedata[,16] - mean(crimedata[,16]))^2)
SStot
Rsquared <- 1-SSE/SStot
Rsquared
