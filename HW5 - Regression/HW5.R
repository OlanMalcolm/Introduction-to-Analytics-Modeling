set.seed(123)

crimedata <- read.table("uscrime.txt", header = TRUE)


model <- lm(Crime ~., data = crimedata)
plot(model)

summary(model)

model2 <- lm(Crime ~ M + Ed + U2 + Po1 + Ineq + Prob, data = crimedata)
plot(model2)

summary(model2)


test <- data.frame(M = 14.0, SO = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5, LF = 0.640, M.F = 94.0, Pop = 150, 
                   NW = 1.1, U1 = 0.120, U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.040, Time = 39.0)
pred_model <- predict(model2, test)
pred_model

pacman::p_load(DAAG)

cv <- cv.lm(crimedata, model2, m=5)

SSE <- attr(cv, "ms")*nrow(crimedata)
SST = sum((crimedata$Crime - mean(crimedata$Crime))^2)
CVR = 1 - SSE/SST
CVR


