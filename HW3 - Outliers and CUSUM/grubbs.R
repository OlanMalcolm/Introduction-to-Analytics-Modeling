set.seed(123)
pacman::p_load(outliers)

data <- read.table("uscrime.txt", header = TRUE)

head(data[, "Crime"])

crime <- data[,"Crime"]
hist(crime, breaks = "Sturges")

boxplot(crime, horizontal = FALSE)

grubbs.test(crime, type = 11)

grubbs.test(crime, type = 10)

summary(crime)

