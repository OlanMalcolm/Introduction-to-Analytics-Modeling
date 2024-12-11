set.seed(123)

data <- read.table("temps.txt", header = TRUE)

head(data)

data <- data[,-1]
data <- as.vector(unlist(data))

data_ts <- ts(data, frequency = 123, start = 1996)
data_ts

plot(data_ts)

pacman::p_load(zoo)

data_mean = rollmean(data_ts,30,fill = NA, allign = "right")
plot(data_mean)

data_holt <- HoltWinters(data_ts)
data_holt
