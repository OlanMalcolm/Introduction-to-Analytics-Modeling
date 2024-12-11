library(kernlab)

CCdata <- read.table("credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)

CCmodel <- ksvm(V11 ~., data = CCdata, type = "C-svc", kernel = "vanilladot", C = 100000, scaled = TRUE)

CCmodel

a <- colSums(CCmodel@xmatrix[[1]]*CCmodel@coef[[1]])
a0 <- -CCmodel@b
a
a0

pred <- predict(CCmodel,CCdata[,1:10])
pred
sum(pred == CCdata$V11)/nrow(CCdata)
