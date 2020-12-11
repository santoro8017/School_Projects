#initialize environment
library(kernlab)
cat("\014")
rm(list=ls())
set.seed(1)

#using the file credit_card_data from UCI Machine Learning repository try to predict the credit score
#using various version of the SVM function

#read file
file_loc = "credit_card_data-headers.txt"
data = read.delim(file_loc)

#create multiple versions of the SVM model to compare
model <- ksvm(as.matrix(data[,1:10]), as.factor(data[,11]), type="C-svc", kernel="vanilladot",C=100, scaled = TRUE)
model1 <- ksvm(as.matrix(data[,1:10]), as.factor(data[,11]), type="C-svc", kernel="rbfdot",C=100, scaled = TRUE)
model2 <- ksvm(as.matrix(data[,1:10]), as.factor(data[,11]), type="C-svc", kernel="splinedot",C=100, scaled = TRUE)
model3 <- ksvm(as.matrix(data[,1:10]), as.factor(data[,11]), type="C-svc", kernel="laplacedot",C=100, scaled = TRUE)

#print coefficient values ot understand the effect each factor has on the model
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
print(paste(a))

#print the intercept value
a0 <- -model@b
print(paste(a0))

#make predicitons using each of the different models 
pred <- predict(model,data[,1:10])
predPerc = sum(pred==data[,11])/nrow(data)
print(cat("vanilladot", predPerc))

pred <- predict(model1,data[,1:10])
predPerc = sum(pred==data[,11])/nrow(data)
print(cat("rbfdot", predPerc))

pred <- predict(model2,data[,1:10])
predPerc = sum(pred==data[,11])/nrow(data)
print(cat("splinedot", predPerc))

pred <- predict(model3,data[,1:10])
predPerc = sum(pred==data[,11])/nrow(data)
print(cat("laplacedot", predPerc))