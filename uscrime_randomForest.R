library(randomForest)
library(DAAG)

#Continue exploring uscrime data from statsci.org now using a random forest
#to predict crime for made up city
#read crime data

file_loc = "C:\\Users\\Steph\\Documents\\Matt\\ISYE6501\\Week 7\\uscrime.txt"
uscrime = read.delim(file_loc)
set.seed(42)

# Grow the random tree and set the number of predictors that 
# want to consider at each split of the tree (numpred)
num_pred<- 4
uscrime_rf <- randomForest(Crime~., 
                           data = uscrime, 
                           mtry = num_pred,
                           importance = TRUE)

uscrime_rf

importance(uscrime_rf)

yhat_rf <- predict(uscrime_rf)
ss_res_rf <- sum((uscrime$Crime-yhat_rf)^2)
R2_rf <- 1-(ss_res_rf/ss_tot)


# Plot of actual vs. predicted crime values

plot(uscrime$Crime, yhat_rf)
abline(0,1)

# Plot residuals

plot(uscrime$Crime, scale(yhat_rf - uscrime$Crime))
abline(0,0)


#cross-validation using leave-one-out

SSE <- 0

for (i in 1:nrow(uscrime)) {
  rd <- randomForest(Crime~., data = uscrime[-i,], mtry = num_pred, importance = TRUE)
  SSE = SSE + (predict(rd,newdata=uscrime[i,]) - uscrime[i,16])^2
}
1 - SSE/ss_tot

## 0.45

varImpPlot(uscrime_rf)


