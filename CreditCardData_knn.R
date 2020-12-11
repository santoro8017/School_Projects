library(kknn)
cat("\014")
rm(list=ls())

#using the file credit_card_data from UCI Machine Learning repository try to predict whether an application was positive or negative
#using k-nearest neighbors

#load file
file_loc = "credit_card_data-headers.txt"
data = read.delim(file_loc)

#create data containers for prediction results
results <- rep(100,nrow(data))
predPerc <- rep(0,20)

#loop through values of k to understand what the optimal number of neighbors for prediction would be
for (ii in 1:20){
  #loop through each row of data and create model using all other data points except current row
  #use model to make prediction for current row
  for (i in 1:nrow(data)) {
    kknn_model <- kknn(R1~., k=ii, data[-i,], data[i,], scale = TRUE)
    results[i] = round(fitted(kknn_model),digits=0)
  }
  
  #output to indicate place in dataset
  print(ii)
  
  #compare all predicited results to actual values to find performance
  predPerc[ii] = sum(results==data[,11])/nrow(data)
}

#plot performance for an overview
plot(predPerc, main="rank weighting")
print(predPerc)

