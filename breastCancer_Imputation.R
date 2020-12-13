library(kknn)
library(DAAG)

#Use breast cancer data from http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/  
#to practice imputing missing data values

cat("\014")
rm(list=ls())
#set seed to remove randomness 
set.seed(123)

#read cancer data
file_loc = "C:\\Users\\Steph\\Documents\\Matt\\ISYE6501\\Week 10\\breast-cancer-wisconsin.data.txt"
cancer = read.csv(file_loc, header = FALSE, sep = ",", stringsAsFactors = FALSE)

#Column V7 is the only one missing values, which appear to be '?'
#get indices of missing data
cancer_rep<-which(cancer$V7=='?')

#replace ? with zeros
cancer$V7[cancer_rep]<-0
cancer$V7<-as.integer(cancer$V7)

####impute values with mean
cancer_mean_rep<-cancer
cancer_mean <- mean(cancer$V7[-cancer_rep])
cancer_mean_rep$V7[cancer_rep]<-cancer_mean

####impute values with mode
cancer_mode<-cancer
hist(cancer$V7)

uniqv <- unique(cancer$V7)
common<-uniqv[which.max(tabulate(match(cancer$V7,uniqv)))]
cancer_mode$V7[cancer_rep]<-common

####impute based on linear regression
cancer_lm<-cancer
cancer_lm_pert<-cancer

#create training/test data
train<-cancer[-cancer_rep,2:10]
test<-cancer[cancer_rep,c(2,3,4,5,6,8,9,10)]

#train model and predict points
cancer_model<-lm(V7~., data = train)
summary(cancer_model)

#not all the variables are significant, use backwards stepwise regression to 
#choose significant variables
step(cancer_model)

# Generate the linear model that stepwise regression recommends.
model2 <- lm(V7~V2+V4+V5+V8, data = train)
summary(model2)

#use cross validation to see how good model is
model2_cv <- cv.lm(train, model2, m=5)
SST <- sum((as.numeric(cancer[-cancer_rep,]$V7) - mean(as.numeric(cancer[-cancer_rep,]$V7)))^2)
R2_cv <- 1 - attr(model2_cv,"ms")*nrow(cancer[-cancer_rep,])/SST
R2_cv

##0.608, pretty good model

#get predictions for missing V7 values
pred_lm <- predict(model2, test)


#round all predictions to integers
cancer_lm$V7[cancer_rep]<-as.integer(round(pred_lm))

#bound results to the same as other V7 values
pred_lm[round(pred_lm)<1]<-1
pred_lm[round(pred_lm)>10]<-10

#insert points
cancer_lm$V7<-as.integer(cancer_lm$V7)


# Perturb the predictions for missing V7 values with a random normal distriubtion
# in which the predicted values are the means and the standard deviation is the 
# standard deviation of the predicted values.

V7_hat_pert <- rnorm(nrow(cancer[cancer_rep,]), pred_lm, sd(pred_lm))

# Notice that we get some negative values when we perturb the predicted values.

data_reg_pert_imp <- cancer
data_reg_pert_imp[cancer_rep,]$V7 <- V7_hat_pert
data_reg_pert_imp$V7 <- as.numeric(data_reg_pert_imp$V7)

# Round the V7_hat_pert values to integers.

data_reg_pert_imp[cancer_rep,]$V7 <- round(V7_hat_pert)
data_reg_pert_imp$V7 <- as.integer(data_reg_pert_imp$V7)

# Make sure no V7 values are outside of the orignal range.

data_reg_pert_imp$V7[data_reg_pert_imp$V7 > 10] <- 10
data_reg_pert_imp$V7[data_reg_pert_imp$V7 < 1] <- 1

# test the accuracy of the different datasets that we have imputed values for
##test
training <- sample(nrow(cancer), size = floor(nrow(cancer) * 0.7))
validation <- setdiff(1:nrow(cancer), training)

accuracy<-rep(0,25)
#data with mean imputation
for(x in 1:5){
  model=kknn(V11~V2+V3+V4+V5+V6+V7+V8+V9+V10, cancer_mean_rep[training,], cancer_mean_rep[validation,], k=x)
  predicted<-as.integer(fitted(model)+0.5)
  accuracy[x] = sum(predicted==cancer_mean_rep[validation,]$V11)/nrow(cancer_mean_rep[validation,])
}

#data with linear regression imputation
for(x in 1:5){
  model=kknn(V11~V2+V3+V4+V5+V6+V7+V8+V9+V10, cancer_lm[training,], cancer_lm[validation,], k=x)
  predicted<-as.integer(fitted(model)+0.5)
  accuracy[x+5] = sum(predicted==cancer_lm[validation,]$V11)/nrow(cancer_lm[validation,])
}

#data with perturbation
for(x in 1:5){
  model<-kknn(V11~V2+V3+V4+V5+V6+V7+V8+V9+V10, data_reg_pert_imp[training,], data_reg_pert_imp[validation,], k=x)
  predicted<-as.integer(fitted(model)+0.5)
  accuracy[x+10] = sum(predicted==data_reg_pert_imp[validation,]$V11)/nrow(data_reg_pert_imp[validation,])
}

#no large change between the models

