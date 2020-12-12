#initialize environment
library(DAAG)

#based on crime data from statsci.org use linear regression to predict crime for made up city

#initialize environment
cat("\014")
rm(list=ls())
#set seed to remove randomness 
set.seed(123)

#read file
file_loc = "uscrime.txt"
data = read.delim(file_loc)
head(data)

#create test point/made up city
test_point <- data.frame(M=14.0, So=0, Ed=10.0, Po1=12.0, Po2=15.5, LF=0.640, M.F=94.0, Pop=150, NW=1.1,U1=0.120,U2=3.6, Wealth=3200, Ineq=20.1, Prob=0.04,Time = 39.0)

#generat model based on loaded crime data
linear_model <- lm(Crime~., data)

#look at model to understand performance and significant variables
summary(linear_model)

#the model has a good R^2 value, but there are many variables being used in the model that are not significant

pred <- predict(linear_model, test_point)
pred

#plot other crime values to see if prediction is reasonable
plot(data$Crime)  
#the predicted value is less then half the next lowest crime value, this seems like an unreasonable prediction
#let's remove the factors that aren't significant to see if it improves performance

model2 <- lm( Crime ~  M + Ed + Po1 + U2 + Ineq + Prob, data = data)

summary(model2)

#the model has a high R^2 again and all the factors appear significant

pred2<-predict(model2, test_point)
pred2

#the prediction is now much closer to other city crime values and seems reasonable

#let's use cross-validation to find a better metric for the performance of both models
c<-cv.lm(data,model2,m=5)

# We can calculate the R^2 values directly.
# R^2 = 1 - SSEresiduals/SSEtotal
#
# total sum of squared differences between data and its mean

SStot <- sum((data$Crime - mean(data$Crime))^2)

# for linear_model, model2, and cross-validation, calculated SSEres

SSres_model <- sum(linear_model$residuals^2)

SSres_model2 <- sum(model2$residuals^2)

SSres_c <- attr(c,"ms")*nrow(data) # mean squared error, times number of data points, gives sum of squared errors

# Calculate R^2's for model, model2, cross-validation

1 - SSres_model/SStot # initial model with insignificant factors

1 - SSres_model2/SStot # model2 without insignificant factors

1 - SSres_c/SStot # cross-validated

# Including the insignificant factors overfits compared to removing them,

# We can also try cross-validation on the first, 15-factor model

cfirst <- cv.lm(data,linear_model,m=5)

SSres_cfirst <- attr(cfirst,"ms")*nrow(data) # mean squared error, times number of data points, gives sum of squared errors

1 - SSres_cfirst/SStot # cross-validated

