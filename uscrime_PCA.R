#initialize environment
library(ggplot2)
library(GGally)
library(DAAG)

#Continue exploring uscrime data from statsci.org now using Principal Component Analysis
#to predict crime for made up city

cat("\014")
rm(list=ls())
#set seed to remove randomness 
set.seed(123)

#read crime data
file_loc = "uscrime.txt"
uscrime = read.delim(file_loc)

#-----------------------------------------------------------------------------
#plot the data to see if there is correlation between any of the factors
ggpairs(uscrime, columns = c("Po1", "Po2", "U1", "U2", "Ineq", "Crime"),
        mapping=ggplot2::aes(color= "#3366FF"))

#correlation table shows the same thing, but less fancy
corr <- cor(uscrime)
round(corr, 2)
#-----------------------------------------------------------------------------

#create new test point
test_point <- data.frame(M=14.0, So=0, Ed=10.0, Po1=12.0, Po2=15.5, LF=0.640, M.F=94.0, Pop=150, NW=1.1,U1=0.120,U2=3.6, Wealth=3200, Ineq=20.1, Prob=0.04,Time = 39.0)

#find principal components
uscrime.pca <- prcomp(uscrime[,-16], scale. = TRUE)

#-----------------------------------------------------------------------------

#screeplot will help visualize how many PC's are really significant
screeplot(uscrime.pca, type="lines",col="blue")

# Calculate the variances and proportion of variances from the pca object

var <- uscrime.pca$sdev^2
propvar <- var/sum(var)

# Plot the proportion of variances from PCA

plot(propvar, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = "b")

# Plot the cumsum proportion of variances from PCA
cumsum(propvar)
plot(cumsum(propvar), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained",ylim = c(0,1), type = "b")

#-----------------------------------------------------------------------------

#set number of princpal componenets and save them from eigenvector matrix
numberOfPCs = 4
PCA_four <- uscrime.pca$x[,1:numberOfPCs]

#create new data matrix for regression using principal components and original crime data
uscrimePC <- cbind(PCA_four, uscrime[,16])

#new regression for beta coefficients
PCAmodel <- lm(V5~., data = as.data.frame(uscrimePC))
summary(PCAmodel)

#save coefficients from linear regression model based on PCs
betaCoeff <-PCAmodel$coefficients[-1]

#save y-intercept from linear regression model based on PCs
b0 <- PCAmodel$coefficients[1]

#rotate all coefficients, except for intercept back into original coordinate space
scaled_alphas <-betaCoeff%*%t(uscrime.pca$rotation[,1:numberOfPCs])

#scale alphas back to original space by dividing by st_dev
origAlphas <- scaled_alphas/uscrime.pca$scale

#scale intercept back to original space by subtracting from the intercept the sum of (alpha*mu)/sigma
origb0 <- b0 - sum(scaled_alphas*uscrime.pca$center/uscrime.pca$scale)

#now that we have y-intercept and coefficients we can create our linear equation y=mx+b
#create new estimates in order to calculate R^2
estimates <- as.matrix(uscrime[,1:15])%*%t(origAlphas) + origb0
SSE <- sum((estimates-uscrime[,16])^2)
SST <- sum((uscrime[,16]-mean(uscrime[,16]))^2)
R2 <- 1-SSE/SST
print(R2)

#-----------------------------------------------------------------------------
#lets perform cross-validation to evaluate performance based on the number of PC's used

# do 5-fold cross-validation

r2cross <- numeric(15) # create a vector to store the R-squared values

for (i in 1:15) {
  pclist <- uscrime.pca$x[,1:i]  # use the first i prinicipal components
  pcc <- cbind(uscrime[,16],pclist)  # create data set
  model <- lm(V1~.,data = as.data.frame(pcc)) # fit model
  c <- cv.lm(as.data.frame(pcc),model,m=5) # cross-validate 
  r2cross[i] <- 1 - attr(c,"ms")*nrow(uscrime)/sum((uscrime$Crime - mean(uscrime$Crime))^2) # calculate R-squared
}

r2cross

plot(r2cross, xlab = "Principal Component", ylab = "Cross-validated R-squared with this many principal components",
     ylim = c(0,1), type = "b")

#the 5th principal component seems to be where a big jump in performance occurs

#-----------------------------------------------------------------------------

#use principal component model to transform test point into PCA coordinate space
#PCA coordinate space is where the linear regression model was built
pred_df <- data.frame(predict(uscrime.pca, test_point))


PCA_four <- uscrime.pca$x[,1:5]
uscrimePC <- cbind(PCA_four, uscrime[,16])
PCAmodel <- lm(V6~., data = as.data.frame(uscrimePC))

#predict new crime rate by using pred_df and our linear regression model
pred <- predict(PCAmodel, pred_df)
print(pred)
