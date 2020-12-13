library(tree)
library(DAAG)

#Continue exploring uscrime data from statsci.org now using a regression tree
#to predict crime for made up city

##question 10.1 decision tree
cat("\014")
rm(list=ls())
#set seed to remove randomness 
set.seed(123)

#read crime data
file_loc = "uscrime.txt"
uscrime = read.delim(file_loc)

#initial look at data
head(uscrime)

uscrime_tree = tree(Crime~., data = uscrime)
summary(uscrime_tree)

#summary shows that only four variables were used to build the tree

#more information about how data was split
uscrime_tree$frame

plot(uscrime_tree)
text(uscrime_tree)

# Determine if pruning the tree will improve performance through cross-validation
# by looking at the deviance of trees with different number of terminal nodes.

cv.data <- cv.tree(uscrime_tree)
plot(cv.data$size, cv.data$dev, type = "b")

# This plot suggests that we get the best fit using all of the terminal nodes in the
# tree that we already plotted.

# Procede using the unpruned regression model.
yhat_tree <- predict(uscrime_tree)

# Plot of actual vs. predicted crime values
plot(uscrime$Crime, yhat_tree)
abline(0,1)


# Calculate SSres of the unpruned regression model.
ss_tot <- sum((uscrime$Crime - mean(uscrime$Crime))^2)
ss_res <- sum((uscrime$Crime-yhat_tree)^2)
R2_tree <- 1-(ss_res/ss_tot)

#R^2 is pretty good but we should compare it using cross validation


# Here's the sum of squared errors for trees of each size,
# from 7 nodes down to 1 node (1 leaf)
prune.tree(uscrime_tree)$size
prune.tree(uscrime_tree)$dev

#now compare with CV
cv.data <- cv.tree(uscrime_tree) # cross-validation
cv.data$size
cv.data$dev

#the CV errors are much larger

#let's see if we can improve performance if we only split into two leaves and create a 
#regression model for each leaf
prune.data <- prune.tree(tree.data,best=2)

# Now each data point is in one of two leaves:

prune.data$where

# Separate rows of data in each leaf
d1 <- data[which(prune.data$where == 2),]
d2 <- data[which(prune.data$where == 3),]

# First leaf:
m1 <- lm(Crime~.,data=d1)
summary(m1)

# But only four factors are even marginally significant, so refine:

m1b <- lm(Crime~Ed+Pop+Prob+Time,data=d1)
summary(m1b)

# But now only two factors are even marginally significant, so refine:

m1c <- lm(Crime~Pop+Time,data=d1)
summary(m1c)

# And now just one factor is significant:

m1d <- lm(Crime~Pop,data=d1)
summary(m1d)

# R-squared on training data is 0.296, now estimate with leave-one-out cross-validation (since we have few data points):

c1d <- cv.lm(d1,m1d,m=nrow(d1))
1 - attr(c1d,"ms")*nrow(d1)/sum((d1$Crime - mean(d1$Crime))^2)
## [1] 0.181

#second leaf.

m2 <- lm(Crime~.,data=d2)
summary(m2)

# Only one factor is significant:

m2b <- lm(Crime~Ineq,data=d2)
summary(m2b)

#none of the factors appear to be significant
