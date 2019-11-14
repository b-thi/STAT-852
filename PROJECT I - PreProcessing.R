################################################################################
#                                                                              #
#                                                                              #
#                  Project I - Barinder Thind - STAT 852                       #
#                                                                              #
#                                                                              #
################################################################################

#############
#           #
# Libraries #
#           # 
#############

library(tidyverse)
library(GGally)
library(keras)
library(future.apply)
library(randomForest)
library(mgcv)
library(gbm)
library(glmnet)
library(caret)

#############
#           #
#   EDA     #
#           # 
#############

# Loading in data
dat <- read.csv("Data2019.csv", header = T)

# Changing into factor
dat$X1 <- as.factor(dat$X1)
dat$X2 <- as.factor(dat$X2)
dat$X15 <- as.factor(dat$X15)

# Setting seed
set.seed(1994)

# Splitting into usual data set and validation set
samp_ind <- sample(1:nrow(dat), ceiling(0.8*nrow(dat)))
validation <- dat[-samp_ind,]
dat2 <- dat[samp_ind,]

# Indicator set up
dmy <- dummyVars(" ~ .", data = dat2, fullRank = T)
dat2_ind <- data.frame(predict(dmy, newdata = dat2))

# Looking at pairs plot
ggpairs(dat2) + theme_bw()

#######################
#                     #
#   Var Selection     #
#                     # 
#######################

##### LASSO Variable Selection #####

# Initializing

# Random selection of the data set
set.seed(3)
resamp <- sample(1:nrow(dat2_ind), nrow(dat2_ind), replace = T)

# Creating selected data
dat2_ind_boot <- dat2_ind[resamp,]

# Setting up data
y.1 <- dat2_ind_boot[, 1]
x.1 <- as.matrix(dat2_ind_boot[,-1])
xs.1 <- scale(x.1)

# cv
cv.lasso.1 <- cv.glmnet(y=y.1, x= x.1, family="gaussian")

# checking coefficients
coef(cv.lasso.1, s = cv.lasso.1$lambda.min)
plot(cv.lasso.1$glmnet.fit, s = cv.lasso.1$lambda.min)

# Variables 5, 13, and 14 get taken out

##### Random Forest Variable Selection #####

# Initializing count
node_boot_var <- c()
IncMSE_boot_var <- c()

# Looping to get results
for (i in 1:1000) {
  
  # Setting seed
  set.seed(i)
  
  # Random selection of the data set
  resamp <- sample(1:nrow(dat2), nrow(dat2), replace = T)
  
  # Creating selected data
  dat_to_use <- dat2[resamp,]
  
  # Creating model
  rf_var <- randomForest(Y ~ ., 
                         data = dat_to_use, 
                         importance = T)
  
  # Saving results
  node_boot_var <- c(node_boot_var, 
                     names(sort(rf_var$importance[,1], decreasing = T))[1:5])
  IncMSE_boot_var <- c(IncMSE_boot_var, 
                       names(sort(rf_var$importance[,2], decreasing = T))[1:5])
  
  print(i)
  
}

# looking at summary of results
sort(table(node_boot_var), decreasing = T)
sort(table(IncMSE_boot_var), decreasing = T)

# Variable Important Plots
varImpPlot(rf_var, main = "Random Forest Importance Plots")

##### Gradient Boosting Variable Selection #####

# Initializing count
top_5_vars <- c()

# Loop to get importances
for (i in 1:1000) {
  
  # Setting seed
  set.seed(i)
  
  # Random selection of the data set
  resamp <- sample(1:nrow(dat2), nrow(dat2), replace = T)
  
  # Creating selected data
  dat_to_use <- dat2[resamp,]
  
  # Building model
  gbm_var <- gbm(Y ~ ., 
                 data = dat_to_use,
                 distribution = "gaussian")
  
  # Plots
  imp_summary <- summary(gbm_var)
  
  # Getting top 5
  top_5_vars <- c(top_5_vars, as.character(imp_summary$var[1:5]))

}

# Looking at summary of results
sort(table(top_5_vars), decreasing = T)

# Getting plot
imp_summary


##### BIC Variable Selection #####




