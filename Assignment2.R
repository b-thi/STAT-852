################################################################################
#                                                                              #
#                                                                              #
#                Assignment 2 - Barinder Thind - STAT 852                      #
#                                                                              #
#                                                                              #
################################################################################

#############
#           #
# Libraries #
#           # 
#############

library(tidyverse)
library(glmnet)

##############
#            #
# Lecture 3a #
#            # 
##############



##### Question 1 #####

# Reading in data
prostate <-  read.table("Prostate.csv", header=TRUE, sep=",", na.strings=" ")

LASSO_func <- function(seed_picked){
  
  # Splitting data in half using random uniform selection to make two "set"s.
  set.seed(120401002) 
  prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)
  
  # Setting up data
  y.1 <- prostate[which(prostate$set==1),10]
  x.1 <- as.matrix(prostate[which(prostate$set==1),c(2:9)])
  xs.1 <- scale(x.1)
  y.2 <- prostate[which(prostate$set==2),10]
  x.2 <- as.matrix(prostate[which(prostate$set==2),c(2:9)])
  xs.2 <- scale(x.2)
  
  # First half of data 
  lasso.1 <- glmnet(y=y.1, x= x.1, family="gaussian")
  lasso.1s <- glmnet(y=y.1, x= xs.1, family="gaussian")
  
  # cv
  cv.lasso.1 <- cv.glmnet(y=y.1, x= x.1, family="gaussian")
  
  # Repeat for second half of data
  lasso.2 <- glmnet(y=y.2, x= xs.2, family="gaussian")
  cv.lasso.2 <- cv.glmnet(y=y.2, x= x.2, family="gaussian")
  
  ### Part (a) and (b) are calculated below
  
  # Predict both halves using first-half fit
  predict.1.1_min <- predict(cv.lasso.1, newx = x.1, s = cv.lasso.1$lambda.min)
  sMSE.lasso_1_min <- mean((y.1 - predict.1.1_min)^2)
  predict.1.2_min <- predict(cv.lasso.1, newx = x.2, s = cv.lasso.1$lambda.min)
  MSPE.lasso_1_min <- mean((y.2 - predict.1.2_min)^2)
  
  predict.1.1_1se <- predict(cv.lasso.1, newx = x.1, s = cv.lasso.1$lambda.1se)
  sMSE.lasso_1_1se <- mean((y.1 - predict.1.1_1se)^2)
  predict.1.2_1se <- predict(cv.lasso.1, newx = x.2, s = cv.lasso.1$lambda.1se)
  MSPE.lasso_1_1se <- mean((y.2 - predict.1.2_1se)^2)
  
  # Predicting both halves using second half fit
  predict.2.1_min <- predict(cv.lasso.2, newx=x.2, s = cv.lasso.2$lambda.min)
  sMSE.lasso_2_min <- mean((y.2 - predict.2.1_min)^2)
  predict.2.2_min <- predict(cv.lasso.2, newx=x.1, s = cv.lasso.2$lambda.min)
  MSPE.lasso_2_min <- mean((y.1 - predict.2.2_min)^2)
  
  predict.2.1_1se <- predict(cv.lasso.2, newx=x.2, s = cv.lasso.2$lambda.1se)
  sMSE.lasso_2_1se <- mean((y.2 - predict.2.1_1se)^2)
  predict.2.2_1se <- predict(cv.lasso.2, newx=x.1, s = cv.lasso.2$lambda.1se)
  MSPE.lasso_2_1se <- mean((y.1 - predict.2.2_1se)^2)
  
  # Creating table
  df_a <- data.frame(Model = c("Training set 1, min",
                               "Training set 1, 1se",
                               "Training set 2, min",
                               "Training set 2, 1se"),
                     Lambda = c(cv.lasso.1$lambda.min,
                                cv.lasso.1$lambda.1se,
                                cv.lasso.2$lambda.min,
                                cv.lasso.2$lambda.1se),
                     Training_Error = c(sMSE.lasso_1_min,
                                        sMSE.lasso_1_1se,
                                        sMSE.lasso_2_min,
                                        sMSE.lasso_2_1se),
                     Test_Error = c(MSPE.lasso_1_min,
                                    MSPE.lasso_1_1se,
                                    MSPE.lasso_2_min,
                                    MSPE.lasso_2_1se))
  
  return(df_a)
}

# Part (a) and (b)
LASSO_func(120401002)

##### Question 2 #####


##### Question 3 #####
LASSO_func(9267926)

##### Question 4 #####
LASSO_relax_func <- function(seed_chosen){
  
  # Splitting data in half using random uniform selection to make two "set"s.
  set.seed(seed_picked) 
  prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)
  
  # Setting up data
  y.1 <- prostate[which(prostate$set==1),10]
  x.1 <- as.matrix(prostate[which(prostate$set==1),c(2:9)])
  y.2 <- prostate[which(prostate$set==2),10]
  x.2 <- as.matrix(prostate[which(prostate$set==2),c(2:9)])
  
  # Rescaling
  rescale <- function(x1,x2){scale(x1, center = apply(x2, 2, mean), scale = apply(x2, 2, sd)) 
  }
  
  # Applying function
  y.1s1 <- scale(y.1, mean(y.1), sd(y.1))
  y.2s2 <- scale(y.2, mean(y.2), sd(y.2))
  x.1s1 <- rescale(x.1, x.1)
  x.2s1 <- rescale(x.2, x.1)
  x.2s2 <- rescale(x.2, x.2)
  x.1s2 <- rescale(x.1, x.2)
  
  #Look atresults over values of lambda and phi
  # Use crossvalidation to select "optimal" lambda and phi
  cv.relaxo.1 <- cvrelaxo(Y=y.1s1, X= x.1s1)
  cv.relaxo.2 <- cvrelaxo(Y=y.2s2, X= x.2s2)
  
  # Get predicted values and (important!) rescale them to original Y scale
  predrel.1.1 <- predict(cv.relaxo.1, 
                         newX=x.1s1, 
                         lambda = cv.relaxo.1$lambda, 
                         phi = cv.relaxo.1$phi)
  predrely.1.1 <- predrel.1.1*sd(y.1) + mean(y.1)
  sMSE_1 <- mean((predrely.1.1 - y.1s1)^2)
  
  predrel.1.1 <- predict(relaxo.1, 
                         newX=x.2s1, 
                         lambda = cv.relaxo.1$lambda, 
                         phi = cv.relaxo.1$phi)
  
  predrely.1.1 <- predrel.1.1*sd(y.1) + mean(y.1)
  sMSE_1 <- mean((predrely.1.1 - y.1s1)^2)
  
  
  # Predict other half and compute MSPE
  predrel.1.2 <- predict(relaxo.1, newX=x.2s1, lambda=2.46, phi=0.333)
  predrely.1.2 <- predrel.1.2*sd(y.1) + mean(y.1)
  MSPE.relaxo <- mean((y.2 - predrely.1.2)^2)
}

