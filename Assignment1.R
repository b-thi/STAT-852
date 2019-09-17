################################################################################
#                                                                              #
#                                                                              #
#                Assignment 1 - Barinder Thind - STAT 852                      #
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
library(MASS)
library(leaps)

##############
#            #
# Lecture 2a #
#            # 
##############

##############
#            #
# Question 1 #
#            # 
##############

# First, I take tom's code and make a function except add some
# parameters in correspondance to what changes in the questions. Namely,
# the parameters are sample size, beta1, and beta2
mspe_q1 <- function(sample_size, beta_1, beta_2) {
  
  set.seed(392039853)
  
  reps <- 200 # Number of data sets
  N <- sample_size    # Sample size
  
  # Create test data
  test <- expand.grid(x1 = c(.1,.3,.5,.7,.9), x2 = c(.1,.3,.5,.7,.9), x3=c(.1,.3,.5,.7,.9))
  
  # Assuming beta1=1, beta2=1, beta3=0
  # Create vector of true means = 1*x1 + 1*x2
  mu <- beta_1*test$x1 + beta_2*test$x2
  
  # Prepare for looping over reps
  counter <- 1
  # Matrix to save predictions: rows are replicates, 
  #   columns are different X combinations times 3 (one for each model)
  save.pred <- matrix(data=NA, ncol=3*nrow(test), nrow=reps)
  # Matrix to save estimates of sigma^2
  #   Rows are replicates, columns are different models 
  save.sig <- matrix(data=NA, ncol=3, nrow=reps)
  
  # Loop to generate data, analyze, and save results
  for(counter in c(1:reps)){
    # Generating Uniform X's and Normal errors
    x1 <- runif(n=N)
    x2 <- runif(n=N)
    x3 <- runif(n=N)
    ep <- rnorm(n=N)
    # Setting beta1=1, beta2=1, beta3=0
    y <- beta_1*x1 + beta_2*x2 + ep
    
    # reg* is model-fit object, sig* is MSE, pred* is list of predicted values over grid 
    reg1 <- lm(y~x1)
    sig1 <- sum(resid(reg1)^2) / reg1$df.residual
    # Could have used summary(reg1)$sigma^2
    pred1 <- predict(reg1, newdata = test)
    
    reg2 <- lm(y~x1 + x2)
    sig2 <- sum(resid(reg2)^2) / reg2$df.residual
    pred2 <- predict(reg2,newdata=test)
    
    reg3 <- lm(y~x1 + x2 + x3)
    sig3 <- sum(resid(reg3)^2) / reg3$df.residual
    pred3 <- predict(reg3,newdata=test)
    
    # Saving all results into storage objects and incrementing row counter
    save.pred[counter,] <- c(pred1, pred2, pred3)
    save.sig[counter,] <- c(sig1,sig2,sig3)
    counter <- counter + 1
  }
  
  # Estimate bias, variance, and MSE of predictions at each X-combo
  mean.pred <- apply(save.pred, MARGIN=2, FUN=mean)
  bias <- mean.pred - rep(mu, times=3)
  var <- apply(save.pred, MARGIN=2, FUN=var)
  MSE <- bias^2 + var
  
  # Vector of model numbers
  model <- rep(c(1,2,3), each=nrow(test))
  
  # Summary statistics for variances and MSEs for prediction by model
  mse_1 <- mean(MSE[which(model==1)])
  mse_2 <- mean(MSE[which(model==2)])
  mse_3 <- mean(MSE[which(model==3)])
  
  # Creating object to return
  df <- data.frame(MSPE = c(mse_1, mse_2, mse_3))
  row.names(df) <- c("Model 1", "Model 2", "Model 3")
  
  # Returning MSEs
  return(df)
  
}

# Getting the original
orig_results <- mspe_q1(20, 1, 1)
colnames(orig_results) <- "MSPE_original"

# (a)
a_results <- mspe_q1(100, 1, 1)
colnames(a_results) <- "MSPE_a"

# (b)
b_results <- mspe_q1(10, 1, 1)
colnames(b_results) <- "MSPE_b"

# (c)
c_results <- mspe_q1(20, 2, 1)
colnames(c_results) <- "MSPE_c"

# (d)
d_results <- mspe_q1(20, 0.5, 1)
colnames(d_results) <- "MSPE_d"

# (e)
e_results <- mspe_q1(20, 1, 2)
colnames(e_results) <- "MSPE_e"

# (f)
f_results <- mspe_q1(20, 1, 0.5)
colnames(f_results) <- "MSPE_f"

# (g)
g_results <- mspe_q1(20, 2, 2)
colnames(g_results) <- "MSPE_g"

### Putting together in a table
q1_table <- t(do.call("cbind", list(orig_results, a_results, b_results, 
                                  c_results, d_results, e_results, 
                                  f_results, g_results)))

### Looking at table
q1_table

##############
#            #
# Question 2 #
#            # 
##############

# First, I take tom's code and make a function except add some
# parameters in correspondance to what changes in the questions. Namely,
# the parameters are sample size, beta1, and beta2
mspe_q2 <- function(sample_size, beta_1, beta_2) {
  
  set.seed(392039853)
  
  reps <- 200 # Number of data sets
  N <- sample_size      # Sample size
  
  # Prepare for looping over reps
  counter <- 1
  save.ic<- matrix(data=NA, ncol=12, nrow=reps)
  
  # Loop to generate data, analyze, and save results
  for(counter in c(1:reps)){
    x1 <- runif(n=N)
    x2 <- runif(n=N)
    x3 <- runif(n=N)
    ep <- rnorm(n=N)
    y <- beta_1*x1 + beta_2*x2 + ep
    
    # Fit model "*" and store object in "reg*"
    
    reg0 <- lm(y~1) # Intercept only
    aic0 <- extractAIC(reg0,k=2)[2]
    bic0 <- extractAIC(reg0,k=log(N))[2]
    aicc0 <- aic0 + 2 * reg0$rank * (reg0$rank + 1) / (N- reg0$rank -1)
    
    reg1 <- lm(y~x1)
    aic1 <- extractAIC(reg1,k=2)[2]
    bic1 <- extractAIC(reg1,k=log(N))[2]
    aicc1 <- aic1 + 2 * reg1$rank * (reg1$rank + 1) / (N- reg1$rank -1)
    
    reg2 <- lm(y~x1 + x2)
    aic2 <- extractAIC(reg2,k=2)[2]
    bic2 <- extractAIC(reg2,k=log(N))[2]
    aicc2 <- aic2 + 2 * reg2$rank * (reg2$rank + 1) / (N- reg2$rank -1)
    
    reg3 <- lm(y~x1 + x2 + x3)
    aic3 <- extractAIC(reg3,k=2)[2]
    bic3 <- extractAIC(reg3,k=log(N))[2]
    aicc3 <- aic3 + 2 * reg3$rank * (reg3$rank + 1) / (N- reg3$rank -1)
    
    save.ic[counter,] <- c(aic0, aic1, aic2, aic3, bic0, bic1, bic2, bic3, aicc0, aicc1, aicc2, aicc3)
    counter <- counter + 1
  }
  
  # For each IC, figure out which column (model) holds the smallest value, and same model numbers
  model.aic <- table(max.col(-save.ic[,1:4]) - 1)
  model.bic <- table(max.col(-save.ic[,5:8]) - 1)
  model.aicc <- table(max.col(-save.ic[,9:12]) - 1)
  
  # Returning
  return(list(model.aic = model.aic, model.bic = model.bic, model.aicc = model.aicc))
  
}

# Getting the original
orig2_results <- mspe_q2(20, 1, 1)

# Printing Plots
par(mfrow=c(1,3))
barplot(orig2_results$model.aic,xlab="Model Number",ylab="Number chosen",main="AIC",ylim=c(0,150))
barplot(orig2_results$model.aicc,xlab="Model Number",ylab="Number chosen",main="AICc",ylim=c(0,150))
barplot(orig2_results$model.bic,xlab="Model Number",ylab="Number chosen", main="BIC",ylim=c(0,150))

# (a)
a2_results <- mspe_q2(100, 1, 1)

# Printing Plots
par(mfrow=c(1,3))
barplot(a2_results$model.aic,xlab="Model Number",ylab="Number chosen",main="AIC",ylim=c(0,150))
barplot(a2_results$model.aicc,xlab="Model Number",ylab="Number chosen",main="AICc",ylim=c(0,150))
barplot(a2_results$model.bic,xlab="Model Number",ylab="Number chosen", main="BIC",ylim=c(0,150))

# (b)
b2_results <- mspe_q2(10, 1, 1)

# Printing Plots
par(mfrow=c(1,3))
barplot(b2_results$model.aic,xlab="Model Number",ylab="Number chosen",main="AIC",ylim=c(0,150))
barplot(b2_results$model.aicc,xlab="Model Number",ylab="Number chosen",main="AICc",ylim=c(0,150))
barplot(b2_results$model.bic,xlab="Model Number",ylab="Number chosen", main="BIC",ylim=c(0,150))

# (c)
c2_results <- mspe_q2(20, 2, 1)

# Printing Plots
par(mfrow=c(1,3))
barplot(c2_results$model.aic,xlab="Model Number",ylab="Number chosen",main="AIC",ylim=c(0,150))
barplot(c2_results$model.aicc,xlab="Model Number",ylab="Number chosen",main="AICc",ylim=c(0,150))
barplot(c2_results$model.bic,xlab="Model Number",ylab="Number chosen", main="BIC",ylim=c(0,150))

# (d)
d2_results <- mspe_q2(20, 0.5, 1)

# Printing Plots
par(mfrow=c(1,3))
barplot(d2_results$model.aic,xlab="Model Number",ylab="Number chosen",main="AIC",ylim=c(0,150))
barplot(d2_results$model.aicc,xlab="Model Number",ylab="Number chosen",main="AICc",ylim=c(0,150))
barplot(d2_results$model.bic,xlab="Model Number",ylab="Number chosen", main="BIC",ylim=c(0,150))

# (e)
e2_results <- mspe_q2(20, 1, 2)

# Printing Plots
par(mfrow=c(1,3))
barplot(e2_results$model.aic,xlab="Model Number",ylab="Number chosen",main="AIC",ylim=c(0,150))
barplot(e2_results$model.aicc,xlab="Model Number",ylab="Number chosen",main="AICc",ylim=c(0,150))
barplot(e2_results$model.bic,xlab="Model Number",ylab="Number chosen", main="BIC",ylim=c(0,150))

# (f)
f2_results <- mspe_q2(20, 1, 0.5)

# Printing Plots
par(mfrow=c(1,3))
barplot(f2_results$model.aic,xlab="Model Number",ylab="Number chosen",main="AIC",ylim=c(0,150))
barplot(f2_results$model.aicc,xlab="Model Number",ylab="Number chosen",main="AICc",ylim=c(0,150))
barplot(f2_results$model.bic,xlab="Model Number",ylab="Number chosen", main="BIC",ylim=c(0,150))

# (g)
g2_results <- mspe_q2(20, 2, 2)

# Printing Plots
par(mfrow=c(1,3))
barplot(g2_results$model.aic,xlab="Model Numebr",ylab="Number chosen",main="AIC",ylim=c(0,150))
barplot(g2_results$model.aicc,xlab="Model Number",ylab="Number chosen",main="AICc",ylim=c(0,150))
barplot(g2_results$model.bic,xlab="Model Number",ylab="Number chosen", main="BIC",ylim=c(0,150))

##############
#            #
# Lecture 2b #
#            # 
##############

##############
#            #
# Question 1 #
#            # 
##############

# Reading in data
prostate <-  read.table("Prostate.csv", header=TRUE, sep=",", na.strings=" ")

subset_halves_function <- function(seed_chosen) {
  
  # Splitting data in half using random uniform selection to make two "set"s.
  set.seed(seed_chosen) 
  prostate$set <- ifelse(runif(n=nrow(prostate)) > 0.5, yes=2, no=1)
  
  
  # All subsets regression using the "regsubsets" function from "leaps"
  #  Note: default is to limit to 8-variable models.  Add nvmax argument to increase.
  allsub1 <- regsubsets(x=prostate[which(prostate$set==1),2:9], 
                        y=prostate[which(prostate$set==1),10], nbest=1)
  allsub2 <- regsubsets(x=prostate[which(prostate$set==2),2:9], 
                        y=prostate[which(prostate$set==2),10], nbest=1)
  
  # Store summary() so we can see BICs (not comparable across different data sets)
  summ.1 <- summary(allsub1)
  summ.2 <- summary(allsub2)
  
  # Fitting the models in succession from smallest to largest.  
  # Fit one-var model. then update to 2-var model.  Could keep going.
  # Each time computing sample-MSE (sMSE), BIC, and mean squared pred. error (MSPE). 
  
  results1 <- matrix(data=NA, nrow=9, ncol=4)
  mod1 <- lm(lpsa ~ 1, data=prostate[which(prostate$set==1),])
  sMSE <- summary(mod1)$sigma^2
  BIC <- extractAIC(mod1, k=log(nrow(prostate[which(prostate$set==1),])))
  pred2 <- predict(mod1, newdata=prostate[which(prostate$set==2),])
  MSPE <- mean((pred2-prostate[which(prostate$set==2),]$lpsa)^2)
  results1[1,] <- c(0, sMSE, BIC[2], MSPE)
  
  #Get rid of superfluous variables so that I can call the right variables into the data set each time.
  # Also move response to 1st column to be included every time below.
  prostate2 <- prostate[,c(10,2:9)]
  
  
  for(v in 1:8){
    mod1 <- lm(lpsa ~ ., data=prostate2[which(prostate$set==1), summ.1$which[v,]])
    sMSE <- summary(mod1)$sigma^2
    BIC <- extractAIC(mod1, k=log(nrow(prostate2[which(prostate$set==1),])))
    pred2 <- predict(mod1, newdata=prostate2[which(prostate$set==2),])
    MSPE <- mean((pred2-prostate2[which(prostate$set==2),]$lpsa)^2)
    results1[v+1,] <- c(v, sMSE, BIC[2], MSPE)
  }
  
  ##########
  # Repeat for second data set
  
  
  # Fitting the models in succession from smallest to largest.  
  # Fit one-var model. then update to 2-var model.  Could keep going.
  # Each time computing sample-MSE (sMSE), BIC, and mean squared pred. error (MSPE). 
  
  results2 <- matrix(data=NA, nrow=9, ncol=4)
  mod1 <- lm(lpsa ~ 1, data=prostate[which(prostate$set==2),])
  sMSE <- summary(mod1)$sigma^2
  BIC <- extractAIC(mod1, k=log(nrow(prostate[which(prostate$set==2),])))
  pred2 <- predict(mod1, newdata=prostate[which(prostate$set==1),])
  MSPE <- mean((pred2-prostate[which(prostate$set==1),]$lpsa)^2)
  results2[1,] <- c(0, sMSE, BIC[2], MSPE)
  
  #Get rid of superfluous variables so that I can call the right variables into the data set each time.
  # Also move response to 1st column to be included every time below.
  prostate2 <- prostate[,c(10,2:9)]
  
  
  for(v in 1:8){
    mod1 <- lm(lpsa ~ ., data=prostate2[which(prostate$set==2), summ.2$which[v,]])
    sMSE <- summary(mod1)$sigma^2
    BIC <- extractAIC(mod1, k=log(nrow(prostate2[which(prostate$set==2),])))
    pred2 <- predict(mod1, newdata=prostate2[which(prostate$set==1),])
    MSPE <- mean((pred2-prostate2[which(prostate$set==1),]$lpsa)^2)
    results2[v+1,] <- c(v, sMSE, BIC[2], MSPE)
  }
  
  # Here, I begin to organize the data as ideally, I return the table as it
  # is required in the homework. 
  
  # First, I figure out the vars chosen
  vars_chosen <- c(results1[which.min(results1[,2]), 1],
                   results1[which.min(results1[,3]), 1],
                   results1[which.min(results1[,4]), 1],
                   results2[which.min(results2[,2]), 1],
                   results2[which.min(results2[,3]), 1],
                   results2[which.min(results2[,4]), 1])
  
  # Now I do the same for the training error which is the sMSE
  train_error <- c(results1[which.min(results1[,2]), 2],
                   results1[which.min(results1[,3]), 2],
                   results1[which.min(results1[,4]), 2],
                   results2[which.min(results2[,2]), 2],
                   results2[which.min(results2[,3]), 2],
                   results2[which.min(results2[,4]), 2])
  
  # Lastly, I do the same to find the test error (MSPE)
  test_error <- c(results1[which.min(results1[,2]), 4],
                   results1[which.min(results1[,3]), 4],
                   results1[which.min(results1[,4]), 4],
                   results2[which.min(results2[,2]), 4],
                   results2[which.min(results2[,3]), 4],
                   results2[which.min(results2[,4]), 4])
  
  var_names <- c(paste(names(which(summ.1$which[vars_chosen[1], -1] == TRUE)), collapse = ", "),
                 paste(names(which(summ.1$which[vars_chosen[2], -1] == TRUE)), collapse = ", "),
                 paste(names(which(summ.1$which[vars_chosen[3], -1] == TRUE)), collapse = ", "),
                 paste(names(which(summ.2$which[vars_chosen[4], -1] == TRUE)), collapse = ", "),
                 paste(names(which(summ.2$which[vars_chosen[5], -1] == TRUE)), collapse = ", "),
                 paste(names(which(summ.2$which[vars_chosen[6], -1] == TRUE)), collapse = ", "))
  
  
  # Now put it all together
  final_table <- data.frame(training_set = c(rep(1, 3), rep(2, 3)),
                            criterion = c(rep(c("sMSE", "BIC", "MSPE"), 2)),
                            num_vars = vars_chosen,
                            vars_chosen = var_names,
                            training_error = train_error,
                            test_error = test_error)
  
  return(final_table)
  
}

##### (a) #####

# Running code
subset_halves_function(120401002)

##### (b) #####

# Running code
subset_halves_function(9267926)

##### (c) #####

## (i) ##

## (ii) ##


##############
#            #
# Question 2 #
#            # 
##############

# Reading in Data
abalone <- read.csv("Abalone.csv", as.is = T, header = T)

# Looking at data
head(abalone)
str(abalone)

# Creating male/female variable [assuming 0 = male and 1 = female]
str(abalone$Sex)
abalone$male <- ifelse(abalone$Sex == 0, 1, 0)
abalone$female <- ifelse(abalone$Sex == 1, 1, 0)

# Dropping sex variable
abalone <- abalone[,-1]

# Looking at data again
str(abalone)

##### (a) #####

# Creating scatterplot of all variable
ggpairs(abalone)

## Here, we can see that the variable shell seems to have the strongest correlation
## with the rings (our response) variable. Additionally, there seems to be a moderate
## correlation with a numebr of other variables such as length, diameter, height, whole,
## and viscera. In fact, most variables exhibit some mild correlation. 

## With respect to multicollinearity, there is a large potential for this issue. In fact,
## we see a strong correlation between length and a number of other variables such as diameter,
## height, whole, shucked, and viscera. This rings true as well for the relationship between
## these variables with each other as well. The potential for this issue is clearly evident
## from the pairs plot. 

## (i) ##

# Fixing the height variable
abalone <- abalone[(0 < abalone$Height)&(abalone$Height < 0.5), ]

# Let's look at the pairs plot again to see that Height has been "fixed"
ggpairs(abalone)

##### (b) #####

# Setting seed
set.seed(29003092)

# Creating data sets
abalone$set <- ifelse(runif(n=nrow(abalone)) <= 0.75, yes = 1, no = 2)
abalone_1 <- abalone[which(abalone$set == 1), -11]
abalone_2 <- abalone[which(abalone$set == 2), -11]

##### (c) #####

# Creating lm object [minimal model]
fit <- lm(Rings ~ 1, data = abalone_1)

# Doing forward stepwise regression
step(fit, 
        direction = "forward", 
        scope = (~ Length + Diameter + Height + Whole + Shucked + Viscera + Shell + male + female),
        k = log(nrow(abalone_1)))

## The variables are: Shell, Shucked, Height, male, Whole, Viscera, and Diameter

##### (d) #####

# Creating lm object [minimal model]
fit <- lm(Rings ~ 1, data = abalone_1)

# Doing forward stepwise regression
step(fit, 
        direction = "both", 
        scope = (~ Length + Diameter + Height + Whole + Shucked + Viscera + Shell + male + female),
        k = log(nrow(abalone_1)))

##### (e) #####

# Creating lm object [minimal model]
fit <- lm(Rings ~ 1, data = abalone_1)

# Doing forward stepwise regression
step_no_penalty <- step(fit, 
                      direction = "forward", 
                      scope = (~ Length + Diameter + Height + Whole + 
                                 Shucked + Viscera + Shell + male + female),
                      k = 0)

# Penalty term to be added:
BIC_penalty <- function(k) {return(log(nrow(abalone))*k)}

# BIC Values
BIC_values <- c(7424.8, 5876.02, 5423.15, 5215.6, 5141.32, 5079.17, 
                5015.91, 4972.63, 4971.12, 4970.62)

# Getting true BIC values
true_BIC <- c()
for (i in 1:10) {
  true_BIC[i] <- BIC_values[i] + BIC_penalty(i - 1)
}

# Looking at true values
true_BIC

# Finding true minimum model
which.min(true_BIC)
## 8th model has the lowest BIC value with the appropriate error term

## The 8th model which is as follows: Rings ~ Shell + Shucked + Height + male + Whole + Viscera + Diameter
## which is the same as the stepwise selection in the beginning

##### (f) #####

# Running all subsets regression
allsub_training <- regsubsets(x = abalone_1[,c(1:7, 9:10)],
                             y = abalone_1[,8],
                             nbest = 1)

## (i) ##
summary_training <- summary(allsub_training)
summary_training$bic

# Best model has 7 variables - They are the same as the model from befor
# but strangely enough, the BIC values look different hm...
``
## (ii) ##
plot(allsub_training)

##### (g) #####







