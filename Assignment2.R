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
library(relaxo)
library(gridExtra)

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
  set.seed(seed_picked) 
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
  
  return(list(results = df_a, 
              coef_model1 = coef(cv.lasso.1), 
              coef_model2 = coef(cv.lasso.2)))
}

# Part (a) and (b)
LASSO_func(120401002)

##### Question 2 #####


##### Question 3 #####
LASSO_func(9267926)

##### Question 4 #####
LASSO_relax_func <- function(seed_picked){
  
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
  sMSE_1 <- mean((predrely.1.1 - y.1)^2)
  
  predrel.1.2 <- predict(cv.relaxo.1, 
                         newX=x.2s1, 
                         lambda = cv.relaxo.1$lambda, 
                         phi = cv.relaxo.1$phi)
  predrely.1.2 <- predrel.1.2*sd(y.1) + mean(y.1)
  MSPE_1 <- mean((predrely.1.2 - y.2)^2)
  
  # Second half
  predrel.2.2 <- predict(cv.relaxo.2, 
                         newX=x.2s2, 
                         lambda = cv.relaxo.2$lambda, 
                         phi = cv.relaxo.2$phi)
  predrely.2.2 <- predrel.2.2*sd(y.2) + mean(y.2)
  sMSE_2 <- mean((predrely.2.2 - y.2)^2)
  
  predrel.2.1 <- predict(cv.relaxo.2, 
                         newX=x.1s2, 
                         lambda = cv.relaxo.2$lambda, 
                         phi = cv.relaxo.2$phi)
  predrely.2.1 <- predrel.2.1*sd(y.2) + mean(y.2)
  MSPE_2 <- mean((predrely.2.1 - y.1)^2)
  
  df <- data.frame(Model = c("Training Set 1", "Training Set 2"),
                   Lambda = c(cv.relaxo.1$lambda, cv.relaxo.2$lambda),
                   Phi = c(cv.relaxo.1$phi, cv.relaxo.2$phi),
                   Train_Error = c(sMSE_1, sMSE_2),
                   Test_Error = c(MSPE_1, MSPE_2))
  
  
  return(df)
}

# 4
LASSO_relax_func(120401002)
LASSO_relax_func(9267926)

# 5

# Reading in data
abalone <- read.csv("Abalone.csv", as.is = T, header = T)

# Creating function
LASSO_func_abalone <- function(seed_picked){

    # Splitting data in half using random uniform selection to make two "set"s.
    set.seed(seed_picked) 
    
    # Creating male/female variable [assuming 0 = male and 1 = female]
    abalone$male <- ifelse(abalone$Sex == 1, 1, 0)
    abalone$female <- ifelse(abalone$Sex == 2, 1, 0)
    
    # Dropping sex variable
    abalone <- abalone[,-1]
    
    # Fixing height
    abalone <- abalone[(0 < abalone$Height)&(abalone$Height < 0.5), ]
    
    # Data set
    abalone$set <- ifelse(runif(n=nrow(abalone)) <= 0.75, yes = 1, no = 2)
    
    # Setting up data
    y.1 <- abalone[which(abalone$set==1),8]
    x.1 <- as.matrix(abalone[which(abalone$set==1),c(1:7, 9, 10)])
    xs.1 <- scale(x.1)
    y.2 <- abalone[which(abalone$set==2),8]
    x.2 <- as.matrix(abalone[which(abalone$set==2),c(1:7, 9, 10)])
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
                                 "Training set 1, 1se"),
                       Lambda = c(cv.lasso.1$lambda.min,
                                  cv.lasso.1$lambda.1se),
                       Training_Error = c(sMSE.lasso_1_min,
                                          sMSE.lasso_1_1se),
                       Test_Error = c(MSPE.lasso_1_min,
                                      MSPE.lasso_1_1se))
    
    return(list(results = df_a, 
                coef_model1 = coef(cv.lasso.1, s = cv.lasso.1$lambda.min), 
                coef_model2 = coef(cv.lasso.1, s = cv.lasso.1$lambda.1se)))
}

# Running Function  
LASSO_func_abalone(29003092)

# 6
all_model_function <- function(){
  
  #################### DATA SET UP ######################
  
  # Reading in data
  abalone <- read.csv("Abalone.csv", as.is = T, header = T)
  
  # Creating male/female variable [assuming 0 = male and 1 = female]
  abalone$male <- ifelse(abalone$Sex == 1, 1, 0)
  abalone$female <- ifelse(abalone$Sex == 2, 1, 0)
  
  # Dropping sex variable
  abalone <- abalone[,-1]
  
  # Fixing height
  abalone <- abalone[(0 < abalone$Height)&(abalone$Height < 0.5), ]
  
  # Data set
  abalone$set <- ifelse(runif(n=nrow(abalone)) <= 0.75, yes = 1, no = 2)
  
  ############################ REGRESSION ################################
  
  # Getting proper data split
  abalone_1 <- abalone[which(abalone$set == 1), -11]
  abalone_2 <- abalone[which(abalone$set == 2), -11]
  
  # Running model
  fit <- lm(Rings ~ ., data = abalone_1)
  
  # sMSE
  pred_reg_train <- predict(fit, abalone_1)
  # sMSE_reg <- sum((pred_reg_train - abalone_1$Rings)^2)/fit$df.residual
  sMSE_reg <- mean((pred_reg_train - abalone_1$Rings)^2)
  
  # MSPE
  pred_reg_test <- predict(fit, abalone_2)
  MSPE_reg <- mean((pred_reg_test - abalone_2$Rings)^2)
  
  ############################ ALL SUBSETS ################################
  
  # Running all subsets regression
  allsub_training <- regsubsets(x = abalone_1[,c(1:7, 9:10)],
                                y = abalone_1[,8],
                                nbest = 1,
                                nvmax = 9)
  
  # Summarizing
  summary_training <- summary(allsub_training)
  
  # Getting appropriate variables
  explan_vars <- paste(names(which(summary_training$outmat[which.min(summary_training$bic),] == "*")), 
            collapse = " + ")
  
  # Creating form
  formula_subsets = as.formula(paste("Rings", explan_vars, sep = " ~ "))
  
  # Creating fit
  best_subsets <- lm(formula_subsets, data = abalone_1)
  
  # sMSE
  pred_subsets_train <- predict(best_subsets, abalone_1)
  sMSE_subset <- sum((pred_subsets_train - abalone_1$Rings)^2)/best_subsets$df.residual
  
  # MSPE
  pred_subsets_test <- predict(best_subsets, abalone_2)
  MSPE_subset <- mean((pred_subsets_test - abalone_2$Rings)^2)
  
  ############################ LASSO ################################
  
  # Setting up data
  y.1 <- abalone[which(abalone$set==1),8]
  x.1 <- as.matrix(abalone[which(abalone$set==1),c(1:7, 9, 10)])
  xs.1 <- scale(x.1)
  y.2 <- abalone[which(abalone$set==2),8]
  x.2 <- as.matrix(abalone[which(abalone$set==2),c(1:7, 9, 10)])
  xs.2 <- scale(x.2)
  
  # cv
  cv.lasso.1 <- cv.glmnet(y=y.1, x= x.1, family="gaussian")
  
  # Now predicting
  predict_lasso_min_smse <- predict(cv.lasso.1, newx = x.1, s = cv.lasso.1$lambda.min)
  predict_lasso_1se_smse <- predict(cv.lasso.1, newx = x.1, s = cv.lasso.1$lambda.1se)
  sMSE_Lasso_min <- mean((y.1 - predict_lasso_min_smse)^2)
  sMSE_Lasso_1se <- mean((y.1 - predict_lasso_1se_smse)^2)
  
  
  predict_lasso_min_mspe <- predict(cv.lasso.1, newx = x.2, s = cv.lasso.1$lambda.min)
  predict_lasso_1se_mspe <- predict(cv.lasso.1, newx = x.2, s = cv.lasso.1$lambda.1se)
  MSPE_Lasso_min <- mean((y.2 - predict_lasso_min_mspe)^2)
  MSPE_Lasso_1se <- mean((y.2 - predict_lasso_1se_mspe)^2)
  
  
  ################### RELAXED LASSO ######################
  
  # Setting up data
  y.1 <- abalone[which(abalone$set==1),8]
  x.1 <- as.matrix(abalone[which(abalone$set==1),c(1:7, 9, 10)])
  y.2 <- abalone[which(abalone$set==2),8]
  x.2 <- as.matrix(abalone[which(abalone$set==2),c(1:7, 9, 10)])
  
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
  
  # Look atresults over values of lambda and phi
  # Use crossvalidation to select "optimal" lambda and phi
  cv.relaxo.1 <- cvrelaxo(Y=y.1s1, X= x.1s1)
  
  # Predicting
  pred_smse_relax <- predict(cv.relaxo.1, 
                         newX=x.1s1, 
                         lambda = cv.relaxo.1$lambda, 
                         phi = cv.relaxo.1$phi)
  pred_smse_relax <- pred_smse_relax*sd(y.1) + mean(y.1)
  sMSE_Relax_Lasso <- mean((pred_smse_relax - y.1)^2)
  
  pred_mspe_relax <- predict(cv.relaxo.1, 
                         newX=x.2s1, 
                         lambda = cv.relaxo.1$lambda, 
                         phi = cv.relaxo.1$phi)
  pred_mspe_relax <- pred_mspe_relax*sd(y.1) + mean(y.1)
  MSPE_Relax_Lasso <- mean((pred_mspe_relax - y.2)^2)
  
  ################### RETURNING APPROPRIATE THINGS ######################
  
  sMSE_Returns <- c(sMSE_reg, sMSE_subset, sMSE_Lasso_min, sMSE_Lasso_1se, sMSE_Relax_Lasso)
  MSPE_Returns <- c(MSPE_reg, MSPE_subset, MSPE_Lasso_min, MSPE_Lasso_1se, MSPE_Relax_Lasso)
  
  return(list(sMSE = sMSE_Returns, MSPE = MSPE_Returns))
  
}

# Now, I Set the seed and number of loops
set.seed (890987665)
R = 20

# Initializing matrices
sMSE <- matrix(NA , nrow =R, ncol = 5)
colnames(sMSE) <- c("OLS", "ALLBIC", "LASSOMIN" ,
                        "LASSO1SE", "RELAX")
MSPE <- matrix (NA, nrow =R, ncol =5)
colnames(MSPE) <- colnames (sMSE)

# Now, I loops
for (i in 1:R) {
  temp <- all_model_function()
  sMSE[i,] <- temp$sMSE
  MSPE[i,] <- temp$MSPE
}

sMSE
MSPE

# 6a

# Getting means
sMSE_means <- colMeans(sMSE)
MSPE_means <- colMeans(MSPE)

# Getting CIs
lower_ci_sMSE <- sMSE_means - 1.96*(apply(sMSE, 2, function(x){return(sd(x))})/sqrt(20))
upper_ci_sMSE <- sMSE_means + 1.96*(apply(sMSE, 2, function(x){return(sd(x))})/sqrt(20))
lower_ci_MSPE <- MSPE_means - 1.96*(apply(MSPE, 2, function(x){return(sd(x))})/sqrt(20))
upper_ci_MSPE <- MSPE_means + 1.96*(apply(MSPE, 2, function(x){return(sd(x))})/sqrt(20))

# Putting on to table
q6_a <- data.frame(Model = c("OLS", "Allsub/BIC", "LASSO_min", "LASSO_1se", "LASSO_Relax"),
                   Train_Mean = sMSE_means,
                   Train_Lower_CI = lower_ci_sMSE,
                   Train_Upper_CI = upper_ci_sMSE,
                   Test_Mean = MSPE_means,
                   Test_Lower_CI = lower_ci_MSPE,
                   Test_Upper_CI = upper_ci_MSPE)

# 6b

# First, I square root the values
sqrt_sMSE <- data.frame(sqrt(sMSE))
sqrt_MSPE <- data.frame(sqrt(MSPE))
colnames(sqrt_sMSE) <- c("OLS", "Allsub/BIC", "LASSO_min", "LASSO_1se", "LASSO_Relax") 
colnames(sqrt_MSPE) <- c("OLS", "Allsub/BIC", "LASSO_min", "LASSO_1se", "LASSO_Relax") 


# Now creating box plots
train_box <- ggplot(stack(sqrt_sMSE), aes(x = ind, y = values)) +
  geom_boxplot() + 
  theme_bw() + 
  xlab("Model") +
  ylab("Training Error") +
  scale_y_continuous(limits = c(2, 2.5))
  
test_box <- ggplot(stack(sqrt_MSPE), aes(x = ind, y = values)) +
  geom_boxplot() + 
  theme_bw() + 
  xlab("Model") +
  ylab("Test Error") +
  scale_y_continuous(limits = c(2, 2.5))

grid.arrange(train_box, test_box, nrow = 1)

# 6c

# Finding minimum error per split
mins_mspe <- apply(X=sqrt(MSPE), MARGIN=1, FUN=min)

# Dividing
MSPE_div <- matrix(nrow = R, ncol = 5)
for (i in 1:R) {
  MSPE_div[i, ] <- sqrt(MSPE)[i,]/mins_mspe[i]
}

MSPE_div_frame <- data.frame(MSPE_div)
colnames(MSPE_div_frame) <- c("OLS", "Allsub/BIC", "LASSO_min", "LASSO_1se", "LASSO_Relax")

# Now creating boxplot
box_6c <- ggplot(stack(MSPE_div_frame), aes(x = ind, y = values)) +
  geom_boxplot() + 
  theme_bw() + 
  xlab("Model") +
  ylab("Test Error [With Rescale]") 

