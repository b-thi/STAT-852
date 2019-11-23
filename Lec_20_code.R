################################################################################
#                                                                              #
#                                                                              #
#                Lecture 20 - Barinder Thind - Matt Berkowitz                  #
#                                                                              #
#                                                                              #
################################################################################

#############
#           #
# Libraries #
#           # 
#############

library(tidyverse)
library(gbm)
library(future.apply)
  
##############
#            #
# Lecture 20 #
#            # 
##############

# Reading in data
wheat <- read.csv("wheat.csv", as.is = T, header = T)
wheat$class <- as.factor(wheat$class)
wheat$type <- as.factor(wheat$type)

# Setting up data
set.seed(67982193)
perm <- sample(x = nrow(wheat))

# Creating numerical variable
# wheat$classnum <- as.numeric(as.factor(wheat$class))

# Splitting
set1 <- wheat[which(perm <= 200), -1]
set2 <- wheat[which(perm > 200), -1]

### (a) ###

# Setting up functions before hand
gbm_bootstrap_func_lec20 = function(df, r){
  
  # Library
  library(gbm)
  
  # Creating data frame of tuning parameters
  tuning_par <- expand.grid(num_trees = c(100, 200, 500, 1000, 2000, 5000),
                            depth = 1:7,
                            shrinkage = c(0.001, 0.005, 0.01, 0.05, 0.1, 1, 2),
                            bag.fraction =  c(0.3, 0.5, 0.7, 1),
                            num_nodes = c(5, 7, 9, 11))
  
  # Setting cores
  plan(multiprocess, workers = 8)
  
  # Initializing
  MSPR_values <- matrix(nrow = r, ncol = nrow(tuning_par))
  MSE_values <- matrix(nrow = r, ncol = nrow(tuning_par))
  
  # Looping
  for (j in 1:r) {
    
    # setting seed
    set.seed(j)
    
    # Setting up data
    resamp <- sample.int(n=nrow(set1), size=nrow(set1), replace=TRUE)
    newdat <- set1[resamp,]
    newdat2 <- set1[-resamp,]
    
    # Running through apply
    results_gbm <- future_apply(tuning_par, 1, function(x){
      
      # Run nnet and get MSPE and MSE from run
      wheat.gbm <- gbm(data = newdat, 
                       type ~ ., 
                       distribution="multinomial", 
                       n.trees = x[1], 
                       interaction.depth = x[2], 
                       shrinkage = x[3], 
                       bag.fraction = x[4],
                       n.minobsinnode = x[5])
      
      # Save results in new column of matrix
      pred_def_smse <- apply(predict(wheat.gbm, 
                                     newdata = newdat, 
                                     n.trees = wheat.gbm$n.trees,
                                     type = "response"), 1, which.max)
      pred_def_smse_class <- ifelse(pred_def_smse == 1, "Healthy", ifelse(pred_def_smse == 2,
                                                                          "Scab", "Sprout"))
      
      pred_def_mspe <- apply(predict(wheat.gbm, 
                                     newdata = newdat2, 
                                     n.trees = wheat.gbm$n.trees,
                                     type = "response"), 1, which.max)
      pred_def_mspe_class <- ifelse(pred_def_mspe == 1, "Healthy", ifelse(pred_def_mspe == 2,
                                                                          "Scab", "Sprout"))
      
      # Errors
      MSPR <- 1 - mean((newdat2$type == pred_def_mspe_class)^2)
      MSE <- 1 - mean((newdat$type == pred_def_smse_class)^2)
      
      # Putting together
      df_returned <- data.frame(Trees = x[1], Depth = x[2], 
                                Shrinkage = x[3], Bag_Fraction = x[4],
                                Node_Size = x[5],
                                MSE = MSE, MSPE = MSPR)
      
      rownames(df_returned) <- NULL
      
      # Returning
      return(df_returned)
      
    })
    
    # Putting together results
    q1_errors <- do.call(rbind, results_gbm)
    
    # Storing
    MSPR_values[j,] <- q1_errors$MSPE
    MSE_values[j,] <- q1_errors$MSE
  }
  
  return(list(MSPR = MSPR_values,
              MSE = MSE_values))
}

lec20_gbm <- function(){
  
  
  ########################### GBM ##################################
  
  # Default Model
  wheat.gbm_def <- gbm(data = set1, 
                       type ~ ., 
                       distribution="multinomial")
  
  # Default Model Predictions
  pred_def_smse <- apply(predict(wheat.gbm_def, 
                                 newdata = set1, 
                                 n.trees = wheat.gbm_def$n.trees,
                                 type = "response"), 1, which.max)
  pred_def_smse_class <- ifelse(pred_def_smse == 1, "Healthy", ifelse(pred_def_smse == 2,
                                                                      "Scab", "Sprout"))
  
  sMSE_def_gbm <- 1 - mean((set1$type == pred_def_smse_class)^2)
  
  pred_def_mspe <- apply(predict(wheat.gbm_def, 
                                 newdata = set2, 
                                 n.trees = wheat.gbm_def$n.trees,
                                 type = "response"), 1, which.max)
  pred_def_mspe_class <- ifelse(pred_def_mspe == 1, "Healthy", ifelse(pred_def_mspe == 2,
                                                                      "Scab", "Sprout"))
  
  MSPE_def_gbm <- 1 - mean((set2$type == pred_def_mspe_class)^2)
  
  # Tuned Model
  tuned_results <- gbm_bootstrap_func_lec20(set1, 1)
  
  # Creating data frame of tuning parameters
  tuning_par <- expand.grid(num_trees = c(100, 200, 500, 1000, 2000, 5000),
                            depth = 1:7,
                            shrinkage = c(0.001, 0.005, 0.01, 0.05, 0.1, 1, 2),
                            bag.fraction =  c(0.3, 0.5, 0.7, 1),
                            num_nodes = c(5, 7, 9, 11))
  
  # Getting best
  best_par <- tuning_par[which.min(colMeans(tuned_results$MSPR)),]
  
  # printing
  print(best_par)
  
  # Building model
  wheat.gbm <- gbm(data = set1, 
                   type ~ ., 
                   distribution="multinomial", 
                   n.trees = best_par[1], 
                   interaction.depth = best_par[2], 
                   shrinkage = best_par[3], 
                   bag.fraction = best_par[4],
                   n.minobsinnode = best_par[5])
  
  # Tuned Model Predictions
  pred_def_smse <- apply(predict(wheat.gbm, 
                                 newdata = set1, 
                                 n.trees = wheat.gbm$n.trees,
                                 type = "response"), 1, which.max)
  pred_def_smse_class <- ifelse(pred_def_smse == 1, "Healthy", ifelse(pred_def_smse == 2,
                                                                      "Scab", "Sprout"))
  
  sMSE_tuned_gbm <- 1 - mean((set1$type == pred_def_smse_class)^2)
  
  pred_def_mspe <- apply(predict(wheat.gbm, 
                                 newdata = set2, 
                                 n.trees = wheat.gbm$n.trees,
                                 type = "response"), 1, which.max)
  pred_def_mspe_class <- ifelse(pred_def_mspe == 1, "Healthy", ifelse(pred_def_mspe == 2,
                                                                      "Scab", "Sprout"))
  
  MSPE_tuned_gbm <- 1 - mean((set2$type == pred_def_mspe_class)^2)
  
  
  
  ################### RETURNING APPROPRIATE THINGS ######################
  
  return(list(gbm_sMSE_def = sMSE_def_gbm,
              gbm_sMSE_tuned = sMSE_tuned_gbm,
              gbm_MSPE_def = MSPE_def_gbm,
              gbm_MSPE_tuned = MSPE_tuned_gbm))
  
}

# Now, I Set the seed and number of loops
set.seed (890987665)
R = 1

# Initializing matrices
sMSE_lec20_gbm <- matrix(NA , nrow = R, ncol = 2)
colnames(sMSE_lec20_gbm) <- c("def_gbm", "tuned_gbm")
MSPE_lec20_gbm <- matrix (NA, nrow = R, ncol = 2)
colnames(MSPE_lec20_gbm) <- colnames (sMSE_lec20_gbm)

# Now, I loops
for (i in 1:R) {
  temp <- lec20_gbm()
  sMSE_lec20_gbm[i,] <- c(temp$gbm_sMSE_def, temp$gbm_sMSE_tuned)
  MSPE_lec20_gbm[i,] <- c(temp$gbm_MSPE_def, temp$gbm_MSPE_tuned)
}

# # Creating data frame of tuning parameters
# tuning_par <- expand.grid(num_trees = c(100, 200, 500, 1000, 2000),
#                           depth = 2:7,
#                           shrinkage = c(0.001, 0.005, 0.01, 0.05, 0.1, 1),
#                           bag.fraction =  c(0.3, 0.5, 0.7, 1),
#                           num_nodes = c(5, 7, 9, 11))
# 
# 
# # Running through apply
# results_gbm <- future_apply(tuning_par, 1, function(x){
#   
#   # Run nnet and get MSPE and MSE from run
#   wheat.gbm <- gbm(data = set1, 
#                    type ~ ., 
#                    distribution="multinomial", 
#                    n.trees = x[1], 
#                    interaction.depth = x[2], 
#                    shrinkage = x[3], 
#                    bag.fraction = x[4],
#                    n.minobsinnode = x[5])
#   
#   # Save results in new column of matrix
#   pred_def_smse <- apply(predict(wheat.gbm, 
#                                  newdata = set1, 
#                                  n.trees = wheat.gbm$n.trees,
#                                  type = "response"), 1, which.max)
#   pred_def_smse_class <- ifelse(pred_def_smse == 1, "Healthy", ifelse(pred_def_smse == 2,
#                                                                       "Scab", "Sprout"))
#   
#   pred_def_mspe <- apply(predict(wheat.gbm, 
#                                  newdata = set2, 
#                                  n.trees = wheat.gbm$n.trees,
#                                  type = "response"), 1, which.max)
#   pred_def_mspe_class <- ifelse(pred_def_mspe == 1, "Healthy", ifelse(pred_def_mspe == 2,
#                                                                       "Scab", "Sprout"))
#   
#   # Errors
#   MSPR <- 1 - mean((set2$type == pred_def_mspe_class)^2)
#   MSE <- 1 - mean((set1$type == pred_def_smse_class)^2)
#   
#   # Putting together
#   df_returned <- data.frame(Trees = x[1], Depth = x[2], 
#                             Shrinkage = x[3], Bag_Fraction = x[4],
#                             Node_Size = x[5],
#                             MSE = MSE, MSPE = MSPR)
#   
#   rownames(df_returned) <- NULL
#   
#   # Returning
#   return(df_returned)
#   
# })
# 
# # Putting together results
# q1_errors <- do.call(rbind, results_gbm)
# 
# # Getting best
# best_par <- tuning_par[which.min(q1_errors$MSPE),]

# The best tuning parameters after running this were: 
# n.trees = 200, interaction.depth = 6, shrinkage = 0.1, bag.fraction = 1, n.minobsinnode = 5

# WRITE UP: Here, we needed to find a way to tune the gradient boosting model. We had 5
# parameters total. We decided to bootstrap (with 5 bootstraps) and then figure out which
# tuning parameter combination was the best. We set it up so that this could be done over
# repeated runs (although this wasn't neccesary for this question). We set up a massive
# grid over which to search - since we were just doing 1 run (as opposed to the usual 20),
# we had some freedom in how large we could make this grid. So, essentially, we print
# out the best parameters for this run here, but this choice is made over 5 bootstrap
# samples (over these 5, each combination is trained). The best parameters are listed above.
# The validation error is computed with "set2" which isn't used at all during the tuning
# process (as the bootstrap samples are made entirely on set1). We also looked at the default
# error to confirm that tuning is indeed improving the result.

### (b) ###

# Fitting best model
wheat.gbm_best <- gbm(data = set1, 
                 type ~ ., 
                 distribution="multinomial", 
                 n.trees = 200, 
                 interaction.depth = 6, 
                 shrinkage = 0.1, 
                 bag.fraction = 1,
                 n.minobsinnode = 5)

# Plots
imp_summary <- summary(wheat.gbm_best)

# WRITE UP: hahahhahahah

### (c) ###

# Test error
pred_def_mspe2 <- apply(predict(wheat.gbm_best, 
                               newdata = set2, 
                               n.trees = wheat.gbm_best$n.trees,
                               type = "response"), 1, which.max)

pred_def_mspe_class2 <- colnames(predict(wheat.gbm_best, 
                                        newdata = set2, 
                                        n.trees = wheat.gbm_best$n.trees,
                                        type = "response"))[pred_def_mspe2]

MSPE_tuned_gbm <- 1 - mean((set2$type == pred_def_mspe_class2)^2)

# Confusion matrix - polynomial
table(set2$type, pred_def_mspe_class2, dnn=c("Obs","Pred"))

# WRITE UP: Here, we can observe a very similar pattern to what we had before. Again, we see
# that all these methods are having trouble distinguishing between the healthy and the sprout
# type and we see this especially pronounced here with the healthy class being misclassified
# as sprout 10 times. Scab again is classified pretty well although there is some overlap
# with sprout. The error rate here of 43% is not as great as some other approaches we have
# seen thus far (for example, svm, and lda both outperform boosting for this data set).
