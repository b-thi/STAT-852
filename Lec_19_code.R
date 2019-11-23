################################################################################
#                                                                              #
#                                                                              #
#                Lecture 19 - Barinder Thind - Dylan Maciel                    #
#                                                                              #
#                                                                              #
################################################################################

#############
#           #
# Libraries #
#           # 
#############

library(tidyverse)
library(rpart)
library(rpart.plot)
library(randomForest)
library(future.apply)

##############
#            #
# Lecture 19 #
#            # 
##############

# Reading in data
wheat <- read.csv("wheat.csv", as.is = T, header = T)

# Setting up data
set.seed(67982193)
perm <- sample(x = nrow(wheat))

# Splitting
set1 <- wheat[which(perm <= 200), -1]
set2 <- wheat[which(perm > 200), -1]

### (a) ###

# Splitting again
perm2 <- sample(x = nrow(set1))
set1_train <- set1[which(perm2 <= 150),]
set1_test <- set1[which(perm2 > 150),]


# Creating tree
wheat.tree <- rpart(data = set1_train, type ~ ., method="class", cp=0.001)

# Finding optimal cp's for min and 1se pruning
val.tune <- function(obj, valid, G, grid) {
  cp <- matrix(0, ncol=2, nrow=grid)
  for (x in c(1:grid)){
    cp[x,1] <- x/grid  
    pred <- predict(prune(obj, cp=x/grid), newdata = valid, type="class")
    cp[x,2] <- mean(ifelse(pred == G, yes=0, no=1))
  }
  cp
}

# Running function
wheat.valtree <- val.tune(obj = wheat.tree, valid = set1_test, G = set1_test$type, grid=1000)

# Returns optimal cp and misclassification rate there.
optimal_cp <- wheat.valtree[which.min(wheat.valtree[,2]), ]

# Pruning

# Creating a pruned tree using a selected value of the CP by CV.
wheat.prune.cv.1se <- prune(wheat.tree, cp=optimal_cp[1])

# Creating a pruned tree using a selected value of the CP by CV.
wheat.prune.cv.min <- prune(wheat.tree, cp=optimal_cp[2])

# Plotting the trees
par(mfrow=c(1,3))
prp(wheat.tree, type=1, extra=1, main="Non-Pruned tree")
prp(wheat.prune.cv.1se, type=1, extra=1, main="Pruned CV-1SE tree")
prp(wheat.prune.cv.min, type=1, extra=1, main="Pruned CV-min tree")
par(mfrow=c(1,1))

# WRITE UP: Here, we can see that, as expected, the tree with the least splits is the one using
# the "min" cp. In fact, this tree only has 2 splits and we see that the most important variables
# on which the splits are made is weight and density. This is consistent with what we have seen
# before in previous methods where these two variables were seen as the most important when compared
# with the others (such as when we did the likelihood ratio test in lecture 17 assignment). The
# splits indicate that "healthy" class predictions made are using both splits so both weight and
# density are important for that class. We also see that separation between sprout and scab
# is made with the weight variable (when it's less than 21, we get scab predictions) - this results
# in a few misclassifications but most of them come in the second split. This is again consistent
# with what we have seen before (from confusion matrices) where the main issue of the models seems
# to be when they are trying to separate between sprout and healthy types.

# Prediction Errors
MSPE_tree_no_prune <- mean(predict(wheat.tree, newdata = set2, type="vector") == as.numeric(as.factor(set2$type)))
MSPE_tree_prune_1se <- mean(predict(wheat.prune.cv.1se, newdata = set2, type="vector") == as.numeric(as.factor(set2$type)))
MSPE_tree_prune_min <- mean(predict(wheat.prune.cv.min, newdata = set2, type="vector") == as.numeric(as.factor(set2$type)))

### (b) ###

# Creating grid to tune over
tuning_par <- expand.grid(c(1:(ncol(set1) - 1)))
colnames(tuning_par) <- c("mtry")

# Parallel applying
plan(multiprocess, workers = 8)

# Creating factors
set1_f <- set1
set1_f$class <- as.factor(set1_f$class)
set1_f$type <- as.factor(set1_f$type)

# Running through apply
tuning_rf <- future_apply(tuning_par, 1, function(x){
  
  # Running Cross Validations
  rf_model <- randomForest(type ~ ., data = set1_f,
                           mtry = x[1])
  
  # Getting predictions
  OOB_error = mean((predict(rf_model) == set1$type))
  
  # Putting together
  df_returned <- data.frame(mtry = x[1], OOB_error = OOB_error)
  
  # Returning
  return(df_returned)
  
})

# Putting together results
tuning_rf_results <- do.call(rbind, tuning_rf)

# Saving Errors
sMSE_rf_best <- tuning_rf_results[which.max(tuning_rf_results$OOB_error), ]

# Getting MSPE
final_rf <- randomForest(type ~ ., data = set1_f,
                         mtry = 4,
                         importance = T)

# mtry = 4
varImpPlot(final_rf)

# WRITE UP: Again, as we have seen with all the methods thus far, the variables weight and
# density are the most important here, showing up most importance in terms of mean decrease
# in accuracy and when we look at the MeanDecreaseGini. The variable moisture seems to be the
# clear winner as far as being the third most important variable. Class seems to be the least
# important variable here. The mean decrease gini here means ________________________________
# ___________________________________________________________________________________________

# Setting up test error
set2_f <- set2
set2_f$class <- as.factor(set2_f$class)
set2_f$type <- as.factor(set2_f$type)

# Predicting for MSPE
MSPE_rf <- mean((predict(final_rf, newdata = set2_f) == set2_f$type)^2)

### (c) ###

# Looking at number of trees of the given model
hist(treesize(final_rf))

# WRITE UP: Here, we can see that the number of trees on average, is around 35. Next, I will
# run this with various node sizes and see what happens to the error rates. 

# Creating grid to tune over
tuning_par <- expand.grid(nodes = seq(5, 100, 5))

# Running through apply
tree_size_rf <- future_apply(tuning_par, 1, function(x){
  
  # Running Cross Validations
  rf_model <- randomForest(type ~ ., data = set1_f,
                           maxnodes = x[1],
                           mtry = 4,
                           ntree=2500)
  
  # Getting predictions
  OOB_error = mean((predict(rf_model) == set1$type))
  
  # Test Error
  MSPE_error = mean((predict(rf_model, newdata = set2_f) == set2$type))
  
  # Putting together
  df_returned <- data.frame(Nodes = x[1], OOB_error = OOB_error, Test_Error = MSPE_error)
  
  # Returning
  return(df_returned)
  
})

# Putting together results
tree_size_results <- do.call(rbind, tree_size_rf)

# WRITE UP: For smaller number of trees, it seems that the OOB error seems to remain
# around the same average however, the test error is lower but quickly converges to the
# best it is (with mtry = 4) once we hit 20 nodes.
