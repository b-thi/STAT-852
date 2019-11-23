################################################################################
#                                                                              #
#                                                                              #
#                Lecture 21 - Barinder Thind - Matt Berkowitz                  #
#                                                                              #
#                                                                              #
################################################################################

#############
#           #
# Libraries #
#           # 
#############

library(tidyverse)
library(e1071)

##############
#            #
# Lecture 21 #
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

# Fitting model
svm_fit <- svm(data = set1, type ~ ., kernel= "radial", cross=10)
summary(svm_fit)
svm_fit$tot.accuracy
svm_fit$accuracies

# WRITE UP: Here, we fit the model using a radial basis with the default parameters
# and saw that we had a cross-validating accuracy of 70.5%. A 10-fold cross-validation was
# used. Next, we tune the model to find the optimal parameters for this particular data set.

# Splitting train into another 2 sets
train_1 <- set1[sample(1:nrow(set1), ceiling(0.7*nrow(set1))),]
train_2 <- setdiff(set1, train_1)

# Tuning
wheat.tune <-  tune.svm(data = train_1, 
                        type ~ ., 
                        kernel="radial", 
                        tunecontrol = tune.control(sampling="fix"), 
                        validation.x = train_2[,1:6], 
                        validation.y = train_2[,7],
                        gamma = 10^(-5:0), 
                        cost = 10^(0:5),
                        cross = 10) 

# Summary of results
summary(wheat.tune) 

# Looking at best combination
tuning_results <- summary(wheat.tune)$performances 
tuning_results[order(tuning_results[,3]),] 

# Contour plot
plot(wheat.tune, type="contour", transform.x=log10, transform.y=log10)

# WRITE UP: Here, we attempted to tune the model using the grid supplied by Tom. We split
# the current training set into another test and training set so that we had a validation
# set for the training. After running using the grid, we found that our minimum was not on
# a boundary (however, on repeat runs, there did seem to be variation with the parameter
# values selected but the error rate minimum seemed fairly consistent). We found that the
# minimum was achieved when gamma was 10^-1 and the cost was 10^2

# Building optimal model
svm_tuned <- svm(data = set1, 
                 type ~ .,
                 cost = 10^2,
                 gamma = 10^-2, 
                 kernel= "radial",
                 cross = 10)

# Getting test error
1 - mean(predict(svm_tuned, set2) == set2$type)

# The error rate is 34.7% using the radial basis with tuned parameters

### (b) ###

# Fitting model
svm_fit_poly <- svm(data = set1, type ~ ., kernel= "polynomial", cross=10)
summary(svm_fit_poly)
svm_fit_poly$tot.accuracy
svm_fit_poly$accuracies

# WRITE UP: Here, we fit the model using a polynomial basis with the default parameters
# and saw that we had a cross-validating accuracy of 67%. A 10-fold cross-validation was
# used. Next, we tune the model to find the optimal parameters for this particular data set.

# Tuning
wheat.tune_poly <-  tune.svm(data = train_1, 
                        type ~ ., 
                        kernel="polynomial", 
                        tunecontrol = tune.control(sampling="fix"), 
                        validation.x = train_2[,1:6], 
                        validation.y = train_2[,7],
                        gamma = 10^(-5:0), 
                        cost = 10^(0:5),
                        cross = 10) 

# Summary of results
summary(wheat.tune_poly) 

# Looking at best combination
tuning_results2 <- summary(wheat.tune_poly)$performances 
tuning_results2[order(tuning_results2[,3]),] 

# Contour plot
plot(wheat.tune_poly, type="contour", transform.x=log10, transform.y=log10)

# WRITE UP: Here, we attempted to tune the model using the grid supplied by Tom. We split
# the current training set into another test and training set so that we had a validation
# set for the tuning process. After running using the grid, we found that our minimum was not on
# a boundary (however, on repeat runs, there did seem to be variation with the parameter
# values selected but the error rate minimum seemed fairly consistent). We found that the
# minimum was achieved when gamma was 10^-1 and the cost was 10^1

# Building optimal model
svm_tuned_poly <- svm(data = set1, 
                 type ~ .,
                 cost = 10^2,
                 gamma = 10^-1, 
                 kernel= "polynomial",
                 cross = 10)

# Getting test error
1 - mean(predict(svm_tuned_poly, set2) == set2$type)

# The error rate is 38.7% using the radial basis with tuned parameters

### (c) ###

# Confusion matrix - radial
table(set2$type, predict(svm_tuned, set2), dnn=c("Obs","Pred"))

# Confusion matrix - polynomial
table(set2$type, predict(svm_tuned_poly, set2), dnn=c("Obs","Pred"))

# WRITE UP: The error rate in general seemed to be worse for the svm (although over
# repeat runs, it may be that this isn't really the case due to the high variability
# we saw during the tuning process). The misclassification however, seems to be happening
# again with the classes of healthy and sprout. It seems that all the methods we have
# used thus far seem to have trouble distinguishing between these two. The polynomial basis
# for svm also had a tougher time with the scab class, misclassifying it as sprout 7 times
# which is worse than the previous results using trees, lda, and multinom. It seems that
# the radial basis performs better for this problem than the polynomial basis does. 