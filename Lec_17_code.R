################################################################################
#                                                                              #
#                                                                              #
#                Lecture 17 - Barinder Thind - Dylan Maciel                    #
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
library(nnet)
library(car)

##############
#            #
# Lecture 17 #
#            # 
##############

# Reading in data
wheat <- read.csv("wheat.csv", as.is = T, header = T)
wheat2 <- wheat %>% select(-c(id))

### (a) ###

# Setting up data
set.seed(67982193)
perm <- sample(x = nrow(wheat))

# Creating numerical variable
wheat$classnum <- as.numeric(as.factor(wheat$class))

# Splitting
set1 <- wheat[which(perm <= 200), -1]
set2 <- wheat[which(perm > 200), -1]

### (b) ###

# Creating scatter plot
ggpairs(wheat2[,-8]) + theme_bw()

# WRITE UP: Weight and size sem to be pretty highly correlated indicating that there is some
# potential for dimensionality reduction with (one of) these two variables. Also, the size and
# density box-plots, split over type, seem to show some separation between the types. This means
# that these variables could do a good job of predicting type. This is also true for weight.

### (c) ###

# Running pca
pc <- prcomp(x = set1[,-c(7, 1)], scale. = TRUE)
###### QUESTION: I removed the class variable, is that okay? ######

# Looking at summary
summary(pc)

# Looking at cumulation
evals <- pc$sdev^2
plot(y = evals, x = c(1:6), xlab = "PCs", ylab = "Variance")

# WRITE UP: Looking at the screeplot and at the summary of the data, we can see that 
# with 4 principal components, we explain over 92% of the variance in the data. Hence,
# we can conclude that 4 principal components would likely be enough. 

### (d) ###

# Scaling
set1s <- apply(set1[,-c(7, 1)], 2, scale)

# Running LDA
lda.fit <- lda(x = set1[,-c(7, 1)], grouping = set1$type)
lda.fit.s <- lda(x = set1s, grouping = set1$type)

# (i)

# (ii)

# Plot 1
plot(lda.fit, dimen=1, type="histogram", main="Values along first canonical variate")

# (iii)

# Training and test error
sMSE_lda <- mean((predict(lda.fit, newdata = set1[,-c(1, 7)])[[1]] == set1$type))
MSPE_lda <- mean((predict(lda.fit, newdata = set2[,-c(1, 7)])[[1]] == set2$type))

# (iv)

# Test set confusion matrix
table(set2$type, predict(lda.fit, newdata = set2[,-c(1, 7)])[[1]], dnn=c("Obs","Pred"))

# WRITE UP: It seems that sprout is being misclassified the most and, it seems that
# healthy is the prediction that it's being labelled as often although it is often
# labelled as scab as well (6). Also, healthy is misclassified as sprout 7 times in the
# test set. 

### (e) ###

# Fitting QDA
qda.fit <- qda(x = set1[ ,-c(7, 1)], grouping = set1$type)

# (i)

# Training and test error
sMSE_qda <- mean((predict(qda.fit, newdata = set1[,-c(1, 7)])[[1]] == set1$type))
MSPE_qda <- mean((predict(qda.fit, newdata = set2[,-c(1, 7)])[[1]] == set2$type))

# WRITE UP: The training set seems to perform comparably with an sMSE of 0.74 (it was 0.72
# for LDA) whereas the MSPE is off by about 4% when compared with the results from LDA. More
# splits would need to be run (or perhaps bootstrapping) to see whether this difference is
# actually because of differences in the model's predictive powers or because it just
# happened to be a split that was more favorable to LDA

# (ii)

# Test set confusion matrix
table(set2$type, predict(qda.fit, newdata = set2[,-c(1, 7)])[[1]], dnn=c("Obs","Pred"))

# WRITE UP: Again, we notice a similar pattern of misclassifications. It seems that there
# are some true values of healthy for which the explanatory variables overlap with that
# of sprout. And similarily, there seems to be some observations of sprout that are
# similar to healthy. This tells us that there is something about these two types that is
# likely inherent with respect to the explanatory variables and hence, it is difficult to
# separate them completely, using these methods anyway. Also, sprout was misclassified as
# scab a few times as well. In comparison with LDA, this method did a worse job of classifying
# healthy and got it confused with sprout more often than LDA did. The rest of the performance
# was very comparable.

### (f) ###

# Rescale function
rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}

# Rescaling
set1.rescale <- data.frame(cbind(rescale(set1[,-c(1, 7)], set1[,-c(1, 7)]), class=set1$type))
set2.rescale <- data.frame(cbind(rescale(set2[,-c(1, 7)], set1[,-c(1, 7)]), class=set2$type))

# Fitting multinom
mod.fit <- multinom(data = set1.rescale, formula = class ~ ., maxit = 1000, trace = TRUE)

# (i)

# looking at coefficients of multinom
summary(mod.fit)

# looking at coefficients of lda
lda.fit

# WRITE UP: The signs of the coefficients of both seem to be very similar. For example,
# we can see that the first LD and the coefficients for Scab in multinom are the exact
# same with respect to their signs. Similarily, for the second LD, we can see that the
# only difference is with weight (it is negative in multinom). This is because _______
# ____________________________________________________________________________________

# (ii)

# ANOVA
Anova(mod.fit)

# WRITE UP: Here, we notice that density and weight have very large values for the
# chi-square test value and thus have very small p-values indicating that they are
# significantly different between the different classes. Also, we can see that size
# is not significant and knowing that previously, it was highly correlated with weight,
# we can note that this variable doesn't neccesarily add much to the model that isn't
# already information we have in the weight variable. Hence, we could probably get a more
# sparse model by getting rid of this variable. Also, classnum seems to not add much at
# all to this model and thus, we could  probably be okay with getting rid of this variable.
# Moisture, while not being significant, seems to have a fairly low p-value and a further
# analysis would need to be carried to see whether or not it is worth keeping. We would
# repeat these tests with different splits of the data to see whether or not this pattern
# of significance persists. Also, to see how the more sparse models performs when compared
# with this full model.

# (iii)
class.col <- ifelse(set1$class=="hrw", y=50, n=100)
plot(x = set1.rescale[ ,1], y = set1.rescale[, 4], col=colors()[class.col],
     xlab = "Density", ylab = "Weight", main = "Separation by 2 Most Important Variables")

# WRITE UP: Here, we notice that it isn't a super clear 

# (iv)

# Making predictions
sMSE_multinom <- mean(predict(mod.fit, newdata = set1.rescale) == set1.rescale$class)
MSPE_multinom <- mean(predict(mod.fit, newdata = set2.rescale) == set2.rescale$class)

# WRITE UP: This model performs right between QDA and LDA. In fact, all three models perform
# very similarly. The MSPE of o.6 with this multinom model is worse than LDAs however, it is
# slightly better than how QDA was doing. The sMSE is very similar as well across all three 
# models. This makes sense with what we saw of the coefficient terms earlier. It's hard to
# decide which model is the best here. While, purely on the results, we could say LDA is the best
# it's difficult to make that claim with certainty as we have only done this analysis for 1
# split of the data

# (v)

# Test set confusion matrix
table(set2.rescale$class, predict(mod.fit, newdata = set2.rescale), dnn=c("Obs","Pred"))

# WRITE UP: Here, we can notice that same pattern as we saw with LDA and QDA where the 
# healthy class is being misclassified except this time, it's more spread out between the
# two other variables. Also, we see this continue with sprout as well which is misclassified
# even more often than healthy is. It seems that, from this small data set, multinom has
# a tougher time with sprout whereas the discriminant analysis methods have a tougher time
# with the healthy variable but both of them have issues with both. We also see that scab
# is predicted well which is inline with what we saw from QDA and LDA as well. 

