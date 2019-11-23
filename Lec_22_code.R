################################################################################
#                                                                              #
#                                                                              #
#                Lecture 22 - Barinder Thind - Matt Berkowitz                  #
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
# Lecture 22 #
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