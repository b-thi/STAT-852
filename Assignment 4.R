################################################################################
#                                                                              #
#                                                                              #
#                Assignment 4 - Barinder Thind - STAT 852                      #
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
library(caret)
library(leaps)
library(MASS)
library(BMA)
library(datasets)
library(splines)

##############
#            #
# Lecture 5  #
#            # 
##############

# Q1

# Loading data
data("UKDriverDeaths")

# Plotting
plot(UKDriverDeaths)

# Q2

# I will just write a spline function
spline_function <- function(type, df_consider = 1:10){
  
  if (type == "natural"){
    par(mfrow = c(5, 5))
    for (i in 1:length(df_consider)) {
      
      nat.spl <- lm(data = UKDriverDeaths, UKDriverDeaths ~ ns(time(UKDriverDeaths), df = df_consider[i]))
      
      plot(x = time(UKDriverDeaths), y = predict(nat.spl, newdata=time(UKDriverDeaths)), 
                 col="red", type = "l", lwd = 2,
           xlab = paste0("Time\n[Natural] df = ", df_consider[i]), 
           ylab = "Driver Deaths",
           ylim = c(1000, 2700))
      
      lines(x = time(UKDriverDeaths), y = UKDriverDeaths, type = "l",
            xlab = paste0("Time\n[Natural] df = ", df_consider[i]), 
            ylab = "Driver Deaths",
            ylim = c(1000, 2700))
      }
  }
  
  if (type == "basis"){
    par(mfrow = c(5, 5))
    for (i in 1:length(df_consider)) {
      
      bs.spl <- lm(data = UKDriverDeaths, UKDriverDeaths ~ bs(time(UKDriverDeaths), df = df_consider[i]))
      
      plot(x = time(UKDriverDeaths), y = predict(bs.spl, newdata=time(UKDriverDeaths)), 
           col="green", type = "l", lwd = 2,
           xlab = paste0("Time\n[Basis] df = ", df_consider[i]), 
           ylab = "Driver Deaths",
           ylim = c(1000, 2700))
      
      lines(x = time(UKDriverDeaths), y = UKDriverDeaths, type = "l",
            xlab = paste0("Time\n[Basis] df = ", df_consider[i]), 
            ylab = "Driver Deaths",
            ylim = c(1000, 2700))
      }
  }
  
  if (type == "smoothing.spline"){
    par(mfrow = c(5, 5))
    for (i in 1:length(df_consider)) {
      
      s_spline <- smooth.spline(x = time(UKDriverDeaths), y = UKDriverDeaths, 
                    df = df_consider[i])
      
      plot(s_spline, 
           col="blue", type = "l", lwd = 2,
           xlab = paste0("Time\n[Smoothing] df = ", df_consider[i]), 
           ylab = "Driver Deaths",
           ylim = c(1000, 2700))
      
      lines(x = time(UKDriverDeaths), y = UKDriverDeaths, type = "l",
            xlab = paste0("Time\n[Smoothing] df = ", df_consider[i]), 
            ylab = "Driver Deaths",
            ylim = c(1000, 2700))
      }
  }
  
  if (type == "loess") {
    df_consider = c(0, 1, 2)
    span_considered = seq(0.1, 1, 0.2)
    par(mfrow = c(3, 5))
    for (i in 1:length(df_consider)) {
      for (j in 1:length(span_considered)) {
        
        loess_model <- loess(data = UKDriverDeaths, 
                       as.numeric(UKDriverDeaths) ~ as.numeric(time(UKDriverDeaths)), 
                       degree = df_consider[i],
                       span = span_considered[j])
        
        plot(x = time(UKDriverDeaths), y = loess_model$fitted, 
             col = "orange", lwd=2, type = "l",
             xlab = paste0("Time\n[Loess] degree = ", df_consider[i],
                           "; span = ", span_considered[j]), 
             ylab = "Driver Deaths",
             ylim = c(1000, 2700))
        
        lines(x = time(UKDriverDeaths), y = UKDriverDeaths, type = "l",
              xlab = paste0("Time\n[Loess] degree = ", df_consider[i],
                            "; span = ", span_considered[j]), 
              ylab = "Driver Deaths",
              ylim = c(1000, 2700))
        }
      }

      }
}
  

# Answer now for this question
spline_function("natural", seq(1, 50, 2))

# Q3
spline_function("smoothing.spline", seq(1, 50, 2))

# Q4
spline_function("loess")


loess_model <- loess(data = UKDriverDeaths, 
                     as.numeric(UKDriverDeaths) ~ as.numeric(time(UKDriverDeaths)), 
                     degree = 0,
                     span = 0.1)
par(mfrow = c(1, 1))
plot(x = time(UKDriverDeaths), y = loess_model$fitted, 
     col = "orange", lwd=2, type = "l",
     xlab = paste0("Time\n[Loess] df = ", 0,
                   "; span = ", 0.1), 
     ylab = "Driver Deaths",
     ylim = c(1000, 2700))
