################################################################################
####          Estimating the Accuracy of a Linear Regression Model          ####
################################################################################
library(ISLR)
A = Auto

# Create a function that gets the auto data and indices and returns the coefficients of the regression
boot.fn = function(data,index)
{
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
}

# Test the function
boot.fn(A, 1:392)  # running the function on the original data
set.seed(1) 
boot.fn(A,sample(392,392, replace = T)) # running the function on a sample

# Bootstrap
library(boot)
boot(A, boot.fn, 1000)

# Regression using lm
summary(lm(mpg ~ horsepower, data = A))$coef

# if the assumptions of regression don't hold, than bootstrapping is more
# reliable than the lm results