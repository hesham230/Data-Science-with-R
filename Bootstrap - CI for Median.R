############################################################################
##      Calculation of CI for Median of Balance using Bootstrap           ##
############################################################################

# Import Data #
library(ISLR)
D = Default



################################################################################
###                          Bootstrap using boot                            ###
# The boot function creates R sample. Each one will contain a different set    #
# of rows. When calling a function, such as median, R needs to somehow pass    #
# on the indices of the rows to be used. That's why we cannot use built in     #
# functions, we must create our own. The function to be called by boot         #
# must have two arguments: data to be used and the indices. Boot will call this#
# function and tell it what the indices are, we do not have to do it ourselves.#
################################################################################
library(boot)
set.seed(1)
boot(data = D$balance,statistic = function(x,i) median(x[i]),R = 1000)
b = boot(data = D$balance,statistic = function(x,i) median(x[i]),R = 1000)

# Let's do it manually !!

n = length(D$balance)
R = 1000
results = rep(NA,R)
for (i in 1:R)
{
  boot.sample = sample(n,replace = T)
  results[i] = median(D$balance[boot.sample])
}
sd(results)