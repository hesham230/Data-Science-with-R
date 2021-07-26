#################################################################################
###       Estimating the Mean, Median and 10th Percentile of a Variable       ###
#################################################################################
library(MASS)
set.seed(1)
B = Boston
attach(B)

#1
medv.mean = mean(medv)
medv.mean

# 2
medv.err = sd(medv)/sqrt(length(medv))
medv.err


# 3
boot.fn = function(data, index) return(mean(data[index]))
library(boot)
bstrap = boot(medv, boot.fn, 1000)
bstrap

# 4
t.test(medv)
c(bstrap$t0 - 2 * 0.4119, bstrap$t0 + 2 * 0.4119)

# 5
medv.med = median(medv)
medv.med

boot.fn = function(data, index) return(median(data[index]))
boot(medv, boot.fn, 1000)


# 6
medv.tenth = quantile(medv, c(0.1))
medv.tenth

boot.fn = function(data, index) return(quantile(data[index], c(0.1)))
boot(medv, boot.fn, 1000)