# Percentage returns for the S&P 500 stock index over 1250 days. For each date we have records of
# percetage returns of the previous 5 days, and the number of shares traded on the previous day (volume), and
# the percentage of returns today (Today). The dependent variable is Direction - whether the market was
# up or down on the date.
# High percentage return - good thing

library(ISLR)
library(ggplot2)
library(cowplot)
library(reshape2)
library(lmtest)
library(pscl)
library(pROC)

# Importing dataset from package
S = Smarket
S = S[,-8]
S$Direction = factor(S$Direction)
summary(S)

cor(S[,-8]) # low correlation between today's returns and previous returns

# Box Plot with means
# scale_fill_brewer(palette="Set3") - setting the set of colours to be used for direction
P1 = ggplot(S, aes(x=Direction, y=Today, fill=Direction)) +
  geom_boxplot(alpha=0.4) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")

P2 = ggplot(S, aes(x=Direction, y=Volume, fill=Direction)) +
  geom_boxplot(alpha=0.4) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")

P3 = ggplot(S, aes(x=Direction, y=Lag1, fill=Direction)) +
  geom_boxplot(alpha=0.4) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")

P4 = ggplot(S, aes(x=Direction, y=Lag2, fill=Direction)) +
  geom_boxplot(alpha=0.4) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")

P5 = ggplot(S, aes(x=Direction, y=Lag3, fill=Direction)) +
  geom_boxplot(alpha=0.4) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")

P6 = ggplot(S, aes(x=Direction, y=Lag3, fill=Direction)) +
  geom_boxplot(alpha=0.4) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")

P7 = ggplot(S, aes(x=Direction, y=Lag4, fill=Direction)) +
  geom_boxplot(alpha=0.4) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")

P8 = ggplot(S, aes(x=Direction, y=Lag5, fill=Direction)) +
  geom_boxplot(alpha=0.4) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")

plot_grid(P1,P2,P3,P4,P5,P6,P7,P8)

# Wide to Long for Plotting in one graph
LongS= melt(S, id.vars=c("Year", "Direction"))
# LongS[order(LongS$ID,LongS$variable),]
ggplot(LongS, aes(x=Direction, y=value, fill=Direction)) +
  geom_boxplot(alpha=0.4) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3")+
  facet_wrap(~ variable)

# Density Plots
ggplot(LongS, aes(x=value, fill=Direction)) + geom_density(alpha=.3) +
  facet_wrap(~ variable)

# Training and Testing
train.index = S$Year < 2005
train = S[train.index,]
test = S[!train.index,]

# Logistic Regression on the Training Set
logistic1 = glm(data = train, Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial)
summary(logistic1)


# Likelihood Ratio Test
# log likelihood model = -690.55
# Residual deviance = -2log Likelihood =  1381.1 
# log likelihood empty model = -691.63
# Residual deviance = -2log Likelihood =  1383.3
# Likelihood Ratio Test = Deviance reduced - Deviance full or -2*((LL Reduced) - (LL Full))
# Distribution is chi square with p-r degrees of freedom (number of vars added to the model)
emptymodel = glm(data = train, Direction ~ 1, family = binomial)
lrtest(emptymodel, logistic1)
LR.Statistic = -2*((-691.63) - (-690.55))
LR.Statistic = logistic1$null.deviance - logistic1$deviance
LR.PValue = 1-pchisq(2.1601,6)

pR2(logistic1) # McFadden R^2

# Logistic Regression Prediction - Test Set
test.predictions = predict(logistic1, test , type = "response")
predictions = rep("Down",length(test$Direction))
predictions[test.predictions>0.5] = "Up"
confusion1 = table(predictions,test$Direction)  # Testing confusion matrix

TP = confusion1[4]
TN = confusion1[1]
FP = confusion1[2]
FN = confusion1[3]

accuracy = (TP+TN)/(TP+TN+FP+FN)
sensitivity = TP/(TP+FN)
specificity = TN/(TN+FP)
F1Score = (2*TP)/(2*TP+FP+FN)
PPV = TP/(TP+FP)
NPV = TN/(TN+FN)


roc(response = test$Direction, predictor = test.predictions, auc = T, plot = T)
