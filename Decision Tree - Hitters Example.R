library(ISLR)
library(ggplot2)
library(tree)
H = Hitters

ggplot(data = H) + geom_point(mapping = aes(x = Years, y = Hits, col = Salary)) + scale_color_gradient(low="blue", high="red")

ggplot(data = H) + geom_point(mapping = aes(x = Years, y = Hits, col = Salary)) + scale_color_gradientn(colours = rainbow(5))

# Omitting missing values
H = na.omit(H)
H$Salary = na.omit(H$Salary)

treefit = tree(log(Salary) ~ Years + Hits, data=H)

summary(treefit)
# There are 8 terminal nodes or leaves of the tree.
# deviance is just mean squared error; this gives us an RMS error of 0.27
plot(treefit)
text(treefit,cex=0.7)
# leaves are labeled in log(salary). We need to take 1000*e^num to get the salary

prune1 = prune.tree(treefit,best=5) # Returns best pruned tree
plot(prune1)
text(prune1,cex=0.7)

# similarly: prune.tree(my.tree,best=5,newdata=test.set)

tree.seq = prune.tree(treefit)
plot(tree.seq) # Plots size vs. error
tree.seq$dev # Vector of error
opt.trees = which(tree.seq$dev == min(tree.seq$dev)) # rates for prunings, in order
min(tree.seq$size[opt.trees])

# yval is the mean value of log salary within that node

######################## Hitters data with Train/Test and CV #####################
fold = floor(runif(nrow(H),1,11))
H$fold = fold
test.set = H[H$fold == 1,] # test set is first fold
train.set = H[H$fold != 1,] # exclude test from train

my.tree = tree(log(Salary) ~ Years+ Hits, data=train.set, mindev=0.001)
# The within-node deviance must be at least this times that of the root node for the node to be split.
train.tree = prune.tree(my.tree, best=5) # Return best pruned tree with 5 leaves, evaluating error on training data
plot(train.tree)
text(train.tree)

test.tree = prune.tree(my.tree,best=5,newdata=test.set)
plot(test.tree)
text(test.tree)

# Prunning on Train data
my.tree.seq =prune.tree(my.tree)
plot(my.tree.seq)# error versus plot size
my.tree.seq$dev
opt.trees =which(my.tree.seq$dev ==min(my.tree.seq$dev))
(best.leaves =min(my.tree.seq$size[opt.trees]))

my.tree.pruned =prune.tree(my.tree,best=best.leaves)
plot(my.tree.pruned)
text(my.tree.pruned,cex=0.3,digits=3)

plot(H$Years,H$Hits,pch=20, xlab="Years",ylab="Hits", col="grey")
partition.tree(my.tree.pruned,ordvars=c("Years","Hits"), add=T)


# Prunning on test data
my.tree.cv =cv.tree(my.tree)
cv.tree(my.tree,best=5)
cv.tree(my.tree)
plot(cv.tree(my.tree))
