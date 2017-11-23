# In this file we use the German credit data and use it to classify with
# trees. The data are the same as the knn.r file we studied
# earlier

############################################################################
# Reading in the data:
############################################################################

# This data has some parts missing (indicated by '?'). To tell the data
# frame that this we add na.string="?". Also note that the data is read
# off a web site directly, and not from a local file. Also, since there
# are no headers, we add headers A1 to A16 using sapply, and pase
# functions
dataWebSite<-"http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data"
credit <-  # Note minor simplification compared to before and use of paste0
   read.csv(dataWebSite,na.strings="?",col.names=paste0('A',1:16)) 

#Remove rows with <NA> in A2 A3 and A16 columns. 
credit <- credit[!is.na(credit$A2) & !is.na(credit$A2)&!is.na(credit$A16),]
cat("Top rows of credit:\n")
print(head(credit))
#
cat("Bottom rows of credit:\n")
print(tail(credit))


############################################################################
# Drawing graphs of error rate vs the Gini index and cross entropy
############################################################################

# Before proceeding, let us plot the functions represeting, error rate,
# Gini Index, and entropy for two class case. As you can see all three
# functions are smallest (that is zero) at 0 and 1, and largest at 0.5:

readline("\nPlotting the entropy (green), GINI (blue), and error rate
(red) for two class case. Hit Enter to see\n")

x<-seq(0,1,by=0.01)
plot(x,
    sapply(x,function(y){ifelse((y==0)||(y==1),0,(-y*log(y)-(1-y)*log(1-y))/log(2))}),
    'l',xlab="p", ylab="impurity",col="green")

lines(x,2*x*(1-x), col="blue")
lines(x,sapply(x,function(z){1-max(z,1-z)}),col="red")

############################################################################
# Building the tree model with default parameters
############################################################################

# Load the tree library
library(tree)

# Unlike previous example for kNN where we only used A2 and A3, here we are
# using *all* variables (A1-A15) to classify A16 (approved for credit
# card is+, not approved is -)
treeModel<-tree(A16~., data=credit) #A16~. formula means use all variables (A1-A15)

dev.new() #open a new graphics panel
plot(treeModel, main="default fit") # This just draws the tree with no labeling
readline("This is the skeleton of the tree without labels. Hit Enter to see the
labels\n")

# Plot only draws the tree with no labels. The text function in  R ordinarily
# takes a vector of texts along with two vectors of x and y coordinates, and
# draws the texts on those coordinates. When a tree model is given to the text
# function it will add correct labels to the tree (which must have been plotted
# already).
text(treeModel,pretty=0)
readline("\nHit Enter to see the summary of the default tree model\n")
# The summary function gives basic information about the tree
print(summary(treeModel))
readline("\n Hit Enter to see the complete information about the default tree\n")
# When printing a tree model the software prints the information about each node
# by indenting it according to level of that node in the tree.
print(treeModel)
readline("Hit Enter to continue\n")

# We now experiment with different values of mindev and minsize (as in
# regression case). Feel free to experiment with different values and
# see how the tree grows or shrinks:
d1<-0
s1<-2
print(paste("Minsize set to ",d1, " and mindev set to ",s1,"\n"))
treeModel1<-tree(A16~., mindev=d1, minsize=s1, data=credit)
dev.new()
readline("Hit Enter to see the complete tree without labels\n")
plot(treeModel1, main=paste0("perfect fit: mindev=", d1, ", minsize=",s1))
readline("Hit Enter to see the complete tree with labels\n")
text(treeModel,pretty=0)
readline("\nHit Enter to see the summary of complete (overfitted) Tree Model\n")
print(summary(treeModel1))
readline("Hit Enter to see the complete information about the tree nodes\n")
print(treeModel1)
 
