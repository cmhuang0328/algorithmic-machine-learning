# In this project we compare several variation of the naive Bayes method and
# the tree partitioning problem on detecting psam e-mails. 
# 
# We use the spam data in the UC Irvine archive. Go to the site:
# http://archive.ics.uci.edu/ml/machine-learning-databases/spambase/
# to see documentation about this data set. 
#
# First read the data, it is separated by commas and is in csv format, 
# The data contains no headers and read.csv by default will not add any
# headers
website1<-"http://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data"
spamData<-read.csv(website1)

# The actual headers are in another site in the UCI archive. It
# contains the title of each column, along with some comments. These
# are lines that start with a "|". Please go to the web site and examine its
# content. THat way it would be easier to understand the next few lines. 
# 
# Read data from  the website2 and treat lines starting with a '|' as
# comment (check the origin of the file)
# Make sure not to include any line breaks when you cut and paste:
website2<-"http://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.names"
spamHeaders<-read.table(website2, comment.char="|")

# Turn each item in the first column of spamHeaders to a string
# (otherwise they are treated as levels of some factor by R)  Also start from
# row 2, since row 1 does not have data (check the origin of the file)
# paste will create a vector where every item is turned a string (look at
# the documentation of paste, and also paste0)
tmpNames<-paste(spamHeaders[2:nrow(spamHeaders),1])

# Remove the ":" character from the column headers since the 'tree'
# function does not like them (read documentation by typing ?strsplit)
tmpNames<-strsplit(tmpNames,":")
# Assign the strings in the array tmpNames to the headers of the spamData.
# Note that we can simply assign it to the result of colnames function
colnames(spamData)[1:(ncol(spamData))]<- c(paste(tmpNames),"spam")

# Columns 49 through 54 indicate whether characters ';', '(', '[',
# '!', '$' and '#' occur in the e-mail. Since these types of characters
# cannot appear in a tree formula we replace them with words. Check the
# documentation for R's "sub" function. Also, \\; is used to interpret
# ';' as a semicolon and not the end of an R statement. Ditto for other
# characters:
colnames(spamData)[49]<-sub("\\;","semiColon",colnames(spamData)[49])
colnames(spamData)[50]<-sub("\\(","lparen",colnames(spamData)[50])
colnames(spamData)[51]<-sub("\\[","lsqBrac",colnames(spamData)[51])
colnames(spamData)[52]<-sub("\\!","exclam",colnames(spamData)[52])
colnames(spamData)[53]<-sub("\\$","dollar",colnames(spamData)[53])
colnames(spamData)[54]<-sub("\\#","hashtag",colnames(spamData)[54])
# The data frame is now ready for naive Bayes and tree models

# The following line is optional. You could also turn the spam column to
# factor with levels 0 and 1
spamData$spam<-ifelse(spamData$spam==1,"Spam","NoSpam")
spamData$spam<-as.factor(spamData$spam)

# save two copies one where each word is 0-1 variable (spamData) and the other
# where for word frequencies are considered as numerical variables (spamData1)

spamData1<-spamData

# Transform all variables except the last four into 0-1 factors (the
# table contains *proportion* of words not just whether they occur or not)
for(i in 1:(ncol(spamData)-4)){
    spamData[,i]<-ifelse(spamData[,i]>0,1,0)
    spamData[,i]<-factor(spamData[,i],levels=c(0,1))
}

print(" Here is the spamData data frame after turning word variables
into binary form:\n")
print(head(spamData))
print(tail(spamData))

# load klaR library for NaiveBayes function
library("klaR")

# To see how our algorithms work we set aside 20% of the data as the
# test set and then see how different models perform
spamTrain<-spamData[samp<-sample(1:nrow(spamData),floor(0.8*nrow(spamData))),]
spamTest<-spamData[-samp,]

#####################################################################
# Model 1: Only keywords, and only if they occur in the e-mail.
#####################################################################
# Include every variable except the last four (which are quantitative)
# 
readline("Running Naive Bayes without smoothing and with 0-1 variables: Hit
Enter to continue\n")
# build a formula from a string (useful for long formulas):
form <- paste0("spam ~ ", colnames(spamData)[1])
for (i in 2:54)
  form<-paste0(form," + ", colnames(spamData)[i]) 

# Turn the string into a formula
BayesModel1 <- NaiveBayes(formula(form), data=spamTrain)

print(summary(BayesModel1))

print("\n Building a predictor, print the confusion matrix and 
calculate the error rate. Hit Enter:\n")
spamPred1<-predict(BayesModel1, spamTest)
print("Confusion table for BayesModel1 (0-1 word variables) without Laplace
smoothing:")
print(tbl<-table(spamPred1$class, spamTest$spam))
print(paste("Naive Bayes Error Rate on spam Data BayesModel1 without
Laplace smoothing: ", (tbl[1,2]+tbl[2,1])/sum(tbl)))

# Try Laplace smoothing alpha=1, beta=10 for all 0-1 variables:

readline("Running Naive Bayes with smoothing (alpha=1, beta=10) and with 
0-1 variables: Hit Enter to continue\n")

BayesModel1L <- NaiveBayes(formula(form), fL=c(1,10), data=spamTrain)

print(summary(BayesModel1L))

print("\n Building a predictor, print the confusion matrix and 
calculate the error rate. Hit Enter:\n")
spamPred1L<-predict(BayesModel1L, spamTest)
print("Confusion table for BayesModel1L (0-1 word variables) with Laplace
smoothing, alpha=1, beta=10:")
print(tbl<-table(spamPred1L$class, spamTest$spam))
print(paste("Naive Bayes Error Rate on spam Data BayesModel1L with
Laplace smoothing: ", (tbl[1,2]+tbl[2,1])/sum(tbl)))

#####################################################################
# Model 2N: keywords and symbols, and proportion of words treated as normal
#####################################################################
# Now we run a second naive Bayes model where the proportions are used not
# just the binary version indicating whether the word occurs or not
print(head(spamData1))
print(head(spamData1))
readline("Head and tail of spamData11 with proportions")
readline("\n Hit Enter to see another Bayes model where proportions are used
and treated as normal variables\n")
spamTrain<-spamData1[samp,]# Use the same  training and test data but with
spamTest<-spamData1[-samp,]# actual proportions rather than simply a 0-1 variable
# NaiveBayes by default treats numerical variables as normal RV. Note that
# we also use the run variables at the end. So we use *all* variables. Also
# usekernel=FALSE is default and is not necessary. It is added for emphasis.
BayesModel2N <- NaiveBayes(spam~., usekernel=FALSE, data=spamTrain) 
print(summary(BayesModel2N))
spamPred2N<-predict(BayesModel2N, spamTest)
print(tbl2N<-table(spamPred2N$class, spamTest$spam))
print(paste("Naive Bayes Error Rate on spam Data with normal variables: ", 
                (tbl2N[1,2]+tbl2N[2,1])/sum(tbl2N)))


#####################################################################
# Model 2K: keywords and symbols, and density of proportion of words by
# kernel
#####################################################################
# Now we run a second naive Bayes model where the proportions are used 
# and their distribution is estimated non-parametrically using "kernels"
readline("\n Hit Enter to see another Bayes model where proportions are used
but their distributions are estimated non-paramterically\n")
# NaiveBayes by default treats numerical variables as normal RV
BayesModel2K <- NaiveBayes(spam~., usekernel=TRUE, data=spamTrain) 
print(summary(BayesModel2K))
spamPred2K<-predict(BayesModel2K, spamTest)
print(tbl2K<-table(spamPred2K$class, spamTest$spam))
print(paste("Naive Bayes Error Rate on spam Data with kernel estimated
density for  variables: ", 
                (tbl2K[1,2]+tbl2K[2,1])/sum(tbl2K)))

#####################################################################
# Model 3: Use CART on the binary keywords
#####################################################################

readline("\nRunning the tree model on spam data. Hit Enter to continue:\n")
library(tree)

treeSpam<- tree(spam ~ ., data=spamData)
print(summary(treeSpam))
dev.new()
plot(treeSpam);text(treeSpam, pretty=0)
print(treeSpam)
print(summary(treeSpam))
