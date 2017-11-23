# Running the kNN on the UCI credit data by using validation technique to find
# the optimal k:

#
# This file works on data available at a well-know repository of data
# histed by University of California-Irvine. The website for this
# dataset:
#  http://archive.ics.uci.edu/ml/datasets/Credit+Approval
# 
# The data contains information about individuals and whether they
# were approved  for credit. The problem is that the name of variables
# have been changed for privacy concerns. So we add 

# The reading and setting up of the data is identical to the knn.r file Refer
# to that file for detailed information.

#cat("This file compares many different classification techniques on a
#credit approval data set from UCI web site.
#
#We are downloading the data from a web site
#Hit Enter to continue\n")
#readline()

# This data has some parts missing (indicated by '?'). To tell the data
# frame that this we add na.string="?". Also note that the data is read
# off a web site directly, and not from a local file. Since there
# are no headers, we add headers A1 to A16 using sapply, and paste
# functions
credit <-
    read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data",   na.strings="?",
        col.names=sapply(1:16, function(x){return (paste("A",x,sep=""))}))
cat("\nnorw(credit) ", nrow(credit), "\n")
#readline()

#Remove rows with <NA> in A2 column. 
credit <- credit[!is.na(credit$A2),]# & !is.na(credit$A2)&!is.na(credit$A16),]
#(You can add A3 and A16 as well)
#credit <- credit[!is.na(credit$A2) & !is.na(credit$A2)&!is.na(credit$A16),]

#########################################################################
# Running the validation technique to find the best k in kNN  method   #
#########################################################################

cat("\nRunning the validation test to determine the best k for the kNN
   method:\n")
n<-30 #Number of times a random sample is taken for each possible k 

# The following list will have the values of k tested in kList, along with the
# misclassification error. The minusList will contain percentage of those
# rejected that should not have been, plusList contains percentage of 
# those who were accepted and should not have been, and totalList
# contains the percentage of all misclassified items

# Initialize:
kList<-c()
minusList<-c()
plusList<-c()
totalList<-c()

   # Set the smallest and the largest  k to be testes, and step size between
   # them:
   lowKey=1; highKey=500; stepKey=10
   for (k in seq(lowKey,highKey,by=stepKey)){ # testing for each k
     kList<-c(kList,k) #attach k to the list
     #cat("\nRunning kNN for k=",k,"\n")
     #initialize error rates:
     knnMinusErrorRate<-0
     knnPlusErrorRate<-0
     knnErrorRate<-0
     for (i in 1:n){ # take n samples as training set
       samp<-sample(1:nrow(credit),floor(0.8*nrow(credit)))# 80% training 20% test
       creditTrain<-credit[samp,]
       creditTest<-credit[-samp,]
       #knnModel <- sknn(A16 ~ A3 + A2, k=k, data=creditTrain)
       #knnPred<-predict(knnModel, data=creditTest)
       #knnConfusion<-table(creditTest$A16, knnPred$class)
       knnModel <- kknn(A16 ~ A3 + A2, k=k,train=creditTrain,
                               test=creditTest,kernel="rectangular")
       # predict class of the test data
       knnPred<-predict(knnModel, data=creditTest)
       # using the table function (run ?table for documentation) count the
       # misclassified items in the test data
       knnConfusion<-table(creditTest$A16, knnPred)
       # update the error rates for this run 
       knnMinusErrorRate<-knnMinusErrorRate+
                knnConfusion[1,2]/sum(knnConfusion[1,])
       knnPlusErrorRate<-knnPlusErrorRate+
                knnConfusion[2,1]/sum(knnConfusion[2,])
       knnErrorRate<-knnErrorRate+
                (knnConfusion[1,2]+knnConfusion[2,1])/sum(knnConfusion)
       } #end inner for loop
     # compute the average of error rate over n test runs
     knnMinusErrorRate<-knnMinusErrorRate/n
     knnPlusErrorRate<-knnPlusErrorRate/n
     knnErrorRate<-knnErrorRate/n
     # add the error rates for current value of k to the lists:
     minusList<-c(minusList,knnMinusErrorRate)
     plusList<-c(plusList,knnPlusErrorRate)
     totalList<-c(totalList,knnErrorRate)
     #cat("\nError rate for '-':\n",knnMinusErrorRate,"\n")
     #cat("\nError rate for '+'\n",knnPlusErrorRate,"\n")
     #cat("\nError rate knn where k = ",k, ": ",knnErrorRate,"\n")
   } #end outer for loop


# Find the lowest total error rate and the k at which it is attained
bestError=min(totalList);
bestK=kList[which.min(totalList)]

# Plot k versus both positive and negative error rates and the total error rate
plot(kList,totalList, 'l',ylim=c(0,1),
         main=paste("Error rate as a function of k\noptimal k=",bestK))
                 lines(kList,minusList,col="red")
lines(kList,plusList,col="blue")
print(paste("Optimal k is:" , bestK," results in ",
     min(totalList), " error rate"))
