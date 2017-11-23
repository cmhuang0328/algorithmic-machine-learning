# In this file we use the neural networks on the UCI credit data Make
# sure to install the "neuralnet" package in R.
credit <-
read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data",
   na.strings="?",
   col.names=sapply(1:16, function(x){return (paste("A",x,sep=""))}))
#cat("\nnorw(credit) ", nrow(credit), "\n")
#readline()

#Remove rows with <NA> in A2, A3, A8, A14 and A16 columns. 
credit <-
credit[!is.na(credit$A2)&!is.na(credit$A3)&!is.na(credit$A8)&
       !is.na(credit$A14)&!is.na(credit$A16),]
#(You can add A3 and A16 as well)
#credit <- credit[!is.na(credit$A2) & !is.na(credit$A2)&!is.na(credit$A16),]
#cat("Top rows of credit:\n")
#print(head(credit))
#readline()
# For whatever reason, the neuralnet package needs to have the response
# in the 0-1 form so we create a column A17, which is 1 if A16="+" and 0
# if A16="-":
credit=cbind(credit,A17=ifelse(credit$A16=="-",0,1))

# neural net does not like factor so we leave it.
#credit$A17=factor(credit$A17)

# It is important in neural nets to scale the data. Below we scale the
# A2 and A3 features by centering them around their mean and dividing
# them by their stdev. See the documentation for scale function.
credit$A2=scale(credit$A2, scale=T,center=T)
credit$A3=scale(credit$A3, scale=T,center=T)
credit$A8=scale(credit$A8, scale=T,center=T)
library(neuralnet)
samp=sample(1:nrow(credit), floor(0.9*nrow(credit)))
creditTrain<-credit[samp,]
creditTest<-credit[-samp,]
# Build the neural net model:
numRep=1 #number of repetitions of the neural net
neuralModel1<-neuralnet(A17 ~ A2+A3+A8,
#neuralModel1<-neuralnet(A17 ~ A2+A3,
           data=creditTrain,rep=numRep,
           hidden=c(3,2),linear.output=F,err.fct="ce",act.fct="logistic")
#print(summary(neuralModel1))
plot(neuralModel1)
# predict on the test set
readline()
#nnPred<-compute(neuralModel1, creditTest[,c("A2","A3")],rep=numRep)
nnPred<-compute(neuralModel1, creditTest[,c("A2","A3","A8")],rep=numRep)
#nnPred<-compute(neuralModel1, creditTest[,c("A2","A3")],rep=numRep)
print(tbl<-table(ifelse(nnPred$net.result>1/2,1,0), creditTest$A17))
print(paste("Neural Net Error Rate: ",(tbl[1,2]+tbl[2,1])/sum(tbl)))
