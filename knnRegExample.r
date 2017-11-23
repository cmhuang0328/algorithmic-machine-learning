# This file simulates a data by creating a vector 't', and the vector
# 'y' by setting y=3+4*t + eps 
# where 'eps' is a random error following normal distribution with mean
# zero and standard deviation 3. We use this data to create a kNN
# regression model.
# 
# The kknn library must be installed. It allows both knn regression and
# classification
library(kknn)

tt=seq(-5,5,by=0.5)
yy=4*tt+3+rnorm(length(tt),0,sd=3.5)
trainDat=data.frame(t=tt,y=yy)
testDat=data.frame(t=seq(-5,5,by=0.01),y=3+4*seq(-5,5,by=0.01)+rnorm(length(seq(-5,5,by=0.01)),0,3))
# Experiment with different values of k and see the results.
k1=21

# Now build the knn regression model. you should set
# kernel="rectangular" otherwise, the kknn method will use a bit more
# sophistcated versions of kknn and not the one covered in class

knnRegModel<-kknn(y~t, train=trainDat, test=testDat,k=k1,kernel="rectangular")

# Create a data frame for predicted coordinates
datPred<-data.frame(t=seq(-5,5,by=0.01),y=predict(knnRegModel, data=testDat))

###############################################################
# To use the ordinary R graphics functions  un-comment the    #
# following lines, and comment out the lines below for the    #
# ggplot2 package                                             #
###############################################################

plot(tt,yy,col="blue", main=paste(k1,"-NN Regression"))
readline()
lines(testDat$t,predict(knnRegModel, data=testDat) ,col="red")


###############################################################
# To use ggplot2 library un-comment the following lines, and  #
# comment out the lines above for the regular R graphic       #
# functions.                                                  #
###############################################################

# The ggplot2 library is a nicer looking graphics package. Its product
# looks a bit more professional looking. Install and run this library
# Use the following for ggplot package plotting. 
#library(ggplot2)
#sp<-ggplot(trainDat,aes(x=t,y=y))
#sp<-sp+geom_point(color=I("blue"))+
#     geom_line(data=datPred,aes(x=t,y=y),color=I("red"))
#sp<-sp+ggtitle(paste(k1,"-NN"))
# To see the actual graph in ggplot you must use "print" otherwise no
# printing will take place:
#print(sp)

