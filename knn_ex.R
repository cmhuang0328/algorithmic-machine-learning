# This file simulates a data by creating a vector 'tt' and the vector 'yy' by setting yy= 3+4*t +eps
# Where 'eps' is a random error following normal distribution with mean zero and standard devidation 3. We use this data to create a kNN regression model.

# The kknn library must be installed. It allows both knn regression and classfication
library(kknn)

#The
library(ggplot2)
tt=seq(-5, 5, by=0.5)
yy=4*tt +3+ rnorm(length(tt),0, sd=3.5)
trainDat= data.frame(t=tt,y=yy)
testDat= data.frame(t=seq(-5, 5, by=0.01), y=3+4* seq(-5, 5, by=0.01)+rnorm(length(seq(-5, 5, by=0.01)),0,3))
k1=200
knnRegModel<- kknn(y~t, train = trainDat, test= testDat, k=k1)

#create a data frame for predcicted coordinates
dataPred<- data.frame(t=seq(-5, 5, by=0.01), y=predict(knnRegModel, data=testDat))

#You may use the ordinary plotting facilities in R. Use the following
sp <- ggplot(trainDat, aes(x=tt, y=yy))
sp <- sp+ geom_point(color=I("Blue"))+ geom_line(data=dataPred, aes(x=t, y=y, color=I("red")))

print(sp)