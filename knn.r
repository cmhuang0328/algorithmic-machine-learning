# This file works on data available at a well-know repository of data
# hosted by the University of California-Irvine. The website for this
# dataset:
#  http://archive.ics.uci.edu/ml/datasets/Credit+Approval
# 
# The data contains information about individuals and whether they
# were approved  for credit. The problem is that the name of variables
# have been changed for privacy concerns. So we add  our own names.

# The data does not have any headers, so we add generic headers A1 to
# A16.

cat("This file compares many different classification techniques on a
credit approval data set from UCI web site.

We are downloading the data from a web site
Hit Enter to continue\n")
readline()

# This data has some parts missing (indicated by '?'). To tell the data
# frame that this we add na.string="?". Also note that the data is read
# off a web site directly, and not from a local file. Since there
# are no headers, we add headers A1 to A16 using sapply, and paste
# functions
credit <-
    read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data",   na.strings="?",
        col.names=sapply(1:16, function(x){return (paste("A",x,sep=""))}))
cat("\nnorw(credit) ", nrow(credit), "\n")
readline()

#Remove rows with <NA> in A2 column. 
credit <- credit[!is.na(credit$A2),]# & !is.na(credit$A2)&!is.na(credit$A16),]
#(You can add A3 and A16 as well)
#credit <- credit[!is.na(credit$A2) & !is.na(credit$A2)&!is.na(credit$A16),]
cat("Top rows of credit:\n")
print(head(credit))
readline()

cat("Bottom rows of credit:\n")
print(tail(credit))
readline()


# First, look at the connection between acceptance and A2:
#%cat("\n Hit Enter to see box plot A2 ~ acceptance:\n")
#%readline()
#dev.off()

#%boxplot(A2~A16, data=credit)
#%readline()

# Now, look at the connection between acceptance and A3:
#%cat("\n Hit Enter to see box plot A3 ~ A16:\n")
#%readline()
#dev.set(dev.next())
#%boxplot(A3~A16, data=credit)
#%readline()
#dev.off()

# We can now plot the combinations of A2 and A3 for acceptance (in
# blue) and no acceptance (in red)
#%cat("\n Hit Enter to see the scatterplot of A2 vs A3 for
#%acceptance cases:\n")
#%readline()
# Set up the graphics pane:

#%cat("\nSetting up the graphics pane:\n")
plot(credit[,"A2"], credit[,"A3"], col="white", 
    main="Scatter plot of credit, A2 and A3")
#readline()
points(credit[credit$A16=="+","A2"],
		credit[credit$A16=="+", "A3"], col="blue",pch=20)
#cat("\nHit Enter to add the non-acceptance points:\n")
#readline()
points(credit[credit$A16=="-","A2"],
		credit[credit$A16=="-", "A3"], col="red",pch=20)
#readline()

x<-sort(credit$A2)
last<-function(x){return(x[length(x)])} #An auxiliary function

####################Now build the knn model#############################

# There are several R packages that implement with the kNN model. Two common
# ones are the klaR library (with the sknn function) and the kknn library (with
# the kknn function). The following code uses the kknn package. To use
# klaR,ncoment lines containinf sknn, and comment out lines containing kknn.
# Also lad the klaR library.

#cat("\nNow Running the knn model, loading the klaR library\n")
cat("\nNow Running the knn model, loading the kknn library\n")
cat("\nHit Enter to start kNN:\n")
readline()
# Two of many libraries providing the kNN algorithm are the klaR and the kknn
# libraries

#library("klaR")
library("kknn")

# We now create a set of points to test the model These new set of points forma
# fine grid in the A2-A3 plane. We collect this data on our test data frame
# called newPts0.
# 
# We next let the user choose $k$ and run the kNN algorithm for that k
# on the credit data. We test the model on newPts0 data frame to see if the new
# points are classified as accepted for credit or rejected. Finally we plot the
# newPts0 data juxtaposed on the training data. Those predicted to be accepted
# are colored blue, and those predicted to be rejected are colored red. 


newA2<-seq(mA2<-floor(min(credit$A2)),MA2<-ceiling(max(credit$A2)),by=1)
newA3<-seq(mA3<-floor(min(credit$A3)),MA3<-ceiling(max(credit$A3)),by=0.5)

# Creating a grid of points to find their classification under kNN
newPts0<-data.frame(A2=c(), A3=c())
for (i in newA2){
   for (j in newA3){
      newPts0<-rbind(data.frame(A2=i, A3=j), newPts0)
   }
}
# Keep asking the user for k and build a corresponding kNN model:
repeat{
   cat("\nEnter the value k to be used for kNN (0 to exit):\n") 
   k<-as.numeric(readline())
   if(k<1) break;
   cat("\nRunning kNN for k=",k,"\n")
   cat("\nHit Enter to see the results:\n")
   #knnModel <- sknn(A16 ~ A3 + A2, k=k, data=credit)
   #knnPred<-predict(knnModel, newPts0, type="response")
   #newPts<-cbind(newPts0,data.frame(A16=knnPred$class))
   knnModel <- kknn(A16 ~ A3 + A2, k=k,train=credit,
                   test=newPts0,kernel="rectangular")
   knnPred<-predict(knnModel, data=newPts0)
   newPts<-cbind(newPts0,data.frame(A16=knnPred))

   # Create a new graphics panel
   dev.new()
   plot(credit[,"A2"], credit[,"A3"], col="white", main=paste(k,"NN"))
   points(credit[credit$A16=="+","A2"],
		credit[credit$A16=="+", "A3"], col="blue",pch=20)
   points(credit[credit$A16=="-","A2"],
		credit[credit$A16=="-", "A3"], col="red",pch=20)

   points(newPts[newPts$A16=='+',"A2"],newPts[newPts$A16=='+',"A3"],
					pch='.',cex=2,col="blue")
   points(newPts[newPts$A16=='-',"A2"],newPts[newPts$A16=='-',"A3"],
					pch='.',cex=2,col="red")
   # Now draw the boundary between blue and red points:
   knnBound<-data.frame(A2=c(), A3=c())
   for(i in newA2){
      for (j in newA3){
        if (i==last(newA2) | j==last(newA3)) break;
        if (newPts[newPts$A2==i & newPts$A3==j, "A16"] !=
           newPts[newPts$A2==i+1 & newPts$A3==j, "A16"])
           knnBound<-rbind(knnBound,data.frame(A2=i+0.5, A3=j))
        if (newPts[newPts$A2==i & newPts$A3==j, "A16"] !=
              newPts[newPts$A2==i & newPts$A3==j+0.5, "A16"])
           knnBound<-rbind(knnBound,data.frame(A2=i, A3=j+0.25))
        if (newPts[newPts$A2==i & newPts$A3==j, "A16"] !=
              newPts[newPts$A2==i+1 & newPts$A3==j+0.5, "A16"])
           knnBound<-rbind(knnBound,data.frame(A2=i+0.5, A3=j+0.25))
      }
   }
points(knnBound$A2,knnBound$A3,pch=20,cex=0.8, col="darkgreen")
}
