# This file works on data available at a well-know repository of data
# histed by University of California-Irvine. The website for this
# dataset:
#  http://archive.ics.uci.edu/ml/datasets/Credit+Approval
# 
# The data contains information about individuals and whether they
# were approved  for credit. The problem is that the name of variables
# have been changed for privacy concerns. So we add 

# The data does not have any headers, so we add generic headers A1 to
# A16.

cat("In this file we compare various non-parametric different 
classification  techniques on a credit approval data set from UCI web site.

We are downloading the data from a web site
Hit Enter to continue\n")

# This data has some parts missing (indicated by '?'). To tell the data
# frame that this we add na.string="?". Also note that the data is read
# off a web site directly, and not from a local file. Also, since there
# are no headers, we add headers A1 to A16 using sapply, and pase
# functions
credit <-
read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data",na.strings="?",
   col.names=paste0('A',1:16))

#Remove rows with <NA> in A2 column. 
#credit <- credit[!is.na(credit$A2),]# & !is.na(credit$A2)&!is.na(credit$A16),]
#(You can add A3 and A16 as well)
credit <- credit[!is.na(credit$A2) & !is.na(credit$A2)&!is.na(credit$A16),]
#cat("Top rows of credit:\n")
print(head(credit))
#
#cat("Bottom rows of credit:\n")
#print(tail(credit))

# We plot the combinations of A2 and A3 for acceptance (in
# blue) and no acceptance (in red)
readline("\n Hit Enter to see the scatterplot of A2 vs A3 for
acceptance cases:\n")
# Set up the graphics pane:

cat("\nSetting up the graphics pane:\n")
plot(credit[,"A2"], credit[,"A3"], col="white",xlab="A2", ylab="A3",
      main="scatter plot of the data")
points(credit[credit$A16=="+","A2"],
		credit[credit$A16=="+", "A3"],col="cornflowerblue",pch=18)
cat("\nHit Enter to add the non-acceptance points:\n")
points(credit[credit$A16=="-","A2"],
		credit[credit$A16=="-", "A3"], col="orange",pch=20)
#x<-sort(credit$A2)

k<-5
pAIC<-c()
pd<-c()
library(splines)
for (i in 1:k){
  for (j in 1:k){
      if(i==1 & j==1){
        optPmodel<-pModel<-
           glm(A16 ~poly(A2,i)*poly(A3,j),family=binomial, data=credit)
           #glm(A16 ~bs(A2,df=i)*bs(A3,df=j),family=binomial, data=credit)
        bestPAIC<-extractAIC(pModel)[2]
        besti<-i
        bestj<-j
        pd<-c(pd,extractAIC(pModel)[1])
        pAIC<-c(pAIC,bestPAIC)
      }else{
      # Now find the best value of k (minimum AIC) and save its model
      # for polynomials B splines and natural splines
      pModel<-
           glm(A16 ~poly(A2,i)*poly(A3,j),family=binomial, data=credit)
           #glm(A16 ~bs(A2,df=i)*bs(A3,df=j),family=binomial, data=credit)
        if((tmp<-extractAIC(pModel)[2]) < bestPAIC){
         bestPAIC<-tmp
         optPmodel<-pModel
         besti<-i
         bestj<-j
        }
       pd<-c(pd,extractAIC(pModel)[1])
       pAIC<-c(pAIC,tmp)
      }
     }
}

#dev.new()
# For graphics drawing we generate a grid of data and put them on the
# data frame newPts. NewA2 and newA3 are the A2 and A3 coordinates of
# new points ranging from smallest to largest A2 (A3), and with
# increments of 1 (0.5 for A3):
newA2<-seq(mA2<-floor(min(credit$A2)),MA2<-ceiling(max(credit$A2)),by=1)
newA3<-seq(mA3<-floor(min(credit$A3)),MA3<-ceiling(max(credit$A3)),by=1/2)

#Now set up the newPts data frame:
#newPts<-data.frame(A2=c(), A3=c())
ii<-c()
jj<-c()
for (i in newA2){
   for (j in newA3){
      # newPts<-rbind(newPts,data.frame(A2=i, A3=j)) #add rows
       ii<-c(ii,i)
       jj<-c(jj,j)
   }
}

newPts<-data.frame(A2=ii, A3=jj) #add rows
# build the predictor for all the new points
optPmodel<-glm(A16 ~ poly(A2,besti)*poly(A3,bestj),family=binomial,data=credit)
nlogitPredP<-predict(optPmodel, newPts, type="response")
readline("\nnLogitPred for polynomials predicted\n")

# Add the classification for all the new points made by QDA
newPts<-cbind(newPts,data.frame(A16=ifelse(nlogitPredP>0.5,'+','-')))

type<-"Polynomials"
d<-extractAIC(optPmodel)[1]
plot(credit[,"A2"], credit[,"A3"], col="white", 
	main=paste("nonparametric logistic, degree=",d,", type= ", type))
points(credit[credit$A16=="+","A2"],
	credit[credit$A16=="+", "A3"], col="cornflowerblue",pch=18)
points(credit[credit$A16=="-","A2"],
	credit[credit$A16=="-", "A3"], col="orange",pch=20)
# Add the scatter plot of all new points classified as '+' (in blue) and '-'
# (in orange) (cex=2 means draw a '.' twice as large 
points(newPts[newPts$A16=='+',"A2"],newPts[newPts$A16=='+',"A3"],
					pch='.',cex=1.5,col="cornflowerblue")
points(newPts[newPts$A16=='-',"A2"],newPts[newPts$A16=='-',"A3"],
					pch='.',cex=1.5,col="orange")
# Drawing the boundary line for logistic model: We walk over all points in newPts,
# and if the class of a point and the one right above it, or right to
# its right or right to its northeast is different, then we create a new
# data point between the two. We will then plot these data which
# approximately depicts the classification border
last<-function(x){return (x[length(x)])}
first<-function(x){return (x[1])}
nlogitBoundP<-data.frame(A2=c(), A3=c())

for(i in newA2){
   for (j in newA3){
     if (i==last(newA2) | j==last(newA3)) break;
     if (newPts[newPts$A2==i & newPts$A3==j, "A16"] !=
           newPts[newPts$A2==i+1 & newPts$A3==j, "A16"])
        nlogitBoundP<-rbind(nlogitBoundP,data.frame(A2=i+0.5, A3=j))
     if (newPts[newPts$A2==i & newPts$A3==j, "A16"] !=
           newPts[newPts$A2==i & newPts$A3==j+0.5, "A16"])
        nlogitBoundP<-rbind(nlogitBoundP,data.frame(A2=i, A3=j+0.25))
     if (newPts[newPts$A2==i & newPts$A3==j, "A16"] !=
           newPts[newPts$A2==i+1 & newPts$A3==j+0.5, "A16"])
        nlogitBoundP<-rbind(nlogitBoundP,data.frame(A2=i+0.5, A3=j+0.25))
     if (first(newPts$A3)!=j) 
      if  (newPts[newPts$A2==i & newPts$A3==j, "A16"] !=
           newPts[newPts$A2==i+1 & newPts$A3==j-0.5, "A16"])
        nlogitBoundP<-rbind(nlogitBoundP,data.frame(A2=i+0.5, A3=j-0.25))
   }
}
# plot the boundary points:
cat("\nDrawing the boundary of the nonparametric polynomial logistic model:\n")
points(nlogitBoundP$A2,nlogitBoundP$A3,pch=20, cex=0.8, col="darkgreen")

readline("Hit Enter to see the contour and image values")
######## Now draw using contours and image functions #########
   # Preparing for contour plots. The z matrix will contain predicted
   # probabilities for '-' (credit rejected):
   #nlogitPredP<-predict(optPmodel, newPts, type="response")
   z<-matrix(nrow=length(newA2),ncol=length(newA3))
   for (i in 1:length(newA2))
     for(j in 1:length(newA3)){
      # mapping from linear array to matreix
      z[i,j]=nlogitPredP[j+(i-1)*length(newA3)]
      }

   # Setting up the graphics pane. NewPts is a grid of points on the A2-A3
   # plane and then the optPmodel is used to predict (red for predicting to reject 
   # credit, blue for predicting accept credit) We use image and contour to
   # draw the boundary between red and blue regions.  Image makes a bit more
   # resolution and instead of just blue and red, it changes color for larger
   # or smaller probabilities. This is sometimes called "heat map"
   
   dev.new()
   image(newA2,newA3,z,col=rainbow(10,start=.55, end=.1),
          main=paste("nonparametric logistic, degree=",d,", type= ", type))
   # For contour we set levels=0.5 so only the boundary between classes are
   # drawn, add = TRUE so that the contour is added to the existing graph

   contour(newA2,newA3,z,levels=c(0.5),add=TRUE,col="darkgreen", 
            drawlabels=FALSE,lwd=2)
   points(newPts[newPts$A16=='+',"A2"],newPts[newPts$A16=='+',"A3"],
					pch='.',cex=2,col="cornflowerblue")
   points(newPts[newPts$A16=='-',"A2"],newPts[newPts$A16=='-',"A3"],
				pch='.',cex=2,col="orange")
   dev.new()
   # Redraw contour without the image heatmap
   contour(newA2,newA3,z,levels=c(0.5),col="darkgreen",
        drawlabels=FALSE,lwd=2,
        main=paste("nonparametric logistic, degree=",d,", type= ", type))
   points(newPts[newPts$A16=='+',"A2"],newPts[newPts$A16=='+',"A3"],
					pch='.',cex=2,col="cornflowerblue")
   points(newPts[newPts$A16=='-',"A2"],newPts[newPts$A16=='-',"A3"],
				pch='.',cex=2,col="orange")
   points(credit[credit$A16=="+","A2"],
		credit[credit$A16=="+", "A3"], col="cornflowerblue",pch=20)
   points(credit[credit$A16=="-","A2"],
		credit[credit$A16=="-", "A3"], col="orange",pch=20)

