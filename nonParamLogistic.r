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

readline("In this file we compare various non-parametric different classification 
techniques on a credit approval data set from UCI web site.

We are downloading the data from a web site
Hit Enter to continue\n")

# This data has some parts missing (indicated by '?'). To tell the data
# frame that this we add na.string="?". Also note that the data is read
# off a web site directly, and not from a local file. Also, since there
# are no headers, we add headers A1 to A16 using sapply, and pase
# functions
credit <-
read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data",
   na.strings="?",
   col.names=sapply(1:16, function(x){return (paste("A",x,sep=""))}))
#cat("\nnorw(credit) ", nrow(credit), "\n")
#readline()

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



# We plot the combinations of A2 and A3 for acceptance (in
# blue) and no acceptance (in red)
readline("\n Hit Enter to see the scatterplot of A2 vs A3 for
acceptance cases:\n")
# Set up the graphics pane:

cat("\nSetting up the graphics pane:\n")
plot(credit[,"A2"], credit[,"A3"], col="white",xlabel="A2", ylabel="A3",
      main="linear logistic")
#readline()
points(credit[credit$A16=="+","A2"],
		credit[credit$A16=="+", "A3"], col="blue",pch=20)
cat("\nHit Enter to add the non-acceptance points:\n")
#readline()
points(credit[credit$A16=="-","A2"],
		credit[credit$A16=="-", "A3"], col="red",pch=20)
x<-sort(credit$A2)
####################Build the Linear Logistic regression#############
cat("\nApplying the logistic regression to the credit data.\n")

logitModel <- glm(A16 ~ A2 + A3, family=binomial, data=credit)

cat("\nDrawing the boundary by logistic regression (dark green):\n")
#readline()
lines(x, 
  (-logitModel$coefficients[1]-logitModel$coefficients[2]*x)/logitModel$coefficients[3],'l',
col="darkgreen")

##################Now build the Nonparametric logistic model#########
readline()
library(splines)
d<-4
#nlogitModel <- glm(A16 ~ (A2+I(A2^2) +I(A2^3))*(A3+I(A3^2)+I(A3^3)), family=binomial,data=credit)
#nlogitModel <- glm(A16 ~ poly(A2,d)*poly(A3,d), family=binomial,data=credit)
#type<-"polynomial"
nlogitModel <- glm(A16 ~bs(A2,df=d)*bs(A3,df=d) , family=binomial, data=credit)
type<-"B-spline"
#nlogitModel <- glm(A16 ~ns(A2,df=d)*ns(A3,df=d) , family=binomial, data=credit)
#type<-"Natural-spline"
print(paste("Developing the nonparametric logistic Model ",type," Hit Enter:"))
print(summary(nlogitModel))

# For graphics drawing we generate a grid of data and put them on the
# data frame newPts. NewA2 and newA3 are the A2 and A3 coordinates of
# new points ranging from smallest to largest A2 (A3), and with
# increments of 1 (0.5 for A3):
newA2<-seq(mA2<-floor(min(credit$A2)),MA2<-ceiling(max(credit$A2)),by=1)
newA3<-seq(mA3<-floor(min(credit$A3)),MA3<-ceiling(max(credit$A3)),by=1/2)

#Now set up the newPts data frame:
newPts<-data.frame(A2=c(), A3=c())
for (i in newA2){
   for (j in newA3){
      newPts<-rbind(newPts,data.frame(A2=i, A3=j)) #add rows
   }
}
# build the predictor for all the new points
nlogitPred<-predict(nlogitModel, newPts, type="response")

# Add the classification for all the new points made by QDA
newPts<-cbind(newPts,data.frame(A16=ifelse(nlogitPred>0.5,'+','-')))

dev.new()
plot(credit[,"A2"], credit[,"A3"], col="white", 
	main=paste("nonparametric logistic, df=",d,", type= ", type))
points(credit[credit$A16=="+","A2"],
	credit[credit$A16=="+", "A3"], col="blue",pch=20)
points(credit[credit$A16=="-","A2"],
	credit[credit$A16=="-", "A3"], col="red",pch=20)
# Add the scatter plot of all new points classified as '+' (in blue) and '-'
# (in red) (cex=2 means draw a '.' twice as large 
points(newPts[newPts$A16=='+',"A2"],newPts[newPts$A16=='+',"A3"],
					pch='.',cex=1.5,col="blue")
points(newPts[newPts$A16=='-',"A2"],newPts[newPts$A16=='-',"A3"],
					pch='.',cex=1.5,col="red")
# Drawing the boundary line for QDA: We walk over all points in newPts,
# and if the class of a point and the one right above it, or right to
# its right or right to its northeast is different, then we create a new
# data point between the two. We will then plot these data which
# approximately depicts the classification border
nlogitBound<-data.frame(A2=c(), A3=c())

last<-function(x){return(x[length(x)])} #An auxiliary function
first<-function(x){return(x[1])} #An auxiliary function
for(i in newA2){
   for (j in newA3){
     if (i==last(newA2) | j==last(newA3)) break;
     if (newPts[newPts$A2==i & newPts$A3==j, "A16"] !=
           newPts[newPts$A2==i+1 & newPts$A3==j, "A16"])
        nlogitBound<-rbind(nlogitBound,data.frame(A2=i+0.5, A3=j))
     if (newPts[newPts$A2==i & newPts$A3==j, "A16"] !=
           newPts[newPts$A2==i & newPts$A3==j+0.5, "A16"])
        nlogitBound<-rbind(nlogitBound,data.frame(A2=i, A3=j+0.25))
     if (newPts[newPts$A2==i & newPts$A3==j, "A16"] !=
           newPts[newPts$A2==i+1 & newPts$A3==j+0.5, "A16"])
        nlogitBound<-rbind(nlogitBound,data.frame(A2=i+0.5, A3=j+0.25))
     if (first(newPts$A3)!=j) 
      if  (newPts[newPts$A2==i & newPts$A3==j, "A16"] !=
           newPts[newPts$A2==i+1 & newPts$A3==j-0.5, "A16"])
        nlogitBound<-rbind(nlogitBound,data.frame(A2=i+0.5, A3=j-0.25))
   }
}
# plot the boundary points:
cat("\nDrawing the boundary of the nonparametric logisitic model:\n")
points(nlogitBound$A2,nlogitBound$A3,pch=20, cex=0.8, col="darkgreen")

####################End of the QDAmodel#############################

