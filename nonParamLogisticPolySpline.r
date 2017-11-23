# In this file we examine nonparametric logistic model on the credit
# data. The problem model is as follows:
#  log odds(A16 given A2 and A3 = f(A2, A3)
# where the function f is unknown. Note that the relation f(A2,A3)=0
# defines the boundary between those approved for credit (+ blue) and
# those not approved (- red). if f(A2,A3) < 0 then we classify as - and
# if f(A2,A3)>0 we classify as +. 
# In this file the function f(A2,A3) is estimated by three classes of
# functions: polynomials: poly(A2,i)*poly(A3,j) where i and j range from
# 1 to 10, B-splines: bs(A2,df=i)*bs(A3,df=j) and natural splines:
# ns(A2,df=i)*ns(A3,df=j). For each one all models for i=1:10 and j=1:10
# are computed and for each class the one with best AIC score is
# kept. At the bottom of the file we plot the boundaries predicted by
# these three best models.
#
##################### Origin of the data ################
# This file works on data available at a well-known repository of data
# hosted by University of California-Irvine. The website for this
# dataset:
#  http://archive.ics.uci.edu/ml/datasets/Credit+Approval
# 
# The data contains information about individuals and whether they
# were approved  for credit. The problem is that the name of variables
# have been changed for privacy concerns.

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

# We now plot the combinations of A2 and A3 for acceptance (in
# blue) and no acceptance (in red)
cat("\n scatterplot of A2 vs A3 for acceptance (blue) and rejection
(red) cases:\n")
# Set up the graphics pane:

plot(credit[,"A2"], credit[,"A3"], col="white",xlab="A2", ylab="A3",
      main="scatter plot of the data")
points(credit[credit$A16=="+","A2"],
		credit[credit$A16=="+", "A3"],col="cornflowerblue",pch=18)
points(credit[credit$A16=="-","A2"],
		credit[credit$A16=="-", "A3"], col="orange",pch=20)

readline("\nHit Enter to start the loop to find the best B-spline, Natural 
spline and polynomial models\n")
# pAIC, bAIC and nAIC will be arrays containing AIC values of all
# models, and pd, bd, and nd will have the degrees of freedom. We need
# these to plot df vs AIC for polynomials, B splines and natural splines
k<-10
pAIC<-bAIC<-nAIC<-rep(NA,k^2)
pd<-bd<-nd<-rep(NA, k^2)
library(splines)  #need to load this library for use of splines. It is
                  #already installed, so you don't need to install it
# Run over all combinations of i and j from 1 to k and for each pair we
# compute polynomial, B-spline and natural spline models and keep track
# of thier AIC. 
for (i in 1:k){
  for (j in 1:k){
      if(i==1 & j==1){ #First iteration set initial optimal values
        optPmodel<-pModel<-
           glm(A16 ~poly(A2,i)*poly(A3,j),family=binomial, data=credit)
        bestPAIC<-extractAIC(pModel)[2]
        besti<-i #remember the i, j with lowest AIC so far
        bestj<-j
        pd[(i-1)*k+j]<-extractAIC(pModel)[1]
        pAIC[(i-1)*k+j]<-bestPAIC

        optBmodel<-bModel<-
           glm(A16 ~bs(A2,df=i)*bs(A3,df=j),family=binomial, data=credit)
        bestBAIC<-extractAIC(bModel)[2]
        bd[(i-1)*k+j]<-extractAIC(bModel)[1]
        bAIC[(i-1)*k+j]<-bestBAIC

        optNmodel<-nModel<-
           glm(A16 ~ns(A2,df=i)*ns(A3,df=j),family=binomial, data=credit)
        bestNAIC<-extractAIC(nModel)[2]
        nd[(i-1)*k+j]<-extractAIC(nModel)[1]
        nAIC[(i-1)*k+j]<-bestNAIC
      }else{
      # Now find the best value of k (minimum AIC) and save its model
      # for polynomials B splines and natural splines
      pModel<-
           glm(A16~poly(A2,i)*poly(A3,j),family=binomial, data=credit)
        if((tmp<-extractAIC(pModel)[2]) < bestPAIC){
         bestPAIC<-tmp
         optPmodel<-pModel
         besti<-i #remember the i, j with lowest AIC so far
         bestj<-j
        }
       pd[(i-1)*k+j]<-extractAIC(pModel)[1]
       pAIC[(i-1)*k+j]<-tmp

      bModel<-
           glm(A16 ~bs(A2,df=i)*bs(A3,df=j),family=binomial, data=credit)
      if((tmp<-extractAIC(bModel)[2]) < bestBAIC){
         bestBAIC<-tmp
         optBmodel<-bModel
      }
      bd[(i-1)*k+j]<-extractAIC(bModel)[1]
      bAIC[(i-1)*k+j]<-tmp

      nModel<-
           glm(A16 ~ns(A2,df=i)*ns(A3,df=j),family=binomial, data=credit)
      if((tmp<-extractAIC(nModel)[2]) < bestNAIC){
         bestNAIC<-tmp
         optNmodel<-nModel
      }
      nd[(i-1)*k+j]<-extractAIC(nModel)[1]
      nAIC[(i-1)*k+j]<-tmp
     }
  }
}


#####################Draw classification by B-Splines##############
print("#####################Draw classification by B-Splines#########")
readline("Hit Enter to see contours and heat map")
# For graphics drawing we generate a grid of data and put them on the
# data frame newPts. NewA2 and newA3 are the A2 and A3 coordinates of
# new points ranging from smallest to largest A2 (A3), and with
# increments of 1 (0.5 for A3):
newA2<-seq(mA2<-floor(min(credit$A2)),MA2<-ceiling(max(credit$A2)),by=1)
newA3<-seq(mA3<-floor(min(credit$A3)),MA3<-ceiling(max(credit$A3)),by=1/2)

#Now set up the newPts data frame:
ii<-c()
jj<-c()
for (i in newA2){
   for (j in newA3){
       ii<-c(ii,i)
       jj<-c(jj,j)
   }
}
newPts<-data.frame(A2=ii, A3=jj) #add rows

# build the predictor for all the new points, Note: type="response"
# gives the predicted probabilities (otherwise log odds will be
# produced).
nlogitPredB<-predict(optBmodel, newPts, type="response")

# Add the predicted classification for all new points 
newPts<-cbind(newPts,data.frame(prob=nlogitPredB,
              A16=ifelse(nlogitPredB>0.5,'+','-')))

type<-"B Spline"
d<-extractAIC(optBmodel)[1]
z<-matrix(nrow=length(newA2),ncol=length(newA3))
for (i in 1:length(newA2))
  for(j in 1:length(newA3)){
   # mapping from linear array to matreix
   z[i,j]=nlogitPredB[j+(i-1)*length(newA3)]
   }
print("We now generate a \"heat map\"")
print("The orange areas corresponds to high probability of '-'")
print("The blue areas corresponds to high probability of '+'")
print("The other areas correspond to probabilities to more modest probabilites")
readline("Hit Enter to see the heat map")

# image function creates a "heat map" that is for the range of
# values z (here z=Pr[y=1]). We specify the range of colors from
# red (code 0) to blue (code 2/3=0.67) and in 20 levels. 
dev.new()
image(newA2,newA3,z,col=rainbow(20,start=0.09, end=0.67),
       main=paste("nonparametric logistic, df=",d,", type= ", type))
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


#####################Draw classification by natural splines##############
print("#####################Draw classification by N-Splines#########")
readline("Hit Enter to see contours and heat map")
# For graphics drawing we generate a grid of data and put them on the
# data frame newPts. NewA2 and newA3 are the A2 and A3 coordinates of
# new points ranging from smallest to largest A2 (A3), and with
# increments of 1 (0.5 for A3):
newA2<-seq(mA2<-floor(min(credit$A2)),MA2<-ceiling(max(credit$A2)),by=1)
newA3<-seq(mA3<-floor(min(credit$A3)),MA3<-ceiling(max(credit$A3)),by=1/2)

#Now set up the newPts data frame:
ii<-c()
jj<-c()
for (i in newA2){
   for (j in newA3){
       ii<-c(ii,i)
       jj<-c(jj,j)
   }
}
newPts<-data.frame(A2=ii, A3=jj) #add rows

# build the predictor for all the new points
nlogitPredN<-predict(optNmodel, newPts, type="response")

# Add the classification for all the new points 
newPts<-cbind(newPts,data.frame(A16=ifelse(nlogitPredN>0.5,'+','-')))

type<-"Natural Spline"
d<-extractAIC(optNmodel)[1]
z<-matrix(nrow=length(newA2),ncol=length(newA3))
for (i in 1:length(newA2))
  for(j in 1:length(newA3)){
   # mapping from linear array to matreix
   z[i,j]=nlogitPredN[j+(i-1)*length(newA3)]
   }
print("We now generate a \"heat map\"")
print("The orange areas corresponds to high probability of '-'")
print("The blue areas corresponds to high probability of '+'")
print("The other areas correspond to probabilities to more modest probabilites")
readline("Hit Enter to see the heat map")

# image function creates a "heat map" that is for the range of
# values z (here z=Pr[y=1]). We specify the range of colors from
# red (code 0) to blue (code 2/3=0.67) and in 20 levels. 
dev.new()
image(newA2,newA3,z,col=rainbow(20,start=0.09, end=0.67),
       main=paste("nonparametric logistic, df=",d,", type= ", type))
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

#####################Draw classification by polynomials##############
print("#####################Draw classification by polynomials #########")
readline("Hit Enter to see contours and heat map")
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

# Add the classification for all the new points 
newPts<-cbind(newPts,data.frame(A16=ifelse(nlogitPredP>0.5,'+','-')))

type<-"Polynomials"
d<-extractAIC(optPmodel)[1]
for (i in 1:length(newA2))
  for(j in 1:length(newA3)){
   # mapping from linear array to matreix
   z[i,j]=nlogitPredP[j+(i-1)*length(newA3)]
   }
print("We now generate a \"heat map\"")
print("The orange areas corresponds to high probability of '-'")
print("The blue areas corresponds to high probability of '+'")
print("The other areas correspond to probabilities to more modest probabilites")
readline("Hit Enter to see the heat map")

# image function creates a "heat map" that is for the range of
# values z (here z=Pr[y=1]). We specify the range of colors from
# red (code 0) to blue (code 2/3=0.67) and in 20 levels. 
dev.new()
image(newA2,newA3,z,col=rainbow(20,start=0.09, end=0.67),
       main=paste("nonparametric logistic, df=",d,", type= ", type))
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



# We now plot the AIC of all models generated against degrees of
# freedom for both A2 and A3. To do so we use the scatterplot3d package.
readline("\nDrawing the df vs AIC for B Splines, hit Enter\n")
pdi<-rep(1:k,k)
pdj<-1+(0:(k^2-1)%/%k)
library(scatterplot3d)
dev.new()
scatterplot3d(pdi,pdj, log(bAIC), pch=20,cex.symbol=0.8, main="B-Spline", 
      xlab="df A2", ylab="df A3", zlab="bAIC", color="orange")
readline("\nDrawing the df vs AIC for Natural Splines, hit Enter\n")
dev.new()
scatterplot3d(pdi,pdj,log(nAIC), pch=20,cex.symbol=0.8, main="Natural Spline", 
      xlab="df A2", ylab="df A3", zlab="nAIC", color="purple")
readline("\nDrawing the df vs AIC for polynomials, hit Enter\n")
dev.new()
scatterplot3d(pdi,pdj,log(pAIC), pch=20,cex.symbol=0.8, main="polynomial", 
      xlab="df A2", ylab="df A3", zlab="pAIC", color="darkblue")

readline("Hit Enter to see the error rates for each model:")

print(tblP<-table(ifelse(predict.glm(optPmodel)>1/2,"+","-"),credit$A16))
print(paste("Error rate for optimal polynomial: ",(tblP[1,2]+tblP[2,1])/sum(tblP)))
print(tblB<-table(ifelse(predict.glm(optBmodel)>1/2,"+","-"),credit$A16))
print(paste("Error rate for optimal B-spline: ",(tblB[1,2]+tblB[2,1])/sum(tblB)))
print(tblN<-table(ifelse(predict.glm(optNmodel)>1/2,"+","-"),credit$A16))
print(paste("Error rate for optimal N-spline: ",(tblN[1,2]+tblN[2,1])/sum(tblN)))
