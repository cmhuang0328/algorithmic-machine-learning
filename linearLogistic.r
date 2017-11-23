# In this script we use the German credit acceptance data from the UCI
# archives and estimate linear logistic regression models. we will build
# several models and use analysis of deviance to compare them.
########################## The data#########################
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

#Remove rows with <NA> in A2, A3, A8, A14 and A16 columns. 
credit <-
credit[!is.na(credit$A2)&!is.na(credit$A3)&!is.na(credit$A8)&!is.na(credit$A14)&!is.na(credit$A16),]
#(You can add A3 and A16 as well)
#credit <- credit[!is.na(credit$A2) & !is.na(credit$A2)&!is.na(credit$A16),]
cat("Top rows of credit:\n")
print(head(credit))
readline()

cat("Bottom rows of credit:\n")
print(tail(credit))
readline()

# We plot the combinations of A2 and A3 for acceptance (in
# blue) and no acceptance (in orange)
readline("\n Hit Enter to see the scatterplot of A2 vs A3 for
acceptance cases:\n")
# Set up the graphics pane:

cat("\nSetting up the graphics pane:\n")
plot(credit[,"A2"], credit[,"A3"], col="white",xlab="A2", ylab="A3",
      main="Linear Logistic")

#readline()
points(credit[credit$A16=="+","A2"],
		credit[credit$A16=="+", "A3"], col="cornflowerblue",pch=18)
cat("\nHit Enter to add the non-acceptance points:\n")
#readline()
points(credit[credit$A16=="-","A2"],
		credit[credit$A16=="-", "A3"], col="orange",pch=20)
####################Build the Linear Logistic regression#############
cat("\nApplying the logistic regression to the credit data.\n")

logitModel1 <- glm(A16 ~ A2 + A3, family=binomial, data=credit)
print("summary of logitModel1")
print(summary(logitModel1))

# we can also calculate the error rate of the prediction on the training
# data itself:

# Note: predict behaves the same way as predict.glm, and is applicable to
# models by the glm function. 
# Predict will return the value of the linear function that is
# bhat0+bhat1*newsA2+bhat2*newA3, which is equal to predicted log odds
# log(p/(1-p)). If you provide no data to predict then it will assume the
# original training data (in this case the credit data frame) is the one
# to be predicted. This is used for finding the error rate on the
# *training data*
print(tbl1<-table(ifelse(predict.glm(logitModel1)>0,"+","-"), credit1$A16))
print(paste("Error Rate on training data:  ", (tbl1[2,1]+tbl1[1,2])/sum(tbl1)))

# Now draw the classification regions and the boundary between regions:

print("Drawing the boundary by logistic regression (dark green)")
print("Note that this is exactly the equation bhat0+bhat1*A2+ bhat2*A3=0")
print(paste(logitModel1$coefficients[1],"+ ",
            logitModel1$coefficients[2]," * A2 + ",
            logitModel1$coefficients[3]," * A3 = 0"))
#readline()
x<-sort(credit$A2)
lines(x, 
  (-logitModel1$coefficients[1]-logitModel1$coefficients[2]*x)/logitModel1$coefficients[3],'l',
col="darkgreen")
# Now create a set of new points with new values of A2 and A3 features.
# These pints are generated so that they make a fine grid in the A2-A3 plane.
# Organize these new points in a data frame called newPts.
newA2<-seq(mA2<-floor(min(credit$A2)),MA2<-ceiling(max(credit$A2)),by=1)
newA3<-seq(mA3<-floor(min(credit$A3)),MA3<-ceiling(max(credit$A3)),by=1/2)
newPts<-data.frame(A2=c(), A3=c())
for (i in newA2){
   for (j in newA3){
      newPts<-rbind(newPts,data.frame(A2=i, A3=j)) #add rows
   }
}
# Now use the linear logitModel1 model to predict whether each point in the
# grid is accepted blue)or rejected (orange).  Note that the option
# type="response" causes predict to give *probability* of A16=1 (that is
# accept). Without it would give *log of odds* as explained eralier.
logitPred1<-predict(logitModel1, newPts, type="response")
# predict accept or reject based on the probabilities.
newPts<-cbind(newPts,data.frame(A16=ifelse(logitPred1>0.5,'+','-')))
points(newPts[newPts$A16=='+',"A2"],newPts[newPts$A16=='+',"A3"],
		pch='.',cex=2,col="cornflowerblue")
points(newPts[newPts$A16=='-',"A2"],newPts[newPts$A16=='-',"A3"],
	pch='.',cex=2,col="orange")
points(credit[credit$A16=="+","A2"],
		credit[credit$A16=="+", "A3"], col="cornflowerblue",pch=20)
points(credit[credit$A16=="-","A2"],
		credit[credit$A16=="-", "A3"], col="orange",pch=20)
print("We now generate a \"heat map\"")
print("The orange areas corresponds to high probability of '-'")
print("The blue areas corresponds to high probability of '+'")
print("The other areas correspond to probabilities to more modest probabilities")
readline("Hit Enter to see the heat map")

dev.new()
# The function image creates a "heat map" that is for the range of
# values z (here z[i,j]=Pr[y=1 | newA2[i], newA3[j]]). 
# We specify the range of colors from orange (code 0.09) to blue (code 
# 2/3=0.67) and in 20 levels. 
z<-matrix(nrow=length(newA2),ncol=length(newA3))
for (i in 1:length(newA2))
  for(j in 1:length(newA3)){
   # mapping from linear array to matrix
   z[i,j]=logitPred1[j+(i-1)*length(newA3)]
   }
image(newA2,newA3,z,col=rainbow(20,start=0.09, end=0.67),
       main="Linear Logistic")
# For contour we set levels=0.5 so only the boundary between classes are
# drawn, add = TRUE so that the contour is added to the existing graph
# Note that this contour is exactly the line bhat0+bhat1*A2+bhat2*A3=0

contour(newA2,newA3,z,levels=c(0.5),add=TRUE,col="darkgreen", 
         drawlabels=FALSE,lwd=2)
points(newPts[newPts$A16=='+',"A2"],newPts[newPts$A16=='+',"A3"],
		pch='.',cex=2,col="cornflowerblue")
points(newPts[newPts$A16=='-',"A2"],newPts[newPts$A16=='-',"A3"],
	pch='.',cex=2,col="orange")

##################Now build the logistic models with more features#########
# we now add features A8 and A14 to the model. 
readline("Hit Enter to see model with A8 and A14 added:")
logitModel2 <- glm(A16~A2+A3+A8+A14, family=binomial, data=credit)
print(summary(logitModel2))

print("logitModel2' serror rate:")

readline("Hit Enter to see comparison of the two models with anova function:")

print(tbl2<-table(ifelse(predict.glm(logitModel2)>0,"+","-"), credit1$A16))
print(paste("Error Rate:  ", (tbl2[2,1]+tbl2[1,2])/sum(tbl2)))

# anova in this case is the "analysis of deviance". You should specify
# test="LRT" which means "Likelihood Ratio Test" so that it can compare two
# *nested* models and test if the "complete model" (here logitModel2) adds
# significant variables to the reduced model (here logitModel1).
print(anova(logitModel1, logitModel2,test="LRT"))

# You can also apply anova to a single model, in which it checks the
# model by adding one variable at a time and in the order they were
# specified in the model.
print(anova(logitModel2,test="LRT"))

print(cat("It is also possible to check individual variables by analysis of
deviance. For instance, in logitModel2 we want to see the significance
of variable A4. Then according to command anova(logitModel2,test=\"LRT\")
the significance is 12.83%. Compare this to 13.79% from the summary. Not
the same, but not too far either.\n"))
