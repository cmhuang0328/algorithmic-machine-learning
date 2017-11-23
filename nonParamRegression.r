# In this file we study non-parametric regression through simulation
# We first create a function f(x)=2*x + cos(x^2) -3*ln(x+1) as
# the "true function" (Feel free to experiment with other functions). 
# Next we generate data that represents points
# of the form (x, f(x) + err) where err is normal with mean 0 and some
# standard deviation. We then use this data to estimate f(x) by
# polynomials and splines (B-splines and natural splines)

# Step one simulate the function f by generating 1000 (y,x) pairs where
# y=f(x) + esp where eps is a normal error with mean=0 and stdev=0.9
f<-function(x){return(2*x + cos(x*x/4) - 3*log(x+1)^2)}
plot(x<-seq(0,10,length.out=200), y<-sapply(x,'f'),'l', 
              main="The original function", xlab="x", ylab="y")
#xx<-seq(0,10,by=0.02)
# Construct a random sample with normal error
xx<-seq(0,10,length.out=1000)
yy<-sapply(xx,'f')+rnorm(length(xx),0,0.9)
points(xx,yy,pch=20)
readline("hit Enter to build the linear models")

# Make a data frame from the sample. We use this sample to reover the
# approximation to the original function f().
dat<-data.frame(X=xx,Y=yy)

# Load the splines library for spline regression
library(splines)
# Set how many different degrees for polynomials, and df for spline we
# test
k<-100  
# We want to collect the AIC for all the three models to plot and compare
# Below the variable i is varied up to k and the AIC values are
# collected. For each type (polynomial, B-spline and N-spline) the best
# value is recorded. We also keep track of all values and plot them.
pAIC<-bAIC<-nAIC<-c()
for (i in 4:k){
   if(i==4){
      optPmodel<-pModel<-lm(Y~poly(X,i), data=dat)
      # extractAIC returns a vector, the first number is number of
      # parameters and the second number is the value of 
      # AIC =-2*logLik +2*number_of_parameters
      bestPAIC<-extractAIC(pModel)[2] # For first iteration initialize
      pAIC<-c(pAIC,bestPAIC)
      optBmodel<-bModel<-lm(Y~bs(X,df=i), data=dat)
      bestBAIC<-extractAIC(bModel)[2]
      bAIC<-c(bAIC,bestBAIC)
      optNmodel<-nModel<-lm(Y~ns(X,df=i), data=dat)
      bestNAIC<-extractAIC(nModel)[2]
      nAIC<-c(nAIC,bestNAIC)
   }else{
   if(i<28){ # polynomials of degree larger than 28 will crash  due to
             # numerical issues
   # Now find the best value of k (minimum AIC) and save its model
   # for polynomials B splines and natural splines
   pModel<-lm(Y~poly(X,i), data=dat)
     if((tmp<-extractAIC(pModel)[2]) < bestPAIC){
      bestPAIC<-tmp
      optPmodel<-pModel
     }
    pAIC<-c(pAIC,tmp)
   }

   bModel<-lm(Y~ bs(X,df=i), data=dat)
   if((tmp<-extractAIC(bModel)[2]) < bestBAIC){
      bestBAIC<-tmp
      optBmodel<-bModel
   }
   bAIC<-c(bAIC,tmp)

   nModel<-lm(Y~ ns(X,df=i), data=dat)
   if((tmp<-extractAIC(nModel)[2]) < bestNAIC){
      bestNAIC<-tmp
      optNmodel<-nModel
   }
   nAIC<-c(nAIC,tmp)
  }
}
# We now have all optimal models:

print(paste("best poly AIC: ",bestPAIC))
print(summary(optPmodel))
print(paste("best B-spline AIC: ",bestBAIC))
print(summary(optBmodel))
print(paste("best N-spline AIC: ",bestNAIC))
print(summary(optNmodel))
readline("Hit Enter to see plots of the optimal polynomial model")
dev.new()
plot(x,y,'l',main=paste("df=",extractAIC(optPmodel)[1],"type=polynomial"))
#points(xx,yy,pch=20)
lines(xx,predict(optPmodel), col="blue")
readline("Hit Enter to see plots of the optimal B-spline model")
dev.new()
plot(x,y,'l',main=paste("df=",extractAIC(optBmodel)[1],"type=B-spline"))
lines(xx,predict(optBmodel,data.frame(X=xx)), col="blue")
readline("Hit Enter to see plots of the optimal N-spline model")
dev.new()
plot(x,y,'l',main=paste("df=",extractAIC(optNmodel)[1],"type=N-spline"))
lines(xx,predict(optNmodel,data.frame(X=xx)), col="blue")

# Plot the AIC for polynomials, B-splines and natural splines
readline("\nHit Enter to see plot of polynomial AIC against degree") 
dev.new()
plot(1:length(pAIC),pAIC,pch=20,col="orange",
     main="Plot of AIC vs df for polynomials", xlab="df", ylab="AIC")
readline("\nHit Enter to see plot of B spline AIC against degree")
dev.new()
plot(1:length(bAIC),bAIC,pch=20,col="red",
   main="Plot of AIC vs df for B-splines", xlab="df", ylab="AIC")
readline("\nHit Enter to see plot of natural spline AIC against degree")
dev.new()
plot(1:length(nAIC),nAIC,pch=20,col="blue",
       main="Plot of AIC vs df for N-splines", xlab="df", ylab="AIC")
