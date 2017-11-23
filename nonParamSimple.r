# In this file we create a function f(x)=2*x + cos(x^2) -3*ln(x+1) as
# the "true function". Next we generate data data that represents points
# of the form (x, f(x) + err) where err is normal with mean 0 and some
# standard deviation. We now use this data to estimate f(x) by
# polynomials and splines (B-splines and natural splines)

################### Simulation step ##################
# take a somewhat arbitrary function:
f<-function(x){return(2*x + cos(x*x/4) - 3*log(x+1)^2)}
# plot it to see how it looks like
plot(x<-seq(0,10,by=0.02), y<-sapply(x,'f'),'l', main="Linear")

# Now take a set of 1000 xx, and simulate yy by setting it to yy=f(xx)+err
# where err is iid normal N(0,sigma=0.9)
xx<-seq(0,10,length.out=1000)
yy<-sapply(xx,'f')+rnorm(length(xx),0,0.9)

# From now on we work with the noisy data xx and yy. 
# First put them in  a data frame
dat<-data.frame(X=xx,Y=yy)
# Now plot these noisy data to visualize them
points(xx,yy,pch=20)

# Now start playing around with different kinds of linear models:
# First try a straight line:
model1<-lm(Y ~ X, data=dat)
print(summary(model1))
lines(xx, model1$coefficients[1]+model1$coefficients[2]*xx, col="red")
readline("Hit enter  to see the quadratic model")

# Now try a quadratic model y=b0 + b1 x + b2 x^2:
dev.new()
plot(x,y,'l', main="Quadratic")
points(xx,yy,pch=20)
model2<-lm(Y ~ X + I(X^2), data=dat)
print(summary(model2))
lines(xx, model2$coefficients[1]+model2$coefficients[2]*xx 
                      +model2$coefficients[3]*xx^2, col="red")
# Now let the user decide what kind of model (polynomial, B-spline or
# N-spline) to use and how many degrees of freedom:

# First, load the splines library for spline regression
library(splines)
repeat{
   # read the degree of polynomial and spline (effective number of
   # variables). Make sure to use as.numeric since anything read by
   # readline is interpreted as string:
   d<-as.numeric(readline("\nWrite the degree (0 to quit):\n"))
   if (d<1) break
   type<-readline("\nWrite 'p' for polynomial, 'b' for B-spline and 'n' for natural spline\n")
   # p: Use regression using polynomials of degree d.
   #
   # b: Use regression using B-Splines. These are piecewise cubic splines
   # which tie together smoothly, that is at knot points agree both on
   # value, and the value of the first and second derivatives. The
   # default is degree 3 polynomial spline.
   #
   # n: Use regression using natural splines. These are almost identical to
   # B-splines, except that they also make sure that the point x=0
   # and the maximal x value the spline function is zero. The default is
   # degree 3 polynomial spline.
   dev.new()
   if(type=='p'){model3<-lm(Y~poly(X,d), data=dat)}
   else if(type=='b'){model3<-lm(Y~ bs(X,df=d), data=dat)}
   else if(type=='n'){model3<-lm(Y~ ns(X,df=d), data=dat)}
   tp<-function(c) # An auxiliary function
         {ifelse(c=='p',"polynomials",ifelse(c=='b',"B-spline","n-spline"))}
   plot(x,y,'l',main=paste("df=",d,"type=",tp(type)))
   #points(xx,yy,pch=20)
   print(summary(model3))
   lines(xx,predict(model3,data.frame(X=xx)), col="blue")
}


