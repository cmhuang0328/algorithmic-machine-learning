# In this file we draw graphs of the likelihood functions for n the discrete
# Bernoulli trials and for n normal data.

# First, the Bernoulli trial. Set an underlying probability p0, and number of
# trials n. Next simulate x, a vector of zeros and ones of length n, where each
# one has a probability p0 of being on. 
p0=1/3
n=100
x=ifelse(runif(n,0,1)<=p0,1,0) 
# Use this vector to graph both the likelihood function, and  log likelihood
# function
p<-seq(0,1,by=0.01)
lik<- p^(sum(x))*(1-p)^(n-sum(x))
loglik<- sum(x)*log(p)+(n-sum(x))*log(1-p)

# If you want to use basic R graphics uncomment the following lines:

#dev.new()
#plot(p,lik,'l', col="red", main=paste("Likelihood function for x=",toString(x)))
#dev.new()
#plot(p,loglik,'l', col="blue",main=paste("Log likelihood function for x=",toString(x)))

# If you want slightly fancier graphics install the ggplot2 library and run the
# following:

library(ggplot2)
pl<-ggplot(df<-data.frame(p=p,l=loglik), aes(x=p,y=l)) +
    geom_line(data=df,aes(p,l),color="red") +
    labs(title=paste("p0=", format(p0,digits=2), ", n=", n," Bernoulli data x="
           ))#,toString(x)))
print(pl)
dev.new()
ppl<-ggplot(df<-data.frame(p=p,l=lik), aes(x=p,y=l)) + 
    geom_line(data=df,aes(p,l),color="blue") +
    labs(title=paste("p0=", format(p0,digits=2), ", n=", n," Bernoulli data x="
           ))#,toString(x)))
print(ppl)

readline("Hit Enter to see the graphs for the normal distribution")
# Now set a mean mu and standard deviation sigma, and length n. Simulated n iid
# normal observations with mean mu and standard deviation sigma 
mu=6
sigma=2
n=3
y<- rnorm(n,mu,sigma)

# Now plot the 3-dimaentiosnal likelihood function for mu and sigma together
mu1=seq(mu-5,mu+5,by=0.1)
sigma1=seq(0.5,2*sigma-0.5,by=0.1)
# For a 3-D plot we need three coordinates x=mu, y=sigma and z=lik(mu, sigma)
# or z=loglik(mu, sigma). $x, and y are vecrtors, but z must be a matrix where
# z[i,j]=lik(x[i],y[j]) or z[i,j]=loglik(x[i],y[j]).

z=matrix(nrow=length(mu1),ncol=length(sigma1)) #z =lik(mu,sigma)
z1=z #z1 =loglik(mu,sigma)
for (i in 1:length(mu1))
   for (j in 1:length(sigma1)){
      z[i,j]=prod(dnorm(y,mu1[i],sigma1[j]))
      z1[i,j]=sum(log(dnorm(y,mu1[i],sigma1[j])))
   }

# To have a fancy graphics where you can rotate the image make sure to insrtall
# the "rgl" library
library("rgl")
persp3d(mu1,sigma1,z, col="darkgreen",xlab="mu",ylab="sigma",zlab="Lik")
open3d()
persp3d(mu1,sigma1,z1, col="maroon",xlab="mu",ylab="sigma",zlab="LogLik")

# For plain graphics with R uncomment the following line
#dev.new()
#persp(mu1,sigma1,z, col="darkgreen",xlab="mu", ylab="sigma", zlab="Lik")
#dev.new()
#persp(mu1,sigma1,z1, col="maroon",xlab="mu", ylab="sigma", zlab="logLik")

