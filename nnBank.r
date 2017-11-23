# In this file we run various neural net models on the bank data and 
# compare the results with linear model. Note that for neural nets, its
# is best to scale the data. 

# Read data and add Age and YrsAtBank to it
bank<-read.csv("bank.csv")
bank<-cbind(bank, Age=95-bank$YrBorn)
bank<-cbind(bank, YrsAtBank=95-bank$YrHired)
# The neuralnet package does not seem to work with categorical data
# very well. So we need to make a 0/1 variable for gender
bank=cbind(bank,Gender1=ifelse(bank$Gender=="Female",1,0))
# Scaling is essential for neural nets. We also use it for linear
# regression. Below we use a scaling that uses the range of data (from
# smallest to largest and shoft it by mininmum (See ?scale for more
# information)
maxs=apply(bank[,c("Salary","Age","YrsAtBank","YrsPrior")],2,max)
mins=apply(bank[,c("Salary","Age","YrsAtBank","YrsPrior")],2,min)
bank[,c("Salary","Age","YrsAtBank","YrsPrior")]<-
      as.data.frame(scale(bank[,c("Salary","Age","YrsAtBank","YrsPrior")], 
                    center=mins, scale=maxs-mins))
# To test the method we create training and test sets. We will compare
# both the training and test errors
samp=sample(1:nrow(bank),floor(nrow(bank)*0.9))
bankTrain=bank[samp,]
bankTest=bank[-samp,]

# Build linear models first: 
lm1=lm(Salary~Age+YrsAtBank, data=bankTrain)
predlm1Train=predict(lm1, newdata=bankTrain)
# Compute the error using sum of squares (optionally divide by the
# length to get MSE, to do so uncomment the divisions
mseTrain.lm1=sum((predlm1Train-bankTrain$Salary)^2)#/nrow(bankTrain)
predlm1Test=predict(lm1, newdata=bankTest)
mseTest.lm1=sum((predlm1Test-bankTest$Salary)^2)#/nrow(bankTest)

# It seems that neuralnet package cannot handle categorical features. We
# need to explicitly create dummy variables. Gender1 replaces Gender.
# Build the second linear model
lm2=lm(Salary~Age+YrsAtBank+Gender1, data=bankTrain)
predlm2Train=predict(lm2, newdata=bankTrain)
predlm2Test=predict(lm2, newdata=bankTest)
# Compute the second mse for the linear model
mseTrain.lm2=sum((predlm2Train-bankTrain$Salary)^2)#/nrow(bankTrain)
mseTest.lm2=sum((predlm2Test-bankTest$Salary)^2)#/nrow(bankTest)


##########Neural Net Model 
library("neuralnet")
# We set layers to a vector. The length of th vector is the number of
# layers. The numbers in the vector are the number of hidden
# variables in the corresponding layer. Experiment with different
# values:
layers=c(3,2)
nnBank1<-neuralnet(Salary~Age+YrsAtBank,hidden=layers, 
         linear.output=T, err.fct="sse",data=bankTrain)
plot(nnBank1)
nnPred1Train=compute(nnBank1, bankTrain[,c("Age","YrsAtBank")])
nnPred1Test=compute(nnBank1, bankTest[,c("Age","YrsAtBank")])
print("Compare the weights extracted from the model by nnBank1$weights
with the numbers on the graph:")
print(nnBank1$weights)
readline("Hit enter to continue")
pred1Train=nnPred1Train$net.result
pred1Test=nnPred1Test$net.result
mse1Train.nn=sum((pred1Train-bankTrain$Salary)^2)#/nrow(bankTrain)
mse1Test.nn=sum((pred1Test-bankTest$Salary)^2)#/nrow(bankTest)
print(paste("For Model1 Salaray~Age+YrsAtBank: Training MSE(LM)=",
             mseTrain.lm1, "Training MSE(NN)= ", mse1Train.nn))
print(paste("For Model1 Salaray~Age+YrsAtBank: Test MSE(LM)=",
             mseTest.lm1, "Test MSE(NN)= ", mse1Test.nn))

nnBank2<-neuralnet(Salary~Age+YrsAtBank+Gender1,hidden=layers,
     linear.output=T, err.fct="sse",data=bankTrain)
plot(nnBank2)
nnPred2Train=compute(nnBank2, bankTrain[,c("Age","YrsAtBank","Gender1")])
nnPred2Test=compute(nnBank2, bankTest[,c("Age","YrsAtBank","Gender1")])
pred2Train=nnPred2Train$net.result
pred2Test=nnPred2Test$net.result
mse2Train.nn=sum((pred2Train-bankTrain$Salary)^2)#/nrow(bankTrain)
mse2Test.nn=sum((pred2Test-bankTest$Salary)^2)#/nrow(bankTest)
# print the training and test error for both models. Note that the
# error on the graph is half the training error reported  below.
print(paste("For Model2 Salaray~Age+YrsAtBank+Gender1: Training MSE(LM)=",
             mseTrain.lm2, "Training MSE(NN)= ", mse2Train.nn))
print(paste("For Model2 Salaray~Age+YrsAtBank: Test MSE(LM)=",
             mseTest.lm2, "Test MSE(NN)= ", mse2Test.nn))

