# INTRODUCTION: Case of A sex discrimination Lawsuit
# Adapted from Winston et al.
#
# A bank is facing gender discrimination suite. The specific charge is
# that its female employees receive substantially smaller salaries than
# its male employees. The bank's employee database is listed in the
# file "bank.csv". For each of its 208 employees the data set includes
# the following information: 
# - EducLev: education level, a categorical variable with categories 1
# (finished highschool), 2 (finished some college courses), 3 (obtained
# bachelor's degree), 4 (took some graduate courses) and 5 (obtained a
# graduate degree).
# - JobGrade: A categorical variable indicating the current job level,
# the possible levels are 1-6 (6 is the highest)
# -YrHired: the year the employee was hired
# -YrBorn: the year the employee was born
# -Gender: a categorical variable with values "Female" and "Male"
# -YrsPrior: number of years of work experience at another bank prior to
# working on this bank
# -PCJob: a categorical yes/no variable depending on whether the
# employee's current job is computer related
# -Salary: current annual salary in the thousands of dollars
#
# This data was gathered in 1995.
# 
# In this file we examine several models and compare them. The main
# objective is to see if in this bank's females employees make a
# 'statistically significant' lower salary than males. 

# Let's first read the data It is in "csv" (comma separated) format and all
# columns have headers. So the following command reads the file and assigns
# titles to columns:

bank <- read.csv("bank.csv")
print(head(bank))
print(tail(bank))

# The next step is to tell R which one of these variables are
# quantitative and which ones are qualitative (that is factors).
# Now, when a column is consisted of nonnumerical data, then R
# knows that it is a factor and will treat it as such
# automatically. For instance, the data under column "Gender" are
# either "Male" or "Female". Suppose we try to use regression
# to see if "Salary" is a function of "Gender". Let's do it!

# Before building the model we can create "boxplots" comparing salaries
# for males and females. This gives us a visual representation and will
# help us build the model in a more informed manner. In general, 
# especially for factors, boxplots are good starting point to compare 
# the response variable for each level. For instance, we can plot salaries
# with respect to gender, with respect to education level, and so on. Again,
# ideally, we should do this *before* building models.
#
cat ("Hit Enter to see salary vs gender\n")
readline()
boxplot(Salary ~ Gender, data=bank)


cat("Hit Enter to see the results of regression model Salary ~ Gender\n")
readline()
model0 <- lm(Salary ~ Gender, data=bank)
print(summary(model0))
readline()

cat("Notice that R automatically made Female the reference and Male 
the factor (which can assume values of zero or one).\n")
cat("We can actually check the design matrix used. Hit Enter to see:\n")
readline()
print(model.matrix(model0))
readline()
cat("Here are the interpretation of the coefficients:\n")
cat("Intercept (b0): This is the mean of the base level, in this case
female employees. So b0 is the estimated mean of female employee
salaries\n")
cat("Intercept: ", coefficients(model0)[1],"\n")
readline()
cat("The coefficient of GenderMale (b1): This factor is the ADDITIONAL 
advantage (or disadvantage if negative) for males' salary over
females'.\n")

cat("Male employees on average get", coefficients(model0)[2], "thousand dollars more than female employees\n")

cat("We can check this information by looking at the means of female
and male salaries separately. Hit Enter to see:\n")
readline()
cat("mean of female salaries",mean(bank[bank$Gender=="Female","Salary"]),"\n")
cat("mean of male salaries",mean(bank[bank$Gender=="Male","Salary"]),"\n")

# Adding more variables to the model:
# Now let's take into account years of experience, that is years
# working in the company. This data is taken in 95. Therefore 
# YrsExper=95-YrHired. We adjoin this data into our data frame:
bank<-cbind(bank, YrsExper=95-bank$YrHired)

# While we are at it, we should add an age value because it is more
# useful than the year employee was born:

bank<- cbind(bank, Age=95-bank$YrBorn)

cat("We added YrsExper=95-YrHired to bank data frame. Also added
Age=95-YrBorn.\n")
cat("Hit Enter to see the new bank data frame\n")
readline()
print(head(bank))
# Before doing the regression let us "visualize" the data. For this
# purpose the scatterplot3d package is necessary. So let's load it.

library(scatterplot3d)

# Now we plot the three dimensional scatter plot of salaries as a
# function of YrsExper and YrsPrior. We will color males blue and
# females red. (See the documentation of scatterplot3d to see the details
# for using this function) 

# First look at them separately:
cat("Hit Return to see scatterplot of Males salaries\n")
readline()
malePoints<-scatterplot3d(bank[bank$Gender=="Male","YrsExper"], 
  bank[bank$Gender=="Male","YrsPrior"], 
  bank[bank$Gender=="Male","Salary"],
  color="darkblue",pch=16,type="h",xlab="Yrs Exper", ylab="Yrs Prior",
zlab="Salary")

cat("Hit Return to see scatterplot of Females salaries\n")
readline()
femalePoints<-scatterplot3d(bank[bank$Gender=="Female","YrsExper"], 
  bank[bank$Gender=="Female","YrsPrior"], 
  bank[bank$Gender=="Female","Salary"],
  color="orange",pch=17,type="h",xlab="Yrs Exper", ylab="Yrs Prior",
zlab="Salary")

# Now let's see them together. scatterplot3d returns two useful
# functions points3d and plane33 that allow us to add points and planes
# to an existing plot

cat("Hit Enter to see the males and females together\n")
readline()
# redraw male point without the vertical lines
malePoints<-scatterplot3d(bank[bank$Gender=="Male","YrsExper"], 
  bank[bank$Gender=="Male","YrsPrior"], 
  bank[bank$Gender=="Male","Salary"],
  color="darkblue",pch=16,xlab="Yrs Exper", ylab="Yrs Prior", zlab="Salary")

# Add female points 
malePoints$points3d(bank[bank$Gender=="Female","YrsExper"], 
  bank[bank$Gender=="Female","YrsPrior"], 
  bank[bank$Gender=="Female","Salary"],
  col="orange",pch=17)


cat("Now we add the effects of work experience to our model. That is
we add YrsExper and YrsPrior to the model:\n")
model1 <- lm(Salary ~ Gender + YrsExper + YrsPrior, data=bank)
cat("Hit Enter to see the refined model:\n")
readline()
print(summary(model1))
readline()
cat("Notice that the advantage of Male over Female is slightly less
than the model without experience.\n")
readline()
cat("Let's do a partial F-test to see if the additional variables are
significant or not\n")
cat("Hit Enter to see the results of the partial F-test:\n")
readline()
print(anova(model0, model1))
readline()

# We can break out the regression formula as follows:
# For Females: Salary = 27.41 + .99 YrsExper + .13 YrsPrior
# For Males: Salary = (27.41 + 8.08) + .99 YrsExper + .13 YrsPrior
# These two models are geometrically two PARALLEL planes
cat("The regression model gives two models of dependence on YrsExper
and YrsPrior, One is for men and one for women\n")

# Now add the two parallel regression planes for females and males. 
# First females:

cat("Hit Enter to see Females' regression plane\n")
readline()
malePoints$plane3d(
  c(coefficients(model1)[1],
  coefficients(model1)[3], coefficients(model1)[4]),col="orange",lty="solid")

# Now add the the Males regression plane
cat("Hit Enter to see Males' regression plane\n")
readline()
malePoints$plane3d(
  c(coefficients(model1)[1]+coefficients(model1)[2], 
  coefficients(model1)[3], coefficients(model1)[4]),col="darkblue",lty="solid")
readline()

# Adding interaction factors:

# First, since YrsPrior was not significant, we will eliminate it from
# the model. We now consider adding Gender*YrsExper as a factor. (This
# will affect not only the intercept but also the different rates by
# which males and females are reward for each year of experience")

cat("Hit Enter to see the effect of interaction term\n")
readline()
model2 <- lm(Salary ~ Gender * YrsExper, data=bank)
print(summary(model2))
readline()

# The interpretation of this model is as follows: 
# For females, that is when GenderMale=0, the equation is:
#             Salary = 34.53 + .28 * YrsExper
# For males, that is when GenderMale=1, the equation is:
#             Salary = (34.53 - 4.1) + (.28 + 1.24) * YrsExper
#                    = 30.43 + 1.52 * YrsExper
# Thus, with the interaction model, the females actually start with a
# higher salary, bu then for each year of experience they get an
# increase significantly less than males (.28 vs. 1.52). 
# Let's plot the two models:
cat("Hit Enter to see the interaction model for males:\n")
readline()
plot(sort(bank$YrsExper), 30.43 + 1.52 *
   sort(bank$YrsExper),col="darkblue",type="l",lwd=2)
cat("Hit Enter to see the interaction model for females:\n")
readline()
lines(sort(bank$YrsExper),34.53+.28*sort(bank$YrsExper),col="orange",type="l",   lwd=2)
readline()

# The interaction model is equivalent to the process where we use the
# model Salary ~ YrsExper to female data only and to Male data only.
# Let's see:
cat("Hit Enter to see regression Salary ~ YrsExper for males:\n")
readline()
print(summary(lm(Salary~YrsExper,data=bank,subset=Gender=="Male")))
readline()
cat("Hit Enter to see regression Salary ~ YrsExper for females:\n")
readline()
print(summary(lm(Salary~YrsExper,data=bank,subset=Gender=="Female")))
readline()

# We now wish to add the effect of education level to our model. However
# since these levels are labeled 1 through 6, R will treat them as
# numerical values. To tell R that a vector should be treated as a
# categorical vector (that is as a factor) the function factor is used.
# Use ?factor to see the full description of this function. Here we are
# going to tell R that the column bank$EducLev is a factor and not a
# numerical data.
bank$EducLev <- factor(bank$EducLev)
cat("Hit Enter to see the result of the command
is.factor(bank$EducLev)\n")
readline()
print(is.factor(bank$EducLev))
readline()
cat("We can even examine levels of EducLev to make sure they are correct.\n")
cat("Hit Enter to see how \"levels\" function shows levels of EducLev:\n")
readline()
print(levels(bank$EducLev))

cat(" Hit Enter to see boxplots of salary vs education level\n")
readline()
boxplot(Salary ~ EducLev, data=bank)

cat("Now let's add the education level to the model (Hit Enter to see
the results)\n")
readline()
model3 <-lm(Salary ~ Gender + YrsExper + YrsPrior + EducLev, data=bank)
print(summary(model3))
readline()
cat("Notice that advantage of Male is further reduced, though it is
still significant\n")
readline()
# We can now conduct multiple partial F-tests. Write the models in order
# of refinements:
cat("Hit Enter to see the partial F-Test for each of the models:\n")
readline()
print(anova(model0, model1, model3))
readline()
# Note that not all education levels have a significant advantage over
# those only with highschool diploma (the base level).

# PCJob factor
# Next we add the PCJob factor to see if it can shed more light to
# the salary difference between male and female employees

# First look at Boxplots:
cat(" Hit Enter to see salary vs PC job\n")
readline()
boxplot(Salary ~ PCJob, data=bank)

#Now add PCJob as a factor to the model:
model4 <- lm(Salary ~ Gender + YrsExper + YrsPrior + EducLev + PCJob, data=bank)
cat("Hit Enter to see the added effect of PCJob factor\n")
readline()
print(summary(model4))
readline()
# We see that even after including all these factors still the female
# employees have a significantly lower salary than males.


# Effect of Job grade
# Finally let's add the effect of Job Grade. First we need to turn this
# into a factor:
bank$JobGrade<-factor(bank$JobGrade)

# Next look at the Boxplots for job grade:
cat(" Hit Enter to see salary vs job grade\n")
readline()
boxplot(Salary ~ JobGrade, data=bank)

# Now do the regression
model5 <- lm(Salary~Gender + YrsExper + YrsPrior + EducLev + PCJob +
  JobGrade, data=bank)
cat("Hit Enter to see the effect of Job grade\n")
readline()
print(summary(model5))
readline()
# Finally the male advantage has been reduced to about $2563  over
# females with JobGrade factor added. It is still significant but not as
# much as before. Even if the male advantage had been made
# insignificant, our analysis indicates that most of the discrepancy
# between male and female salary is explained by job grade: In this
# bank, higher grade jobs are occupied mostly by males while 
# lower level are occupied by females.
cat("Hit Enter to see the results of partial F-test\n")
readline()
print(anova(model0, model1, model3, model5))

# A good way to visualize dependence of various attributes is to pairwise
# plot them against each other. The "pairs" function takes a model, and the
# data frame containing the data, and creates a matrix of plots relating
# variable i to variable j.
# 
# Ideally using pairs should be done *at the beginning*, and even before a
# model is built to see which items might be useful to include in the model
# and which ones are not.

cat("We now look at the \"pairs\" function to plot each variable or factor
against other variables/factors.\n")
cat("Hit Enter to see the matrix of plots\n")
readline()
pairs (Salary~Gender + YrsExper + YrsPrior + EducLev + PCJob + JobGrade,
      data=bank)

