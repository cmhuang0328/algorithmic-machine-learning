# In this example we use the discrimination data we used earlier
# in the course, but instead of linear regression we use tree
# based regression. Here is the exaplantion of the data:
# 
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
# titles to columns, turn EducLev and JobGrade into facotrs, and adds
# Age and YrsInBank:

# Read the data into data frame called bank: 
bank <- read.csv("bank.csv")

# Chnage EducLev and JobGrade into factors:
bank$EducLev<-as.factor(bank$EducLev)
bank$JobGrade<-as.factor(bank$JobGrade)

# Add age and YrsInBank to the data. Since the data was gathered in
# 1995, we subtract YrBorn and YrHired from 95 to get Age and
# YrsInBankk:
bank<-cbind(bank,Age=95-bank$YrBorn,YrsInBankk=95-bank$YrHired)

# Print top and bottom of the data
readline("\nThe beginning and end of the bank employee data\n");
print(head(bank))
print(tail(bank))

# Make sure to install the tree library (only the first time use), and
# load the library:
library(tree)
readline("Starting the default regression tree. Hit Enter:\n")

# the tree model models everything But the "Employee" column is not a
# real variable, only a number so we remove it from the model. 
# Salary ~ . - Employee - YrHired - YrBorn
# means include everything as a variable except Employee, YrHired and
# YrBorn:
treeBank<-tree(Salary~.-Employee-YrHired-YrBorn, data=bank)

# The tree package extends the plot function so that it literally draws
# the output decision tree produced by the model. This tree usually has
# no labels. The text command prints the appropriate label. If the
# labels are long it attempts to "pretty-print" the labels. If you want
# the actual labels set pretty=0.
plot(treeBank, main="The default Model"); text(treeBank, pretty=0)
readline("\nSummary of the default model. Hit Enter:\n")

# The summary function gives a d summary of the tree model, including
# variables that were used in the solution. Tree usually uses tree
# pruning to make sure it does not overfit the data.
print(summary(treeBank))
readline("\nComplete  output of the default model. Hit Enter:\n")

# Simply printing the tree model will actually draws the tree in text
# format (check). It also gives useful information for each node
# (how many data points in the region associated to the node, the
# deviance (RSS for regression), and the value of the response variable
# in the region.
print(treeBank)

# If we want to make the tree smaller or larger than the default one
# produced by the 'tree' function, we can set some parameters:
# minsize is the minimum number of points in each terminal node(leaves
# associated with unsplit regions),
# mindev=is the minimum ration of the deviance (here RSS) of the leaf
# and the root node, if the RSS(node) is larger than mindev*RSS(root)
# then the nodes is split. 
# Experiment with various values of d1 for min dev, and s1 for minsize.
# Setting mindev=0 and minsize=2, will make the tree to split all the
# way to the point where each leaf has only one data point in it. This
# results in a very large tree and overfitting.
d1<-0; s1<-2;
readline(paste0("\nStarting the more detailed model:deviance=",d1,
                  ",minsize= ",s1," Hit Enter:\n"))
treeBank1<-tree(Salary~.-Employee-YrHired-YrBorn,mindev=d1,minsize=s1,data=bank)
dev.new();plot(treeBank1, main="The default Model"); text(treeBank, pretty=0)
readline("\nSummary of the default model. Hit Enter:\n")
print(summary(treeBank1))
readline("\nComplete  output of the default model. Hit Enter:\n")
print(treeBank1)
