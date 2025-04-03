#########################################################################################
#                      Binary (Simple) Correspondence Analysis (BCA)                    #
#########################################################################################
#
# There are two main functions for running BCA in R: 
# - the ca function in the base package ca
# - CA in the FactoMineR package
#
install.packages("FactoMineR",dep=T)
install.packages("ca",dep=T)
library(FactoMineR)
library(ca)

#############################################################################################################################################################
# Data handling and chi2 test of indipendece
#############################################################################################################################################################
Data <- read.table(file="Preferences-ACB.txt",header=T,row.names=1,sep="\t")
# Data <- read.xlsx("/Users/Laura/Desktop/Dropbox/Stat/Didattica/Dati/health.xls")
View(Data) # is a "data.frame" 
Data <- as.table(as.matrix(Data))
View(Data) # is a "table"
addmargins(Data) # take as an imput a "table" object
summary(Data)
results <- chisq.test(Data)
names(results)
?chisq.test
results$observed # for getting the observed counts
results$expected # for getting the expected counts (i.e. the counts under the hypothesis of indipendece)
results$stdres # standardized residuals measure the difference between observed and expected counts, 
# standardized residuals > |1.96| have to be considered significantly different from zero (they follow a normal distribution)
Phi2 <- results$statistic/sum(Data)
J <- ncol(Data) #number of colomn, i.e. number of categories for the variable Y
I <- nrow(Data) #number of colomn, i.e. number of categories for the variable X
(I-1)*(J-1) # degrees of freedom
max_Phi <- (min(I,J)-1) # max of Phi2 will depends on I and J
Phi2/max_Phi  # --> the ratio between Phi2 and its max is the Cramer's V --> Cramer's V is bounded between 0 and 1.
V <- sqrt(results$statistic/(sum(Data)*(min(I,J)-1))) # Cramér's V, it is a measure of the association between the two variables, it is [0,1]
#
# 
# Compute the frequency table
prop.table(Data) # prop.table takes as an argument a "table" object, each cell contains a relative frequency (nij/n) 
F <- as.matrix(prop.table(Data))
round(addmargins(F),2)
#
# Row profiles 
Row.F <- prop.table(Data,margin=1) # prop.table(Data,1) means that you will divide each cell of the contigengy table by the total of its row  
round(addmargins(Row.F),3)
# Column profiles
Col.F <- prop.table(Data,margin=2) # prop.table(Data,2) means that you will divide each cell of the contigengy table by the total of its col  
round(addmargins(Col.F),3)

#############################################################################################################################################################
# These lines are not to be used in real analysis, they help you in understanding what are the weights in a CA and how to get row and column profiles 
addmargins(Data)[1:I,(J+1)] # marginals of the row, i.e. MASS (weights) of the row
addmargins(Data)[(I+1),1:J] # marginals of the columns, i.e. MASS (weights) of the columns
invDI<- diag((addmargins(Data)[1:I,(J+1)]/sum(Data))^(-1)) # diagonal matrix with the inverse of the weights in the space of the columns
invDJ <-diag((addmargins(Data)[(I+1),1:J]/sum(Data))^(-1)) # diagonal matrix with the inverse of the weights in the space of the rows
invDI%*%F # Row profiles 
F%*%invDJ # Column profiles
############################################################################################################################################################


#############################################################################################################################################################
#-------- BCA with the ca package 
results <-ca(Data)
summary(results)
# The items given in Rows and Columns include
# - The principal coordinates for the ﬁrst two dimensions (k = 1 and k = 2). 
# - Squared correlations (cor) 
# - Contributions (ctr) 
# Notice that the quantities in these tables are multiplied by 1000 (e.g., the coordinates and masses) 
# which for the cor and ctr quantities means they are expressed in permills. 
# The total quality (qlt) is given with respect to the dimensionality of the solution, i.e. in this case it is the sum of the squared correlations over the two included dimensions. 
# In the case of supplementary variables, an asterisk is appended to the supplementary variable names in the output. 
#
# symmetric map
plot.ca(results, mass = c(T, T), contrib = c("relative", "relative"), )
# asymmetric map (column standard coords):
plot.ca(results, mass = c(T, T), contrib = c("absolute", "absolute"),map="rowprincipal")
# asymmetric map (rows standard coords)
plot.ca(results, mass = c(T, T), contrib = c("absolute", "absolute"),map="colprincipal")
plot3d.ca(results)
#
#-------- BCA with the FactoMineR package      
results <- CA(Data)
summary(results)
results$col
#############################################################################################################################################################

