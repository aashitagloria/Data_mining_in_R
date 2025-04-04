#########################################################################################
#                      Multiple Correspondence Analysis (MCA)                          #
#########################################################################################
#
# There are two main functions for running MCA in R: 
# - the ca function in the base package ca
# - CA in the FactoMineR package
#
install.packages("FactoMineR",dep=T)
install.packages("ca",dep=T)
install.packages("factoextra",dep=T)
library(FactoMineR)
library(ca)
library(factoextra) # for extra plot and results when using FactorMineR
library(readxl) # for reading excel file

##########################################################################################
# Data handling and simple statistics                                                    #
##########################################################################################
Data <- read_excel("~/Library/CloudStorage/Dropbox/Stat/Didattica/Dati/Races-canines.xlsx")
head(Data)
row_names=Data$Race
Data <- as.data.frame(Data[,-1],stringsAsFactors=T)
row.names(Data) <- row_names
dim(Data)
for (k in 1:ncol(Data)){
  Data[,k] <- as.factor(Data[,k])
}
rm(k)
summary(Data)

##########################################################################################
#-------- MCA with the FactoMineR package      
# Complete Disjunctive Coding
Z <- tab.disjonctif(Data)
dim(Z)
# Burt’s table
B <- t(Z)%*%Z
dim(B)
?MCA
afcm<-MCA(Data, ncp = 10, quali.sup=7,graph=T) # MCA with the variable "function" as a supplementary variable
summary(afcm)
print(afcm)
# 
# Eigenvalues and % of explained inertia using the factoextra package
get_eigenvalue(afcm)  # to extract the proportion of inertia retained by the different MCA dimensions (axes)
fviz_screeplot(afcm, addlabels = TRUE, ylim = c(0, 40)) # to visualize the percentages of inertia explained by each MCA dimensions
#
#
#-------- Results for the variables
afcm$var   
# or 
get_mca_var(afcm)  # if using the factoextra package
get_mca_var(afcm)$coord # coordinates of categories/variables on the MCA dimensions
get_mca_var(afcm)$cos2  # quality of the representation for categories/variables on the factor map.
get_mca_var(afcm)$contrib # contributions (in percentage) of the categories/variables to the definition of the dimensions.
#
fviz_cos2(afcm, choice = "var", axes = 1:2) # to create a bar plot of categories cos2 on the first 2 dimensions
#
# to create a bar plot of categories contribution on each dimension
fviz_contrib(afcm, choice = "var", axes = 1, top = 15) # top only show the 15 categories with the highest contribution
                                                          # axes=1 means that we are focusing on Dim 1
# The variable categories with the larger value, contribute the most to the definition of the dimensions. 
# Variable categories that contribute the most to Dim.1 and Dim.2 are the most important in explaining the inertia in the data set.  
#
# To visualize the associations between variables and MCA principal dimensions, type this:
fviz_mca_var(afcm, choice = "mca.cor", 
            repel = TRUE, # Avoid text overlapping (slow)
            ggtheme = theme_minimal())
# The plot above helps to identify variables that are the most associated with each dimension. 
# The squared correlations between variables and the dimensions are used as coordinates.
# For this example Affection, Taille, Poids are the most associated to 1st dimension, while Veolcité, Poids are the most associated to 2nd dimension
#
fviz_mca_var(afcm, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())
# To visualize only variable categories with cos2 >= 0.4
fviz_mca_var(afcm, select.var = list(cos2 = 0.4))
# Top 10 active variables with the highest cos2
fviz_mca_var(afcm, select.var= list(cos2 = 10))
 # To visulazie variable categories selected by names
name <- list(name = c("TA-", "TA+", "TA++",
                      "PO-", "PO+", "PO++"))
fviz_mca_var(afcm, select.var = name)
#
# Map of Categories withouth the factoextra package functions
plot(afcm,habillage="quali",invisible="ind")
#
#
#-------- Results for the observations
# The function get_mca_ind() [in factoextra] is used to extract the results for observations.
# This function returns a list containing the coordinates, the cos2 and the contributions of observations
get_mca_ind(afcm)
get_mca_ind(afcm)$coord # the coordinates of the observations on the factors (i.e. the syntethic variables)
quant.Data <- as.data.frame(get_mca_ind(afcm)$coord) # to store the factors in a new data.frame to be use in successive analysis
#
# Map of Units colored by Cos2
fviz_mca_ind(afcm, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())
# Color individuals by groups defined according to one variable (this can also be also an external variables)
fviz_mca_ind(afcm, 
             label = "none", # hide individual labels
             habillage = "Poids", # color by groups 
             addEllipses = TRUE,  #addEllipses = TRUE if you want to add a concentration ellipse around each group  
             ellipse.type = "confidence", # If you want a confidence ellipse around the mean point of categories use ellipse.type = "confidence"
             ggtheme = theme_minimal()) 
# Map of Units withouth the factoextra package function
plot(afcm,habillage="quali",invisible="var")
#
#
#-------- Simultaneous Representation of units and categories (Pseudo-Barycentric)
plot(afcm, choix="ind",habillage="quali")
#  or (if using the factoextra package)
fviz_mca_biplot(afcm, 
               repel = TRUE, # Avoid text overlapping (slow if many point)
               ggtheme = theme_minimal())
#
#
#-------- Export results 
# into a TXT file
write.infile(afcm, "mca.txt", sep = "\t")
# into a CSV file
write.infile(afcm, "mca.csv", sep = ";")
#
##########################################################################################
#-------- MCA with the ca package 
?mjca
results <- mjca(Data, lambda="indicator",supcol=7)  # MCA with the variable "function" as a supplementary variable
results$Burt # Burt’s table
# lambda="adjusted"  for improved percentages of inertia
plot.mjca(results, mass = c(T, T), contrib = c("absolute", "absolute"))
