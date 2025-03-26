#########################################################################################
#                           Principal component analysis (PCA)                          #
#########################################################################################
#
# There are two main functions for running PCA in R: 
# - the princomp function in the base package Stat
# - PCA in the FactoMineR package

#-------- PCA with the FactoMineR package      
# FactoMineR is a package specifically designed for multivariate data analysis
# more info or help on functions in FactoMineR: http://factominer.free.fr/classical-methods/
#
install.packages("FactoMineR")
install.packages("factoextra")
library(FactoMineR)
library(factoextra) # for extra analysis
#
data(decathlon)
?decathlon 
# This dataset contains the performances of 41 athletes during two sporting events.
# The variables in this data set are:
# - decathlon[,1:10]: the performances of the athletes in the 10 disciplines 
# - decathlon[,11]: the ranking of the athletes (Rank)
# - decathlon[,12]: the overall score the athletes received at the end of the competition (Points); 
# - decathlon[,13]: the circuit of the race: whether it was olympic or world cup (Competition).
# Var 1:12 are quantitaive variables, var 13 is a nominal variable
 summary(decathlon)
#
#
# Before doing PCA, Let us check correlations.
round(cor(decathlon[,1:12]),digits=2) # digits define the number of decimals to report

# Let's plot des correlations
install.packages("corrplot")  # for visualizing correlation matrix
library(corrplot)             # for help on the corrplot package: https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
corrplot(cor(decathlon[,1:12]), method = 'number')
corrplot(cor(decathlon[,1:12]), method = 'number',type="upper")  # plot only the upper triangular part of the cor matrix
corrplot(cor(decathlon[,1:12]), method = "circle")
corrplot(cor(decathlon[,1:12]), method = "ellipse",type="lower") # plot only the lower triangular part of the cor matrix
corrplot.mixed(cor(decathlon[,1:12]), upper = "ellipse")  # corrplot.mixed() allow for defining different visual method for the lower and upper triangular separately.

# run a PCA on the first 10 quantitative variables
# var 11 and 12 are excluded because they depends on the first 10
# var 13 is excluded because is a nominal variable
res.pca = PCA(decathlon[,1:10], scale.unit=TRUE, ncp=10, graph=T)    #scale.unit: to choose whether to scale the data or not 
                                                                    #ncp: number of dimensions kept in the result
                                                                    #graph: to choose whether to plot the graphs or not
summary(res.pca)
#
# The first part of the results refer to eigenvalues, and hence to the explained inertia. 
# The reported quantities are: 
# - Variance: the eigenvalue associated to each axes (that, as we saw, coincides with the variance of the points along that axis); 
# - % of var.: percentage of explained inertia;
# - Cumulative % of var.: cumulative percentage of inertia explained by the first Q axes.
#
# The second chunk of output is for individuals (top 10 by default, to see all the units use the command specified below).
# For each axes (except for Dist that is a global value) summary print: 
# - Dist is the distance of each individial from the center of the map (that is the square root of the sum of the squared coordinates); 
# - Dim is the coordinate of the individual on each axis; 
# - ctr is the absolute contributions of the individual to each axis; 
# - cos2 is the relative contribution to each axis. 
#
# The third chunk is for variables. The following values are reported for each axes:
# - Dim is the coordinate of the variable on each axis; 
# - ctr is the absolute contributions of the variable to each axis; 
# - cos2 is the relative contribution to each axis. 
#
# more detailed results can be obtained by:
names(res.pca)
res.pca$eig # eigenvalue associated to each axe and corresponding explained variability
res.pca$ind  # outputs for the observation (coordinate, cos2, contribution to the axe)
round(res.pca$ind$cos2,2)    # cos2 = the quality of the individuals on the factor map
res.pca$ind$coord# coordinate of the individual on each axis
res.pca$var   # outputs for the variables (coordinate, cos2, contribution to the axe)

round(res.pca$var$cos2,2)

# How many component?
# Visualize eigenvalues/variances
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))


# save the principal components scores (i.e.coordinate of the individual on each axis) in a data.frame
decathlon.PCA <- as.data.frame(res.pca$ind$coord)
head(decathlon.PCA)
#
#The dimdesc() function calculates the correlation coefficient between a variable and a dimension and performs a significance test. 
dimdesc(res.pca, axes=1:2, proba=0.05) # only on the first to axes. This option can be change, e.g. axes=1:5
round(dimdesc(res.pca, axes=1:2, proba=0.05)$Dim.1$quanti,2)
round(dimdesc(res.pca, axes=1:2, proba=0.05)$Dim.2$quanti,2)


# Plots 
#
plot(res.pca,choix="ind",cex=.75) # Individuals factor map 
plot(res.pca,choix="var",cex=.75) # Variable factor map, i.e. Correlation Circle – Loading Plot
# Var plot interpretation: The first axes opposes athletes of strenght (that are good at Discus and Shot.put, on the right upper part) to the one that are good in running  (strenght have a low correlation with running)
#
# We can filter the points we want to put on the map, based on absolute contribution and cos2, using select=""
# 
# filtring the variables according to their quality of the rappresentation (cos2 values)
plot(res.pca,choix="var",select="cos2 .7") # plot only the var with a sum of the cos2 on the first 2 factors > 0.7
apply(res.pca$var$cos2[,1:2],1,sum) # this is just to show you the sum of the cos2 on the first 2 factors for each var
plot(res.pca,choix="var",select="cos2 .5") # plot only the var with a sum of the cos2 on the first 2 factors > 0.5
#
# filtring the observations according to their contribution
plot(res.pca,select="contrib 10")
plot(res.pca,select="cos2 .5")
#
# --- Advanced plots with factoextra
#
# plot the var by controlling for their contributions
fviz_pca_var(res.pca, col.var="contrib",   # Control variable colors using their contributions
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),  
             repel = TRUE # Avoid text overlapping
             )
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)   # top only print the 10 variables with the highest contribution
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
#
# Control automatically the color of individuals using the cos2
fviz_pca_ind(res.pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
             )
#
# Biplot of individuals and variables
fviz_pca_biplot(res.pca, repel = TRUE)
#



###### Supplementary variables
#
# Three variables have not been considered before: Rank, Points and Competition. Two quantitative and a categorical (Competition).
# These variables can be projected as supplementary: these variables does not play a role in the analysis (i.e. they do not contribute to the axes)
# To add supplementary variables, we use the options quanti.sup and quali.sup.
# 
# adding supplementary quantitative variables
res.pca = PCA(decathlon[,1:12], scale.unit=TRUE, ncp=5, quanti.sup=c(11: 12), graph=T) #quanti.sup: vector of the indexes of the quantitative supplementary variables


# adding supplementary quantitative AND qualitative variables
res.pca = PCA(decathlon, scale.unit=TRUE, ncp=5, quanti.sup=c(11: 12), quali.sup=13, graph=T) #quali.sup: vector of the indexes of the qualitative supplementary variables
#The categories' centres of gravity of this new variable appear on the graph of the individuals.
#They are located at the barycentre of the individuals who took them and they represent an average individual. 
plot(res.pca,choix="ind",cex=.75)
#We can also colour the individuals according to the categories' centres of gravity
plot.PCA(res.pca, axes=c(1, 2), choix="ind", ,cex=.7,habillage=13)   #look at the help for details
                                                                     #axes: the axes to plot
                                                                     #choix: the graph to plot ("ind" for the individuals, "var" for the variables)
                                                                     #habillage: to choose the colours of the individuals: no colour ("none"), a colour for each individual ("ind") or to colour the individuals according to a categorical variable (give the number of the qualitative variable)

# colors the individual with respect to var 13
fviz_pca_ind(res.pca,
             label = "none", # hide individual labels
             habillage = decathlon[,13], # color by groups --> here you can also use the groups obtained by a clustering alhorithm
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
             )

###### Supplementary observations
# to add supplementary observations use the ind= argument in the PCA function
# 
# suppose you want to exclude athlete Casarsa from the analysis
which(rownames(decathlon)=="Casarsa")
res.pca = PCA(decathlon[-41,1:10], scale.unit=TRUE, ncp=5, graph=T)    # -41 is for excluding Casarsa
res.pca = PCA(decathlon[,1:10], scale.unit=TRUE, ncp=5, graph=T,ind=41) # now Casarsa is included as supplementry (i.e. he does not partecipate to the analysis)



#-------- PCA with the function princomp         
decathlon.pca = princomp(decathlon[,1:10],, cor = TRUE)
summary(decathlon.pca)  
plot(decathlon.pca)     # Scree-test plot
biplot(decathlon.pca, cex=0.7)
decathlon.pca$scores    # principal components scores (i.e.coordinate of the individual on each axis) same as res.pca$ind$coord
decathlon.pca$loadings  # loadings = eigenvectors
#apply(pca.auto$loadings, 2, function(x) sum(x^2))  # the norm of each eigenvector =1, i.e. the sum of the square of the elements =1 
cor(decathlon[,1:10],decathlon.pca$scores[,1]) #corr pc1 - variables




