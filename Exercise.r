setwd("/Users/Laura/Desktop/Dropbox/Stat/Didattica/AA 2021-2022/3.Master  ALES - Statistical Learning and Data Mining/")

# 1. Import the data
auto<-read.table("Datasets/auto89 (used for PCA in the slides).txt",header=T,row.names=1,na.strings = "",sep="\t",dec=".")

# 2. Compute the correlation matrix among the 6 variables
round(cor(auto),2)

# 3. plot des correlations. Tips: use the corrplot function in the corrplot package
library(corrplot)             # for help on the corrplot package: https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
corrplot.mixed(cor(auto), upper = "ellipse")  

# 4. Scale the data, tips: use the "scale" function
summary(auto)
scaled.auto <- scale(auto)

# 5. Compute the mean and variance of the scaled data, tips: use the apply function
summary(scaled.auto)
apply(scaled.auto,2,var)
# relations between variables do not change when scaling
corrplot.mixed(cor(scaled.auto), upper = "ellipse")  # corrplot.mixed() allow for defining different visual method for the lower and upper triangular separately.

# 6. run a PCA with the PCA function in FactoMineR
install.packages("FactoMineR")
library(FactoMineR)             # for help on FactoMineR: http://factominer.free.fr/classical-methods/
res.pca = PCA(auto, scale.unit=TRUE, ncp=6, graph=T)  # we look for 5 new components (ncp=5)

# 7. How many dimensions would you retain? look at the eigenvalues from the output. (use the help ?PCA, and the examples in the slides.)
summary(res.pca)
library(factoextra) # for screeplot 
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 100))

# 8. Produce a plot to display cars, showing the top 10 in terms of contribution to the first and to the second axis. 
plot(res.pca,choix="ind",select="contrib 10") # the option select allow to filter the plotted point based on absolute contribution or cos2

# 9. Produce a plot to display the relation between the variables 
plot(res.pca,choix="var")

# 10. Plot only the variables with a cos2 > 0.5 on the first two axes (i.e. that are well rappresented on the first 2 axes)
plot(res.pca,choix="var",select="cos2 5")

# 11. What are the correlations between the variables and the first 2 dimensions? tips: use the dimdesc  
dimdesc(res.pca, axes=1:2, proba=0.5) # only on the first two axes. This option can be change, e.g. axes=1:5
round(dimdesc(res.pca, axes=1:2, proba=0.05)$Dim.1$quanti,2)
round(dimdesc(res.pca, axes=1:2, proba=0.05)$Dim.2$quanti,2)

# 12. Run a HCA on the PCA results. Tips: use the HCPC function   
HCPC(res.pca)
