# resources http://www.sthda.com/english/wiki/factoextra-r-package-easy-multivariate-data-analyses-and-elegant-visualization
##############################################################################
#                           R Lines for Clustering                           #
##############################################################################
#
# load the required packages 
# Installing the packages
install.packages("dplyr")
install.packages("cluster")
install.packages("dendextend")
install.packages("psych")

# library(tidyverse)  
library(dplyr)      # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms and funny visualization
library(dendextend) # for comparing two dendrograms
library(psych)      # for group analysis
#
#

data(USArrests)
?USArrests
head(USArrests)
summary(USArrests)
apply(USArrests,2,var)
apply(USArrests,2,sd)/apply(USArrests,2,mean) # coef of var

# To perform a cluster analysis in R, generally, the data should be prepared as follows:
# 
# - Rows are observations (individuals) and columns are variables
# - Any missing value in the data must be removed or estimated.
# - The data must be standardized (i.e., scaled) to make variables comparable. Recall that, standardization consists of transforming the variables such that they have mean zero and standard deviation one
#
#
# To remove any missing value that might be present in the data, type this:

complete.USArrests <- na.omit(USArrests)


# Scaling/standardizing the data using the R function scale

complete.USArrests <- scale(complete.USArrests)
apply(complete.USArrests,2,var)
head(complete.USArrests)

# There are different functions available in R for computing hierarchical clustering. The commonly used functions are:
#
# -   hclust [in stats package] and agnes [in cluster package] for agglomerative hierarchical clustering (HC)
# -   diana [in cluster package] for divisive HC
#
#
# We can perform agglomerative HC with hclust. 
# First we compute the dissimilarity values with dist and then feed these values into hclust and specify the agglomeration method to be used (i.e. “complete”, “average”, “single”, “ward.D”). We can then plot the dendrogram.

# Dissimilarity matrix
d <- dist(complete.USArrests, method = "euclidean") 
# note: he dist function computes the distance matrix based on the euclidean distance, but there are other distance measures available such as "maximum", "manhattan" etc...
# 
# Alternative: within R it is simple to compute and visualize the distance matrix using the functions get_dist and fviz_dist from the factoextra R package. 
# - get_dist: for computing a distance matrix between the rows of a data matrix. The default distance computed is the Euclidean; however, get_dist also supports distanced described in equations 2-5 above plus others.
# - fviz_dist: for visualizing a distance matrix
d <- get_dist(complete.USArrests)
fviz_dist(d, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
# This starts to illustrate which states have large dissimilarities (red) versus those that appear to be fairly similar (teal).


##############################################################################
#                       R Lines for Hierarchical Clustering                  #
##############################################################################
# There are several hierarchical clustering methods you can choose. The default method is "complete". 
# Type ?hclust for additional details about each method and other arguments.

# Hierarchical clustering using Complete Linkage
hc <- hclust(d, method = "complete" )   
# Plot the obtained dendrogram
plot(hc, cex = 0.6, hang = -1)      # hang: the fraction of the plot height by which labels should hang below the rest of the plot. A negative value will cause the labels to hang down from 0.
# cex : number indicating the amount by which plotting text and symbols should be scaled relative to the default. 1=default, 1.5 is 50% larger, 0.5 is 50% smaller, etc.
plot(as.dendrogram(hc), main = "my results")
# note: Transform the hierarchical clustering output to dendrogram class with as.dendrogram. This will create a nicer visualization. This produce a visualization similar to the one obtained by adding the hang=-1 we used before
plot(as.dendrogram(hc), main = "my results",horiz =  TRUE) # the horiz option change the orientation of the plot

#Adding rectangles around hierarchical clusters 
rect.hclust(hc, k=4, border="red")     # for the 4 class solution
rect.hclust(hc, k=6, border=1:6)       # for the 6 class solution


# Determining Optimal Clusters
#
# The basic idea behind cluster partitioning methods is to define clusters 
# such that the total intra-cluster variation (known as total within-cluster variation or total within-cluster sum of square, wss) is minimized
#
# Thus, we can use the following strategie (i.e. the elbow method) to define the optimal clusters: 
# 
# 1. Compute clustering algorithm for different values of k. For instance, by varying k from 1 to 10 clusters
# 2. For each k, calculate the total within-cluster sum of square (wss)
# 3. Plot the curve of wss according to the number of clusters k.
# 4. The location of a bend (knee) in the plot is generally considered as an indicator of the appropriate number of clusters.
#
# To perform the elbow method we just need to change the second argument in fviz_nbclust to FUN = hcut.
fviz_nbclust(complete.USArrests, FUN = hcut, method = "wss")
# 
# Another approach: the Average Silhouette Method
#
# The average silhouette approach measures the quality of a clustering. 
# It determines how well each object lies within its cluster. A high average silhouette width indicates a good clustering. 
# The average silhouette method computes the average silhouette of observations for different values of k. 
# The optimal number of clusters k is the one that maximizes the average silhouette over a range of possible values for k 
fviz_nbclust(complete.USArrests, kmeans, method = "silhouette")
#
#
# In order to identify groups (i.e. clusters), we can cut the dendrogram with cutree:
#
# Cut tree into 4 groups
clusters <- cutree(hc, k = 4)  # clusters contains individuals class membership
# Number of members in each cluster
table(clusters)

# add the class membership as an extra variable to the original dataset
USArrests.and.cluster <- dplyr::mutate(USArrests,clusters)
rownames(USArrests.and.cluster)<- rownames(USArrests)
head(USArrests.and.cluster)
psych::describeBy(USArrests.and.cluster, group="clusters")  # describe the data by group

# with the tidyverse package
# USArrests %>%
#  mutate(cluster = clusters) %>%
# USArrests %>%
#  mutate(Cluster = clusters) %>%
#  group_by(clusters) %>%
#  summarise_all("mean")


# advanced: the fviz_cluster function from the factoextra package to visualize the result in a scatter plot.
# If there you are using more than two variables for the HCA fviz_cluster will perform principal component analysis (PCA) and plot the data points according to the first two principal components that explain the majority of the variance.
fviz_cluster(list(data = complete.USArrests, cluster = clusters))

# run a Hierarchical clustering on the same data but using a different linkage
#
# Hierarchical clustering using WARD
hc1 <- hclust(d, method = "ward.D2")
# Plot the obtained dendrogram
plot(hc1)


# To further explore the similarity and difference between the alternative clustering algorithms, we can also compare two dendrograms. 
# Here we compare hierarchical clustering with complete linkage versus Ward’s method. 
# The function tanglegram plots two dendrograms, side by side, with their labels connected by lines.
# The output displays “unique” nodes, with a combination of labels/items not present in the other tree, highlighted with dashed lines. 
tanglegram(as.dendrogram(hc), as.dendrogram(hc1))
#
# The quality of the alignment of the two trees can be measured using the function entanglement. 
# Entanglement is a measure between 1 (full entanglement) and 0 (no entanglement). 
# A lower entanglement coefficient corresponds to a good alignment
# Entanglement take as an imput a dendlist object
dend_list <- dendlist("Complete linkage" = as.dendrogram(hc), "Ward linkage"= as.dendrogram(hc1)) # the function dendlist accepts several dendrograms and or dendlist objects and chain them all together. 
entanglement(dend_list)
# 
# a nicer plot
tanglegram(as.dendrogram(hc), as.dendrogram(hc1),
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

#################### Alternative functions for clustering
#
# Alternatively, we can use the agnes and diana functions. 
# These functions behave very similarly; however, with the agnes/diana functions you can also get the agglomerative coefficient, 
# which measures the amount of clustering structure found (values closer to 1 suggest strong clustering structure).

# Compute with agnes
hc2 <- agnes(complete.USArrests, method = "complete")
pltree(hc2, cex = 0.6, hang = -1, main = "Dendrogram of agnes with complete linkage") 
hc2$ac   # Agglomerative coefficient

hc3 <- agnes(complete.USArrests, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes with ward linkage") 
hc3$ac   # Agglomerative coefficient

# Cut agnes() tree into 4 groups
cutree(as.hclust(hc3), k = 4) 


# Divisive Hierarchical Clustering with diana
#
# The R function diana provided by the cluster package allows us to perform divisive hierarchical clustering. 
# diana works similar to agnes; however, there is no method to provide.

# compute divisive hierarchical clustering
hc4 <- diana(complete.USArrests)
hc4$dc   # Divise coefficient; amount of clustering structure found

# plot dendrogram
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")
# Cut diana() tree into 4 groups
cutree(as.hclust(hc4), k = 4)



##############################################################################
#                            R Lines for K means                             #
##############################################################################
# K-means clustering is the most commonly used unsupervised machine learning algorithm for partitioning a given data set 
# into a set of k groups (i.e. k clusters),
#
# The basic idea behind k-means clustering consists of defining clusters so that the total intra-cluster variation (known as total within-cluster variation) is minimized. 
# Each observation (xi) is assigned to a given cluster such that the sum of squares (SS) distance of the observation to their assigned cluster centers (μk) is minimized.
#
# K-means algorithm can be summarized as follows:
# 
# 1. Specify the number of clusters (K) to be created (by the analyst)
# 2. Select randomly k objects from the data set as the initial cluster centers or means
# 3. Assigns each observation to their closest centroid, based on the Euclidean distance between the object and the centroid
# 4. For each of the k clusters update the cluster centroid by calculating the new mean values of all the data points in the cluster. 
#    The centroid of a Kth cluster is a vector of length p containing the means of all variables for the observations in the kth cluster; p is the number of variables.
# 5. Iteratively minimize the total within sum of square. That is, iterate steps 3 and 4 until the cluster assignments stop changing or the maximum number of iterations is reached. 
#    By default, the R software uses 10 as the default value for the maximum number of iterations.

# Compute k-means clustering with k = 4
set.seed(123)
# We can compute k-means in R with the kmeans function.
k4.results <- kmeans(USArrests, 4, nstart = 25)  # The nstart option attempts multiple initial configurations and reports on the best one. For example, adding nstart = 25 will generate 25 initial configurations. This approach is often recommended.
print(k4.results)
# The output of kmeans is a list with several bits of information. The most important being:
# - cluster: A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
# - centers: A matrix of cluster centers.
# - totss: The total sum of squares.
# - withinss: Vector of within-cluster sum of squares, one component per cluster.
# - tot.withinss: Total within-cluster sum of squares, i.e. sum(withinss).
# - betweenss: The between-cluster sum of squares, i.e. $totss-tot.withinss$.
# size: The number of points in each cluster.
#
# We can also view our results by using fviz_cluster. This provides a nice illustration of the clusters. 
# If there are more than two variables fviz_cluster will perform principal component analysis (PCA) and plot the data points according to the first two principal components that explain the majority of the variance.
fviz_cluster(k4.results, data = USArrests)

# Choosing the number of cluster (K)
#
# Because the number of clusters (K) must be set before we start the algorithm, it is raccomandate to use several different values of K and choose the best partition  
k2.results <- kmeans(USArrests, 2, nstart = 25)  
k3.results <- kmeans(USArrests, 3, nstart = 25)  
k5.results <- kmeans(USArrests, 5, nstart = 25)  

# plots to compare
p1 <- fviz_cluster(k2.results, geom = "point", data = USArrests) + ggtitle("k = 2")
p2 <- fviz_cluster(k3.results, geom = "point",  data = USArrests) + ggtitle("k = 3")
p3 <- fviz_cluster(k4.results, geom = "point",  data = USArrests) + ggtitle("k = 4")
p4 <- fviz_cluster(k5.results, geom = "point",  data = USArrests) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

# and use the fviz_nbclust for choosing the number of groups following the Elbow Method
set.seed(123)
fviz_nbclust(USArrests, kmeans, method = "wss")


