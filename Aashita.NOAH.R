# Final Exam V2 - Data Mining for Business
# 2024-2025
# L. Trinchera

# The Cereals dataset reports 15 characteristics, including 13 numerical variables, on 77
# breakfast cereals. The description of the variables is provided below:
# • name: Name of cereal.
# • manuf: Manufacturer of cereal (A=American Home Food Products; G=General Mills; K=Kelloggs; N=Nabisco; P=Post; Q=Quaker Oats; R=Ralston Purina)
# • type: C=cold or H=hot.
# • calories: calories per serving.
# • protein: grams of protein.
# • fat: grams of fat.
# • sodium: milligrams of sodium.
# • fiber: grams of dietary fiber.
# • carbo: grams of complex carbohydrates.
# • sugars: grams of sugars.
# • potass: milligrams of potassium.
# • vitamins: vitamins and minerals - 0, 25, or 100, indicating the typical percentage of FDA recommended.
# • shelf: display shelf (1, 2, or 3, counting from the floor).
# • weight: weight in ounces of one serving.
# • cups: number of cups in one se

# by using ONLY the quantitative variables in the analysis:

# 1. Import the relevant packagess
install.packages("readxl")
install.packages("FactoMineR")
install.packages("factoextra")
library(FactoMineR)
library(factoextra)
library(xlsx)
library(corrplot)


#Reading the csv file
cereals <- read.csv("cereals.csv")
cereals

#Separating the values in different columns
cereals <- read.csv("cereals.csv", sep = ";")
cereals

# 2. Describe the variables using univariate statistics.  

#Summary of the dataset with the mean, median, min, max, etc.
summary(cereals)

# Remove non-numeric variables: name, manuf, type
quant_data <- cereals[, c(4:15)] #We start from 4 because the 1st 3 columns are non-numeric

#Calculating the mean, median and standard deviation of the variables
means <- apply(quant_data, 2, mean)
medians <- apply(quant_data, 2, median)
sds <- apply(quant_data, 2, sd) * (nrow(quant_data)-1)/nrow(quant_data)

#Creating a dataframe with the results
univ_stats <- rbind(means, medians, sds)
round(univ_stats, 2)

#Interpretation: This shows the central tendency and spread of each variable across 77 cereals

# 3. Compute the correlation matrix among all the variables and plot it.
library(corrplot)
cor_matrix <- cor(quant_data)
round(cor_matrix, 2)

corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8, addCoef.col = "black")

#Interpretation: I observe the following relationships between different columns in the correlation matrix as follows:

#Fiber and Potassium show a very strong positive correlation (0.90), 
#this indicates that cereals rich in fiber also tend to be rich in potassium.

#Calories are positively correlated with sugars (0.56) and weight (0.70), 
#which in my opnion is pretty obvious since that heavier and sweeter cereals contribute more energy.

#On the other hand, fiber and cups per serving are negatively correlated (-0.51), 
#implying that nutrient dense cereals take up less space, while thr ones with more volume tend to be less rich in fiber.


# 4. Run a PCA with the PCA function in FactoMineR on scaled data.

#The PCA Principal Component Analysis on the scaled data is
library(FactoMineR)
pca_res <- PCA(scale(quant_data), graph = FALSE)

#Screenplot
library(factoextra)
eigenvalues <- get_eigenvalue(pca_res)


# 5. How many dimensions would you retain?  Justify your reply.
fviz_screeplot(pca_res, addlabels = TRUE, ylim = c(0, 50))

#Interpretation:
#From the screen plot:
#Dim 1 explains 27.2%,
#Dim 2 explains 22.4%,
#Dim 3 explains 14%.
#Together, the first 3 dimensions explain  approximately 63.6% of the total variance, 
#which is a significant proportion for multivariate analysis.

#We also observe an "elbow" after Dimension 3 — after that, 
#the increase in explained variance is much smaller, indicating diminishing returns.

#Therefore, we would retain the first 3 dimensions for further analysis.
# because they together explain approximately 63.6% of the total variance in the data, 
#which is a substantial amount. 
#Additionally, the scree plot shows a clear elbow after the third dimension, 
#suggesting that the remaining components contribute relatively little to the data's overall structure. 
#Retaining three dimensions strikes a good balance between dimensionality reduction and information preservation.

# 6. Produce a plot to display the relationships between the variables using Dimension 1 and 3 and interpret the results.

fviz_pca_var(pca_res, axes = c(1, 3),
             col.var = "contrib",
             gradient.cols = c("lightblue", "orange", "red"),
             repel = TRUE)

#Interpretation:
#The variable plot on Dimensions 1 and 3 shows meaningful relationships among the nutritional characteristics of cereals.
#Dimension 1 distinguishes cereals high in fiber, potassium, protein, and weight 
#(healthier cereals) from those higher in carbohydrates and lighter weight (cups).
#Dimension 3 contrasts cereals with higher sodium, vitamins, 
#and carbohydrates against those with higher calories, sugars, and fat.
#The plot helps to identify how variables cluster together, 
#highlighting correlations and revealing nutrient-driven patterns in cereal composition.


# 7. Show the individuals who are worstly rappresented on Dimension 1, 2 and 3 and comment the results.

ind <- get_pca_ind(pca_res)

# Lowest quality of representation (worst) on each dimension
worst_dim1 <- sort(ind$cos2[,1])[1:5]
worst_dim2 <- sort(ind$cos2[,2])[1:5]
worst_dim3 <- sort(ind$cos2[,3])[1:5]

list(Dim1 = worst_dim1, Dim2 = worst_dim2, Dim3 = worst_dim3)

#Interpretation:
#These cereals are poorly represented on respective axes since they don’t align with major trends.
#Might be outliers or close to the center.

# 8. Plot the Variables on the first two dimensions of the PCA, and color them by their Contribution. Comment the results.

fviz_pca_var(pca_res, axes = c(1, 2),
             col.var = "contrib",
             gradient.cols = c("lightblue", "orange", "red"),
             repel = TRUE)


#Interpretation:
#The variable plot on Dimensions 1 and 2 shows two main patterns.
#Dimension 1 separates cereals that are rich in fiber, potassium, protein, and heavier per serving, 
#from those that are lighter (more cups) and high in carbohydrates.
#Dimension 2 distinguishes cereals with higher calories, sugars, and weight from those lower in these elements.
#Arrows like fiber and potassium point in the same direction, 
#indicating a strong positive correlation. 
#In contrast, cups and weight are nearly opposite, suggesting a negative relationship. 


# 9. Do the same but use dimensions 1 and 3. Comment the results. 

fviz_pca_var(pca_res, axes = c(1, 3),
             col.var = "contrib",
             gradient.cols = c("lightblue", "orange", "red"),
             repel = TRUE)

#Interpretation:
#The plot of Variables on Dimensions 1 and 3 highlights distinct relationships among cereal nutritional features:
#Dimension 1 separates cereals that are high in fiber, protein, potassium, and weight 
#from those that are lighter and high in carbs and cups per serving.

#Dimension 3 captures a contrast between cereals that are high in complex carbohydrates, sodium, and vitamins versus 
#those high in calories, sugars, and fat.

#Notably, fiber and potassium are strong contributors to Dimension 1 and positively correlated. Carbohydrates stand out on Dimension 3, indicating a separate nutritional profile compared to calorie-dense cereals.
