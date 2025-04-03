# 1. Import the data (tips use the read.xlsx function in the xlsx package)

install.packages("readxl")
install.packages("FactoMineR")
install.packages("factoextra")

library(readxl)
library(FactoMineR)
library(factoextra) 

data <- read_excel("Music-exemple_EN.xlsx")
data
music_data <- as.data.frame(data)
rownames(music_data) <- music_data$...1
music_data

#Clean data
music_data <- music_data[, -1]
music_data

# 2. Compute the relative frequencies table and add the marignals for the rows and the columns (tips use the prop.table function)

# Relative frequency (proportion) table
rel_freq <- prop.table(as.matrix(music_data))
rel_freq

# Add row and column marginals
row_marginal <- rowSums(rel_freq)
row_marginal

col_marginal <- colSums(rel_freq)
col_marginal

rel_freq_with_margins <- addmargins(rel_freq)
rel_freq_with_margins


# 3. Compute the chi-square index and state if a significant relation exist between the two variables 

chi_result <- chisq.test(music_data)
print(chi_result)

#INTERPREATION: The p-value is significantly less 2.826e-09
#There is a significant association between these variables

# 4. Compute the row-profile matrix and interpret 2 different values in this table

row_profile <- prop.table(as.matrix(music_data), margin = 1)
print(round(row_profile, 3))

#INTERPRETATION:
#PopMusic – Women = 0.429
#Among people who like Pop Music, 42.9% are women.
#Women are strongly represented in the Pop Music category.

#Classical – Young = 0.052
#Among Classical music listeners, only 5.2% are young people.
#So young people are underrepresented in the Classical group.


# 4b. what is the class of people that is the most represented among people who listen to Jazz?

print("The group most represented among Jazz fans is Men (46.5%), showing a male preference for Jazz.")

# 5. Compute the column-profile matrix and interpret 2 different values in this table

col_profile <- prop.table(as.matrix(music_data), margin = 2)
print(round(col_profile, 3))

#Young – PopMusic = 0.539
#53.9% of young people prefer Pop Music.
#Pop is clearly most popular among the youth.

#Seniors – Classical = 0.531
#53.1% of seniors prefer Classical music.
#Classical is strongly preferred by older people.

# 5b. what is the most favorite kind of music for men?

print("Classical music is the most preferred genre among men (38.5%)")

# 6. run a BCA with the BA function in FactoMineR

library(FactoMineR)
ca_result <- CA(music_data, graph = FALSE)
ca_result


# 7. How much of the total inertia is explained by the first two dimensions? how you interpret it in terms of association between the two variables?
inertia <- ca_result$eig
total_inertia <- sum(inertia[, 1])
inertia_1_2 <- sum(inertia[1:2, 1])
percentage_explained <- (inertia_1_2 / total_inertia) * 100
percentage_explained

#INTERPRETATION: The first two dimensions explain 83.4% of the total inertia, 
#indicating a strong association between the two variables.  

# 8 what is the type of the music that contributed the most to the first dimension?
#Pop Music contributed the most to the first dimension (Dim 1) with a contribution of 0.537.

# 9. Interpret the symmetric map: why the two variables are associated? 

fviz_ca_biplot(ca_result, repel = TRUE)

#Interpretation: 
#Dim1 explains 77.6% of the variation
#Dim2 explains 22.4%
#Together, the first two dimensions explain 100% of the total inertia, 
#meaning the map gives a complete picture of the association.

#KEY POINTS OF INTERPRETATION
#1. The CA biplot shows a clear association between types of people and music preferences.
#2. Young people are closely associated with Pop music, indicating a strong preference.
#3. Men are closest to Jazz, suggesting it’s their most favored genre.
#4. Seniors are clearly linked to Classical music, consistent with expectations.

#5. The fact that Dim1 alone explains 77.6% of the total inertia indicates that most of the association is captured by the first axis.
#This confirms a strong dependency between the two variables, driven primarily by age-related musical preferences.
