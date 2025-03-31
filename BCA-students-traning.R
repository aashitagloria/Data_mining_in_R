# 1. Import the data (tips use the read.xlsx function in the xlsx package)
# Install if not already installed
install.packages("readxl")
install.packages("FactoMineR")
install.packages("factoextra")

# Load the packages
library(readxl)
library(FactoMineR)
library(factoextra) 

# 2. Compute the relative frequencies table and add the marignals for the rows and the columns (tips use the prop.table function)

# Load the data from Excel
data <- read_excel("Music-exemple_EN.xlsx")
data
music_data <- as.data.frame(data)
rownames(music_data) <- music_data$...1

# Remove the column ...1
music_data <- music_data[, -1]

# Final check
print(music_data)
music_data

# 3. Compute the chi-square index and state if a significant relation exist between the two variables

chi_test <- chisq.test(music_data)
chi_test

# 4. Compute the row-profile matrix and interpret 2 different values in this table

row_profiles <- prop.table(as.matrix(music_data), margin = 1)
row_profiles

# 4b. what is the class of people that is the most represented among people who listen to Jazz?

music_data["Jazz", ]  # Shows counts for Jazz


# 5. Compute the column-profile matrix and interpret 2 different values in this table

col_profiles <- prop.table(as.matrix(music_data), margin = 2)
col_profiles


# 5b. what is the most favorite kind of music for men?

music_data[, "Men"]  # Column values


# 6. run a BCA with the BA function in FactoMineR

bca_result <- CA(music_data, graph = TRUE)


# 7. How much of the total inertia is explained by the first two dimensions? how you interpret it in terms of association between the two variables?

eig <- bca_result$eig
eig

# Total inertia explained by Dim 1 + Dim 2
total_inertia <- sum(eig[1:2, 2])
paste("Total inertia explained by first 2 dimensions:", round(total_inertia, 2), "%")


# 8 what is the type of the music that contributed the most to the first dimension?

bca_result$row$contrib[, 1]  # Contribution of each music type to Dim 1

# 9. Interpret the symmetric map: why the two variables are associated? 

fviz_ca_biplot(bca_result, repel = TRUE)

