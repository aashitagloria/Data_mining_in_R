# 1. Import the data
Data <- read.table(file="~/Desktop/Dropbox/Stat/Didattica/Dati/Music-exemple_EN.txt",header=T,row.names=1,sep="\t")
View(Data)
summary(Data)

# 2. Compute the relative frequencies table and add the marignals for the rows and the columns

Data <- as.table(as.matrix(Data))
View(Data)
summary(Data)
prop.table(Data) # prop.table takes as an argument a "table" object, each cell contains a relative frequency (nij/n) 
round(addmargins(prop.table(Data)),2)


# 3. Compute the chi-square index and state if a significant relation exist between the two variables

summary(Data)
# p-value is small enough to support an association between the two variables.

# 4. Compute the row-profile matrix and interpret 2 different values in this table

Row.F <- prop.table(Data,margin=1) # prop.table(Data,1) means that you will divide each cell of the contingency table by the total of its row  
addmargins(round(Row.F,3))

	#4.b. what is the class of people that is the most represented among people who listen to Jazz?
	# Men


# 5. Compute the column-profile matrix and interpret 2 different values in this table

Col.F <- prop.table(Data,margin=2) # prop.table(Data,2) means that you will divide each cell of the contigengy table by the total of its col  
addmargins(round(Col.F,3))

	#5.b  what is the most favorite kind of music for men?
	# Clasical


# 6. run a BCA with the BA function in FactoMineR

results <- CA(Data)
summary(results)


# 7. How much of the total inertia is explained by the first two dimensions? how you interpret it in terms of association between the two variables?

results$eig
# 100% of the association between the music and the type of people is explained by the 2 dimensions


# 8 what is the type of the music that contributed the most to the first dimension?

results$row$contrib
# is the classical


# 9. Interpret the symmetric map: why the two variables are associated? 

# The two variables are mainly associated because Seniors listen to Classical music in a higher proportion than the other type of people (on average 34% of people listen to Classical music, while this proportion is 53% among the Seniors).
# Also among the people who listen to PopMusic  43% are Women and 17% are Young, these percentage are higher than the % of Women and Young in the sample (respectively 38% and 13%).
