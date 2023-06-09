
#  task_2(Lab Exam)
#  Hager Salah Ramadan (G1_Allometry Dataset)
#  Data preprocessing And Analysis


#Setup home director  -->   done
#Get the current working directory
getwd()

# Read CSV FILE  ----------------------------------------------------------

allometry <- read.csv("G1_Allometry.csv")
#To look at the entire dataset after reading, simply type the name of the dataframe.
# This is called printing an object.
allometry

#View your dataset on R environment. ------------------------------------
View(allometry)

# Working with dataframe --------------------------------------------------
#use the str function to view in detail what the object contains.
str(allometry)

#print only the first (or last) few rows of the dataframe
head(allometry)
tail(allometry)

#read rows 10-15 skipping the header line
allomsmall <- read.csv("G1_Allometry.csv", skip=10, nrows=5, header=FALSE)
allomsmall

# print all the tree diameters here, after rounding to one decimal point
round(allometry$diameter,1)

#convert the tree diameter to inches & add it to the dataframe as a new variable
allometry$diameterInch <- allometry$diameter / 2.54
allometry

#A simple summary of the dataframe
summary(allometry)

#number of rows and columns
nrow(allometry)
ncol(allometry)

#add a new variable called volindex, a volume index defined as the square of tree diameter times height

###allometry$volindex <- with(allometry, diameter^2 * height)
allometry$volindex <- allometry$diameter^2 * as.numeric(allometry$height)  #NAs introduced by coercion

#Changing column names
# read names:
names(allometry)
# rename all (make sure vector is same length as number of columns!)
names(allometry) <- c("spec","diam","ht","leafarea","branchm","diamInch","volindex")
allometry

#change some of the names, using simple indexing
# rename Second one to 'Diam'
names(allometry)[2] <- "Diam"
allometry
# rename 1st and 2nd:
##names(allometry)[1:2] <- c("SP","D")
##allometry


# Subsetting dataframe ----------------------------------------------------

#mydataframe[row,column]
# Read data
allometry <- read.csv("G1_Allometry.csv")
# Recall the names of the variables, the number of columns, and number of rows:
names(allometry)
# Extract tree diameters: take the 4th observation of the 2nd variable:
allometry[4,2]

# We can also index the dataframe by its variable name:
allometry[4,"diameter"]

# Extract the first 3 rows of 'height':
allometry[1:3, "height"]

# Extract the first 5 rows, of ALL variables
# Note the use of the comma followed by nothing
# This means 'every column' and is very useful!
allometry[1:5,]

# Extract the fourth column
# Here we use nothing, followed by a comma,
# to indicate 'every row'
allometry[,4]

# Select only 'height' and 'diameter', store in new dataframe:
allomHD <- allometry[,c("height", "diameter")]
allomHD

# Extract diameters larger than 60
allometry$diameter[allometry$diameter > 60]

# Extract all rows of allom where diameter is larger than 60.
allometry[allometry$diameter > 60,]

# find the height of the tree that has the largest diameter
allometry$height[which.max(allometry$diameter)]

# Recalling the previous section, this is identical to:
allometry[which.max(allometry$diameter), "height"]

# Get 10 random observations of 'leafarea'. Here, we make a new vector
# on the fly with sample(), which we use to index the dataframe.
allometry[sample(1:nrow(allometry),10),"leafarea"]

# As we did with vectors, we can also use %in% to select a subset.
# This example selects only two species in the dataframe.
allometry[allometry$species %in% c("PIMO","PIPO"),]

# Extract tree diameters for the PIMO species, as long as diameter > 50
allometry$diameter[allometry$species == "PIMO" & allometry$diameter > 50]


#shuffle the levels around, using factor.
allometry$species <- factor(allometry$species, levels=c("PSME","PIMO","PIPO"))

#Working with factors       
levels(allometry$species)


#count the number of rows in the dataframe for each species
table(allometry$species)


# Re-coding means use different values for a variable. --------------------
# Data Re-coding Columns
allometry$newspecies[allometry$species=="PSME"]='1'
allometry$newspecies[allometry$species=="PIPO"]='2'
allometry$newspecies[allometry$species=="PIMO"]='3'
allometry

# Add a new variable to allometry: 'small' when diameter is less than 10, 'large' otherwise.
allometry$treeSizeClass2 <- factor(ifelse(allometry$diameter < 50, "low", "high"))
# Now, look how many trees fall in each class.
# Note that somewhat confusingly, 'large' is printed before 'small'.
# Once again, this is because the order of the factor levels is alphabetical by default.
table(allometry$treeSizeClass2)
allometry
#add a new factor based on a numeric variable with more than two levels
## The cut function takes a numeric vectors and cuts it into a categorical variable.
# Continuing the example above, let's make 'small','medium' and 'large' tree size classes:
allometry$treeSizeClasses3 <- cut(allometry$diameter, breaks=c(0,25,50,75),
                               labels=c("low","medium","high"))
# And the results,
table(allometry$treeSizeClasses3)


# Filtering data ---------------------------------------------------------
#Filtering data according to specific criteria.
allom2 <- allometry[allometry$species=='PIMO',]
allom3 <- allometry[allometry$species=='PIMO'&allometry$diameter < 50,]

#Data Indexing & Slicing
#Select specific data.
allom4 <- allometry[allometry$diameter < 50,c(1,4)]

# select all data except columns 2,3
allom5 <- allometry[allometry$species=='PIMO',-c(2,5)]

#Remove some columns from dataset.
newallometry1 <- head(allometry[ , - c(1,4) ])


# Data Sorting ------------------------------------------------------------
sortheight1 <- allometry[order(allometry$height) , ]
sortheight2 <- allometry[order(allometry$height,allometry$diameter) , ]


# Working with missing values ---------------------------------------------

#Missing values in dataframes
# Read the data
allometry <- read.csv("G1_Allometry.csv")
# Look at a summary to see if there are missing values:
summary(allometry)

#Deal with NA and empty data.
#what inside c() will be converted to NA
allometry <- read.csv('G1_Allometry.csv', na.strings = c(''))
allometry

#To get first 25 rows, we can see NA and <NA>
head(allometry, 25)
tail(allometry, 10)

#To get all locations of NA
complete.cases(allometry)

#Get all rows contain missing data
allometry[! complete.cases(allometry), ]

missingdata <- allometry[is.na(allometry$Expenses),]
missingdata

#Get all rows contain missing data
allometry[! complete.cases(allometry), ]

#Get the data structure and summary to know the incorrect types for variables.
str(allometry)
summary(allometry)

# Data Re-coding Columns
allometry$newspecies[allometry$species=="PSME"]='1'
allometry$newspecies[allometry$species=="PIPO"]='2'
allometry$newspecies[allometry$species=="PIMO"]='3'

allometry$treeSizeClasses3 <- cut(allometry$diameter, breaks=c(0,25,50,75),
                                 labels=c("low","medium","high"))
allometry
# Cleaning Data -----------------------------------------------------------

# Cleaning Data
allometry$diameter <- gsub("\\.","",allometry$diameter)
allometry

allometry$diameter <- as.numeric(allometry$diameter)
allometry$height <- gsub(",","",allometry$height)
View(allometry)

allometry$height <- gsub("\\.","",allometry$height)
allometry

allometry$height <- as.numeric(allometry$height)
allometry$leafarea <- gsub("\\.","",allometry$leafarea)

allometry$leafarea <- as.numeric(allometry$leafarea)
allometry$branchmass <- gsub("\\.","",allometry$branchmass)

allometry$branchmass <- as.numeric(allometry$branchmass)
allometry$newspecies <- as.numeric(allometry$newspecies)
allometry$computing <- allometry$height + allometry$leafarea
allometry


# Data Manipulation -------------------------------------------------------

is.na(allometry)
any(is.na(allometry$height))
missingheight  <- allometry[is.na(allometry$height), ]
allometryManip <- allometry[ ! is.na(allometry$height), ]
allometryManip

shapiro.test(allometry$height)
#Calculate the median value.
median(allometry[ , 'height'])
#Ignore NA values in Employees
median(allometry[ , 'height'], na.rm = T)
#Calculate median for specific data.
med <- median(allometry[allometry$species=="PIPO",'height'],na.rm = T)
#Finally, make the replacement.
allometry[is.na(allometry$height) & allometry$species=="PIPO",'height'] <- med
allometry


# Data is ready for visualization -----------------------------------------

#Using the tidyverse package.

#install.packages("tidyverse")


library(tidyverse)
library(ggplot2)


# Display the effect of the height on diameter(co_relation) using scatter plot
#numeric vs numeric  ----- co-relation between two numerical variables.
draw1 <- ggplot(allometry,aes(x=height,y=diameter))
draw1 + geom_point() + ggtitle("The co_relation between the height and diameter")

# Display the effect of the height on diameter(co_relation) using scatter plot
#numeric vs factor
draw12 <- ggplot(allometry,aes(x=diameter,y=species))
draw12 + geom_point() + ggtitle("The co_relation between the diameter and species")
#numeric vs factor
draw13 <- ggplot(allometry,aes(x=height,y=species))
draw13 + geom_point() + ggtitle("The co_relation between the height and species")


# Display the effect of height on diameter colored by the groups of newspecies using scatter plot
draw2<-ggplot(allometry , aes(height , diameter))
draw2 + geom_point(aes(color=newspecies)) +stat_smooth(se=FALSE)  


# The distribution of height using histogram,name the figure and rename the x,y
draw3<-ggplot(allometry , aes(height))
draw3 + geom_histogram(binwidth = 8)
draw3 + geom_histogram(fill = "orange")+ ggtitle("height distribution")+labs(x="height" , y="Frequency")


# The distribution of leafarea using histogram,name the figure and rename the x,y
draw32<-ggplot(allometry , aes(leafarea))
draw32 + geom_histogram(binwidth = 8)
draw32 + geom_histogram(fill = "orange")+ ggtitle("leafarea distribution")+labs(x="leafarea", y="Frequency")

# Summarize the newspecies 1,2,3 to species and newspecies groups using Bar chart

draw_bar <- ggplot(allometry, aes(x=newspecies, fill = species))
draw_bar + geom_bar()
draw_bar + geom_bar() + theme_light()
draw_bar + geom_bar() + labs(y="species count",
                             title = "species category rate")
draw_bar + geom_bar() + facet_wrap(~newspecies)



# Summarize the treeSizeClasses3 Low,Medium,high to species and diameter groups using Bar chart

draw_bar2 <- ggplot(allometry, aes(x=treeSizeClasses3, fill = diameter))
draw_bar2 + geom_bar()
draw_bar2 + geom_bar() + theme_light()
draw_bar2 + geom_bar() + labs(y="treeSizeClass count",
                             title = "diameter category rate")
draw_bar2 + geom_bar() + facet_wrap(~treeSizeClasses3)

