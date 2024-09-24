#Install Package
install.packages("dplyr")
library(dplyr)

#Create a Data Frame
stats <- data.frame(Player = c('Himel', 'Mirazul', 'Hasan', 'Rahim', 'Mirazul', 'Karim'),
Runs = c(100, 200, 408, 320, 200, 320),
Wickets = c(17, 20, NA, 6, 20, 17))

filter(stats, Runs==100)

#Remove Duplicate Rows
distinct(stats)

#Delete Duplicate Rows Using Specific Column
distinct(stats, Runs, .keep_all = TRUE)

#Order Data Based on Runs
arrange(stats, desc(Runs))

#Select data using column
select(stats, Player, Wickets)

#Renaming the column
rename(stats, Runs_Scored = Runs)

#Add a new column Avg
mutate(stats, Avg=Runs/4)

#Add a new column Avg and Drop All Other Variables
transmute(stats, Avg=Runs/4)

#Summarize Method
summarize(stats, sum(Runs), mean(Runs))

#Descriptive Statistics
vars <- c("mpg", "hp", "wt")
head(mtcars[vars])

mtcars

#To Get Central Tendency
summary(mtcars[vars])

#Annotating datasets / Labeling Instances
mydata <- read.csv("D:/1 AIUB/8th Semester (Summer 2023-24)/INTRODUCTION TO DATA SCIENCE [A] - (TOHEDUL ISLAM)/MIDTERM - LAB/Lab Task 3 (23 June 2024)/iris.csv", header = TRUE, sep =",")
mydata$Species <- factor(mydata$Species, levels = c("Iris-setosa", "Iris-versicolor", "Iris-virginica"), labels = c(1,2,3))

mydata

#To check data type
str(mydata)

#Find Standard Deviation
sd(mydata$SepalLengthCm)

#NA Showing 
mydata

#Missing Value Detection
is.na(mydata)

colSums(is.na(mydata))

which(is.na(mydata$SepalLengthCm))

#Delete Instance With Missing Value
na.omit(mydata)
