install.packages("tibble")
library(tibble)

patientID <- c(1, 2, 3, 4, 5)

age <- c(23, 45, 68, 25, 56)

diabetes <- c("Type1", "Type1", "Type2", "Type1", "Type2")

status <- c("Poor", "Good", "Improved", "Poor", "Good")

patientdata <- data.frame(patientID, age, diabetes, status)

patientdata[c("age", "diabetes", "blood_group")]

blood_group <- c("A+", "B+", "O+", "AB-", "A+")

cbind(patientdata, blood_group)

new_row <- data.frame(patientID = 6, age = 30, diabetes = "Type1", status = "Improved")

patientdata <- rbind(patientdata, new_row)

patientdata <- add_column(patientdata,blood_group,.after = 3)

patientdata

patientdata$blood_group <- NULL


# USING LIST

g <- "My First List"
h <- c(25, 26, 18, 39)
j <- matrix(1:10, nrow=5)
k <- c("one", "two", "three")
mylist <- list(title=g, ages=h, j, k)

mylist


# USER INPUT

var1 <- readline(prompt = "Enter Your Name: ")
var2 <- readline(prompt = "Enter Your Age: ")

var2 <- as.integer(var2)

print(var1)
print(var2)

x <- scan()

print(x)


# TEXT EDITOR

mydata <- data.frame(name=character(0), age=numeric(0), gender=character(0), weight=numeric(0))
mydata <- edit(mydata)


# READING CSV FILE

mydata <- read.csv("D:/1 AIUB/8th Semester (Summer 2023-24)/INTRODUCTION TO DATA SCIENCE [A] - (TOHEDUL ISLAM)/MIDTERM - LAB/Lab Task 3 (23 June 2024)/iris.csv", header = TRUE, sep =",")

mydata[5:15,]

mydata$Species

subset(mydata, PetalLengthCm >= 6)

print(mydata)


