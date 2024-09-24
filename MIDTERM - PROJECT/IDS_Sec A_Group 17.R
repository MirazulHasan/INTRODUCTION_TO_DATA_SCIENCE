install.packages("dplyr")
install.packages("tibble")
install.packages("ggplot2")
library(dplyr)
library(tibble)
library(ggplot2)

mydata <- read.csv("D:/MPD_Sec-A.csv", header = TRUE, sep =",")

summary(mydata)
str(mydata)
sum(is.na(mydata))



class(mydata$Patient_id)
which(is.na(mydata$Patient_id))
mydata[duplicated(mydata$Patient_id), ]

mydata <- distinct(mydata, Patient_id, .keep_all = TRUE)

names(mydata)[names(mydata) == "Patient_id"] <- "Patient ID"



class(mydata$Age)
which(is.na(mydata$Age))

boxplot(mydata$Age, main = "Boxplot for Age", ylab = "Age")
boxplot.stats(mydata$Age)$out

Q1 <- quantile(mydata$Age, 0.25, na.rm = TRUE)
Q3 <- quantile(mydata$Age, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
mydata <- mydata %>% filter(is.na(Age) | (Age >= lower_bound & Age <= upper_bound))

mean_Age <- mean(mydata$Age, na.rm = TRUE)
mydata$Age[is.na(mydata$Age)] <- round(mean_Age)



class(mydata$Gender)
unique(mydata$Gender)

mydata$Gender <- factor(mydata$Gender, levels = c("male", "female"), labels = c(1,2))

which(is.na(mydata$Gender))

most_frequent_ge <- names(sort(table(mydata$Gender), decreasing = TRUE))[1]
mydata$Gender[is.na(mydata$Gender)] <- most_frequent_ge

mydata$Gender <- factor(mydata$Gender, levels = c(1,2), labels = c("male", "female"))



class(mydata$weight.kg.)
which(is.na(mydata$weight.kg.))

boxplot(mydata$weight.kg., main = "Boxplot for Weight (Kg)", ylab = "Weight")
boxplot.stats(mydata$weight.kg.)$out

Q1 <- quantile(mydata$weight.kg., 0.25, na.rm = TRUE)
Q3 <- quantile(mydata$weight.kg., 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
mydata <- mydata %>% filter(is.na(weight.kg.) | (weight.kg. >= lower_bound & weight.kg. <= upper_bound))

mean_weight.kg. <- mean(mydata$weight.kg., na.rm = TRUE)
mydata$weight.kg.[is.na(mydata$weight.kg.)] <- round(mean_weight.kg.)

names(mydata)[names(mydata) == "weight.kg."] <- "Weight (Kg)"



class(mydata$Delivery_number)
unique(mydata$Delivery_number)

mydata$Delivery_number <- as.integer(mydata$Delivery_number)

which(is.na(mydata$Delivery_number))

most_frequent_dn <- names(sort(table(mydata$Delivery_number), decreasing = TRUE))[1]
mydata$Delivery_number[is.na(mydata$Delivery_number)] <- most_frequent_dn

names(mydata)[names(mydata) == "Delivery_number"] <- "Delivery Number"



class(mydata$Delivery_time)
unique(mydata$Delivery_time)
which(is.na(mydata$Delivery_time))

most_frequent_dt <- names(sort(table(mydata$Delivery_time), decreasing = TRUE))[1]
mydata$Delivery_time[is.na(mydata$Delivery_time)] <- most_frequent_dt

mydata$Delivery_time <- as.character(mydata$Delivery_time)

mydata$Delivery_time <- factor(mydata$Delivery_time, levels = c(0,1,2), labels = c("timely", "premature", "latecomer"))

names(mydata)[names(mydata) == "Delivery_time"] <- "Delivery Time"



class(mydata$Blood)
unique(mydata$Blood)

mydata$Blood <- factor(mydata$Blood, levels = c("low", "normal", "high"), labels = c(0, 1, 2))

which(is.na(mydata$Blood))

most_frequent_bl <- names(sort(table(mydata$Blood), decreasing = TRUE))[1]
mydata$Blood[is.na(mydata$Blood)] <- most_frequent_bl

mydata$Blood <- factor(mydata$Blood, levels = c(0, 1, 2), labels = c("low", "normal", "high"))

names(mydata)[names(mydata) == "Blood"] <- "Blood of Pressure"



class(mydata$Heart)
unique(mydata$Heart)

mydata$Heart <- as.character(mydata$Heart)

mydata$Heart <- factor(mydata$Heart, levels = c(0, 1), labels = c("apt", "inept"))

names(mydata)[names(mydata) == "Heart"] <- "Heart Problem"



class(mydata$Caesarian)
unique(mydata$Caesarian)
which(is.na(mydata$Caesarian))

most_frequent_ca <- names(sort(table(mydata$Caesarian), decreasing = TRUE))[1]
mydata$Caesarian[is.na(mydata$Caesarian)] <- most_frequent_ca

mydata$Caesarian <- as.character(mydata$Caesarian)

mydata$Caesarian <- factor(mydata$Caesarian, levels = c(0, 1), labels = c("No", "Yes"))


summary(mydata)


mydata$Age <- as.numeric(mydata$Age)
mydata$`Weight (Kg)` <- as.numeric(mydata$`Weight (Kg)`)

columns_numeric <- c("Age", "Weight (Kg)")

central_tendency_numeric <- function(column)
  {
  mean_val <- mean(column)
  median_val <- median(column)
  mode_val <- as.numeric(names(sort(table(column), decreasing = TRUE))[1])
  list(mean = mean_val, median = median_val, mode = mode_val)
  }

for (col in columns_numeric)
  {
  print(paste("Central tendency for", col))
  print(central_tendency_numeric(mydata[[col]]))
  }



measure_of_spread <- function(column) 
  {
  range_val <- diff(range(column, na.rm = FALSE))
  variance_val <- var(column, na.rm = FALSE)
  sd_val <- sd(column, na.rm = FALSE)
  list(range = range_val, variance = variance_val, sd = sd_val)
  }

for (col in columns_numeric) {
  print(paste("Measures of spread for", col))
  print(measure_of_spread(mydata[[col]]))
}



plot_distribution_categorical <- function(column, title, xlab, color)
  {
  ggplot(mydata, aes(x = as.factor(column))) +
  geom_bar(fill = color) +
  labs(title = title, x = xlab, y = "Frequency") +
  theme_minimal()
  }

plot_distribution_numeric <- function(column, title, xlab, color) 
  {
  ggplot(mydata, aes(x = column)) +
  geom_histogram(binwidth = 1, fill = color, color = "black") +
  labs(title = title, x = xlab, y = "Frequency") +
  theme_minimal()
  }

print(plot_distribution_numeric(mydata$Age, "Distribution of Age", "Age", "blue"))
print(plot_distribution_categorical(mydata$Gender, "Distribution of Gender", "Gender", "black"))
print(plot_distribution_numeric(mydata$`Weight (Kg)`, "Distribution of Weight (Kg)", "Weight (Kg)", "green"))
print(plot_distribution_categorical(mydata$`Delivery Number`, "Distribution of Delivery Number", "Delivery Number", "purple"))
print(plot_distribution_categorical(mydata$`Delivery Time`, "Distribution of Delivery Time", "Delivery Time", "orange"))
print(plot_distribution_categorical(mydata$`Blood of Pressure`, "Distribution of Blood of Pressure", "Blood of Pressure", "yellow"))
print(plot_distribution_categorical(mydata$`Heart Problem`, "Distribution of Heart Problem", "Heart Problem", "red"))
print(plot_distribution_categorical(mydata$Caesarian, "Distribution of Caesarian", "Caesarian", "cyan"))
