install.packages("dplyr")
install.packages("tibble")
install.packages("ggplot2")

library(ggplot2)
library(dplyr)
library(tibble)

mydata <- read.csv("D:/social-media.csv", header = TRUE, sep =",")
sum(is.na(mydata))

plot_distribution_categorical <- function(column, title, xlab, color)
{
  ggplot(mydata, aes(x = as.factor(column))) +
    geom_bar(fill = color) +
    labs(title = title, x = xlab, y = "Total  People") +
    theme_minimal()
}

plot_distribution_numeric <- function(column, title, xlab, color) 
{
  ggplot(mydata, aes(x = column)) +
    geom_histogram(binwidth = 1, fill = color, color = "black") +
    labs(title = title, x = xlab, y = "Total People") +
    theme_minimal()
}

print(plot_distribution_numeric(mydata$UsageDuraiton, "Distribution of Usage Duration", "Hours", "blue"))
print(plot_distribution_numeric(mydata$Age, "Distribution of Age", "Age", "red"))
print(plot_distribution_categorical(mydata$Country, "Distribution of County", "Country", "black"))
print(plot_distribution_numeric(mydata$TotalLikes, "Distribution of TotalLikes", "Total Likes", "green"))

plot(mydata, main = "Social Media Usage")

pearson_correlation <- function(x, y) 
{
  p_cor <- cor(x, y, method = "pearson")
  
  if (p_cor >= 0.7) 
  {
    p_cor_strength <- "Strong positive correlation"
  } 
  else if (p_cor >= 0.3) 
  {
    p_cor_strength <- "Moderate positive correlation"
  } 
  else if (p_cor > 0) 
  {
    p_cor_strength <- "Weak positive correlation"
  } 
  else if (p_cor <= -0.7) 
  {
    p_cor_strength <- "Strong negative correlation"
  } 
  else if (p_cor <= -0.3)
  {
    p_cor_strength <- "Moderate negative correlation"
  } 
  else if (p_cor < 0)
  {
    p_cor_strength <- "Weak negative correlation"
  } 
  else 
  {
    p_cor_strength <- "No correlation"
  }
  
  return(list(correlation = p_cor, strength = p_cor_strength))
}

print(pearson_correlation(mydata$UserId, mydata$UsageDuraiton))

ggplot(mydata, aes(x = UserId, y = UsageDuraiton)) +
  geom_point() +
  ggtitle("User ID vs Usage Duration") +
  xlab("User ID") +
  ylab("Usage Duration (hours)")

print(pearson_correlation(mydata$UserId, mydata$Age))

ggplot(mydata, aes(x = UserId, y = Age)) +
  geom_point() +
  ggtitle("User ID vs Age") +
  xlab("User ID") +
  ylab("Age")

print(pearson_correlation(mydata$UserId, mydata$TotalLikes))

ggplot(mydata, aes(x = UserId, y = TotalLikes)) +
  geom_point() +
  ggtitle("User ID vs Total Likes") +
  xlab("User ID") +
  ylab("Total Likes")

print(pearson_correlation(mydata$UsageDuraiton, mydata$Age))

ggplot(mydata, aes(x = UsageDuraiton, y = Age)) +
  geom_point() +
  ggtitle("Usage Duration vs Age") +
  xlab("Usage Duration (hours)") +
  ylab("Age")

print(pearson_correlation(mydata$UsageDuraiton, mydata$TotalLikes))

ggplot(mydata, aes(x = UsageDuraiton, y = TotalLikes)) +
  geom_point() +
  ggtitle("Usage Duration vs Total Likes") +
  xlab("Usage Duration (hours)") +
  ylab("Total Likes")

print(pearson_correlation(mydata$TotalLikes, mydata$Age))

ggplot(mydata, aes(x = TotalLikes, y = Age)) +
  geom_point() +
  ggtitle("Total Likes vs Age") +
  xlab("Total Likes") +
  ylab("Age")

summary(aov(UsageDuraiton ~ Country, data = mydata))
summary(aov(TotalLikes ~ Country, data = mydata))
summary(aov(Age ~ Country, data = mydata))
