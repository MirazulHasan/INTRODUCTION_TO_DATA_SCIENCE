install.packages("dplyr")
install.packages("tibble")
install.packages("ggplot2")
install.packages("gridExtra")
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tibble)

mydata <- read.csv("D:/social-media.csv", header = TRUE, sep =",")
sum(is.na(mydata))

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
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("User ID vs Usage Duration") +
  xlab("User ID") +
  ylab("Usage Duration (hours)")

print(pearson_correlation(mydata$UserId, mydata$Age))

ggplot(mydata, aes(x = UserId, y = Age)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("User ID vs Age") +
  xlab("User ID") +
  ylab("Age")

print(pearson_correlation(mydata$UserId, mydata$TotalLikes))

ggplot(mydata, aes(x = UserId, y = TotalLikes)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("User ID vs Total Likes") +
  xlab("User ID") +
  ylab("Total Likes")

print(pearson_correlation(mydata$UsageDuraiton, mydata$Age))

ggplot(mydata, aes(x = UsageDuraiton, y = Age)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Usage Duration vs Age") +
  xlab("Usage Duration (hours)") +
  ylab("Age")

print(pearson_correlation(mydata$UsageDuraiton, mydata$TotalLikes))

ggplot(mydata, aes(x = UsageDuraiton, y = TotalLikes)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Usage Duration vs Total Likes") +
  xlab("Usage Duration (hours)") +
  ylab("Total Likes")

print(pearson_correlation(mydata$TotalLikes, mydata$Age))

ggplot(mydata, aes(x = TotalLikes, y = Age)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Total Likes vs Age") +
  xlab("Total Likes") +
  ylab("Age")



mean_usage_duration <- mean(mydata$UsageDuraiton, na.rm = TRUE)
median_usage_duration <- median(mydata$UsageDuraiton, na.rm = TRUE)
mode_usage_duration <- as.numeric(names(sort(table(mydata$UsageDuraiton), decreasing = TRUE))[1])

ggplot(mydata, aes(x = UsageDuraiton)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5, color = "black") +
  geom_density(color = "blue", size = 1) +
  geom_vline(aes(xintercept = mean_usage_duration), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median_usage_duration), color = "green", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mode_usage_duration), color = "purple", linetype = "dashed", size = 1) +
  ggtitle("Histogram with Density Line and Mean, Median, Mode for Usage Duration") +
  xlab("Hours") +
  ylab("Density") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) +
  annotate("text", x = mean_usage_duration, y = 0.1, label = paste("Mean:", round(mean_usage_duration, 2)), color = "red", angle = 90, vjust = -0.5) +
  annotate("text", x = median_usage_duration, y = 0.1, label = paste("Median:", round(median_usage_duration, 2)), color = "green", angle = 90, vjust = -0.5) +
  annotate("text", x = mode_usage_duration, y = 0.1, label = paste("Mode:", round(mode_usage_duration, 2)), color = "purple", angle = 90, vjust = -0.5)


mean_age <- mean(mydata$Age, na.rm = TRUE)
median_age <- median(mydata$Age, na.rm = TRUE)
mode_age <- as.numeric(names(sort(table(mydata$Age), decreasing = TRUE))[1])

ggplot(mydata, aes(x = Age)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5, color = "black") +
  geom_density(color = "blue", size = 1) +
  geom_vline(aes(xintercept = mean_age), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median_age), color = "green", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mode_age), color = "purple", linetype = "dashed", size = 1) +
  ggtitle("Histogram with Density Line and Mean, Median, Mode for Age") +
  xlab("Hours") +
  ylab("Density") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) +
  annotate("text", x = mean_age, y = 0.1, label = paste("Mean:", round(mean_age, 2)), color = "red", angle = 90, vjust = -0.5) +
  annotate("text", x = median_age, y = 0.1, label = paste("Median:", round(median_age, 2)), color = "green", angle = 90, vjust = -0.5) +
  annotate("text", x = mode_age, y = 0.1, label = paste("Mode:", round(mode_age, 2)), color = "purple", angle = 90, vjust = -0.5)


mean_totalLikes <- mean(mydata$TotalLikes, na.rm = TRUE)
median_totalLikes <- median(mydata$TotalLikes, na.rm = TRUE)
mode_totalLikes <- as.numeric(names(sort(table(mydata$TotalLikes), decreasing = TRUE))[1])

ggplot(mydata, aes(x = TotalLikes)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5, color = "black") +
  geom_density(color = "blue", size = 1) +
  geom_vline(aes(xintercept = mean_totalLikes), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median_totalLikes), color = "green", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mode_totalLikes), color = "purple", linetype = "dashed", size = 1) +
  ggtitle("Histogram with Density Line and Mean, Median, Mode for Total Likes") +
  xlab("Total Likes") +
  ylab("Density") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) +
  annotate("text", x = mean_totalLikes, y = max(density(mydata$TotalLikes)$y) * 0.8, label = paste("Mean:", round(mean_totalLikes, 2)), color = "red", angle = 90, vjust = -0.5) +
  annotate("text", x = median_totalLikes, y = max(density(mydata$TotalLikes)$y) * 0.8, label = paste("Median:", round(median_totalLikes, 2)), color = "green", angle = 90, vjust = -0.5) +
  annotate("text", x = mode_totalLikes, y = max(density(mydata$TotalLikes)$y) * 0.8, label = paste("Mode:", round(mode_totalLikes, 2)), color = "purple", angle = 90, vjust = -0.5)
