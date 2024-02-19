library(readxl)
hw1_data <- read_excel("D:/Multivariate analysis/hw1-data.xlsx")
View(hw1_data)
data<-data.frame(hw1_data)
library(ggplot2)
library(GGally)

#or
library(data.table)
data <- data.frame(
  GPA = c(3.75,4.0,3.5,3.75,3.6,3.2,2.8,3.75,4.0,3.9,3.4,3.0,3.0,3.9,3.8,3.3,3.4,3.75,3.9,3.5,3.75,3.9,4.0,3.2,3.75,3.8,3.6,3.1,3.3,2.9),
  English_Level = c(92, 100, 89, 98, 89, 94, 84, 102, 110, 105, 93, 90, 85, 99, 83, 90, 91, 100, 106, 95, 98, 94, 105, 85, 99, 99, 90, 92, 93, 86),
  Social_Integration = c(5, 2, 1, 3, 2, 3, 10, 7, 5, 4, 4, 9, 1, 1, 3, 2, 2, 5, 4, 3, 4, 3, 3, 9, 3, 4, 4, 1, 1, 1),
  Study_Hours_Per_Week = c(15, 20, 1, 12, 6, 6, 5, 18, 12, 14, 10, 8, 4, 19, 10, 5, 6, 10, 15, 13, 14, 16, 18, 6, 10, 12, 16, 4, 3, 2),
  Mental_Health = c(4, 3, 3, 4, 2, 3, 2, 4, 4, 5, 3, 4, 1, 2, 2, 4, 3, 3, 4, 2, 5, 4, 5, 1, 3, 4, 2, 1, 2, 1),
  Social_Media_Hours_Per_Week = c(20, 10, 18, 16, 13, 20, 15, 16, 12, 22, 15, 24, 5, 4, 15, 25, 23, 15, 18, 15, 13, 16, 12, 24, 18, 12, 14, 28, 13, 22),
  Gaming_Hours_Per_Week = c(10, 6, 5, 10, 8, 3, 15, 8, 2, 5, 14, 10, 12, 7, 5, 16, 18, 8, 5, 2, 13, 5, 4, 12, 6, 8, 12, 14, 15, 20),
  Sport_Hours_Per_Week = c(2, 10, 40, 14, 7, 7, 14, 5, 3, 12, 10, 0, 8, 0, 2, 8, 10, 9, 8, 0, 2, 3, 5, 0, 5, 7, 7, 1, 2, 0)
)

#Univariate visualization
#How is the GPA distributed in all the samples?
ggplot(data, aes(x=GPA)) + geom_bar(fill="steelblue")
#The numerical distribution of GPA ranged from approximately 2.8 to 4.0, with GPA values having a maximum distribution between 3.7 and 3.8

#How is mental scores of gaming hours distributed in all the samples?
ggplot(data, aes(x=Gaming_Hours_Per_Week)) + geom_bar(fill="steelblue")
#In the sample, the most students played in 5 and 8 hours per week, with 5 and 4 students, respectively.
#Only one student plays the most games for 20 hours a week. Only 2 students play only 2 hours a week.

#Bivariate visualization
#Is there a relationship between GPA and the level of English?
ggplot(data, aes(x=English_Level, y=GPA)) + geom_point() + geom_smooth(method=lm)

#GPA values are concentrated between 3.00 and 4.00, while TOEFL scores are relatively more dispersed.
#Students with higher English level tend to have higher GPA,especially at the higher range of TOEFL scores.

#Is there a relationship between GPA and the Study hours per week?
library(ggplot2)
ggplot(data, aes(x=Study_Hours_Per_Week,y=GPA)) + 
  geom_point() +
  labs(title="The Relationship Between GPA and Study Hours", x="Mental_Health", y="GPA")
#Almost all the students who studied for more than 10 hours got good grades of more than 3.5.
#Students whose study time is less than 10 hours will almost all have a GPA below 3.3 or even lower. It shows that learning time does have an significant correlation with GPA.

#Is there a relationship between GPA and the sport time?
library(ggplot2)
ggplot(data, aes(x=Sport_Hours_Per_Week,y=GPA)) + 
  geom_point() +
  labs(title="The Relationship Between GPA and the sport time", x="Sport_Hours_Per_Week", y="GPA")
#There is no significant correlation between GPA and the sport time.

#Multivariate visualization

#scatterplot matrix
labs.data1 <- c("English_Level","Social_Integration","Study_Hours_Per_Week","Mental_Health","Social_Media_Hours_Per_Week","Gaming_Hours_Per_Week","Sport_Hours_Per_Week")
scatterplotMatrix(~English_Level+Social_Integration+Study_Hours_Per_Week+Mental_Health+Social_Media_Hours_Per_Week+Gaming_Hours_Per_Week+Sport_Hours_Per_Week | 
GPA, data=data, var.labels=labs.data1,cex.labels=0.7, data1="boxplot",smooth=FALSE,reg.line=FALSE,
pch=c(1,16),col=rep("steelblue",2), legend.plot=FALSE)

#What is the relationship between GPA and those independent variables?
model <- lm(GPA ~ English_Level + Social_Integration + Study_Hours_Per_Week + Mental_Health + Social_Media_Hours_Per_Week + Gaming_Hours_Per_Week + Sport_Hours_Per_Week, data = data)
summary(model)


