library(data.table)
data <- structure(list(
  GPA = c(3.75, 4.0, 3.5, 3.75, 3.6, 3.2, 2.8, 3.75, 4.0, 3.9, 3.4, 3.0, 3.0, 3.9, 3.8, 3.3, 3.4, 3.75, 3.9, 3.5, 3.75, 3.9, 4.0, 3.2, 3.75, 3.8, 3.6, 3.1, 3.3, 2.9),
  English_Level = c(92, 100, 89, 98, 89, 94, 84, 102, 110, 105, 93, 90, 85, 99, 83, 90, 91, 100, 106, 95, 98, 94, 105, 85, 99, 99, 90, 92, 93, 86),
  Social_Integration = c(5, 2, 1, 3, 2, 3, 10, 7, 5, 4, 4, 9, 1, 1, 3, 2, 2, 5, 4, 3, 4, 3, 3, 9, 3, 4, 4, 1, 1, 1),
  Study_Hours_Per_Week = c(15, 20, 1, 12, 6, 6, 5, 18, 12, 14, 10, 8, 4, 19, 10, 5, 6, 10, 15, 13, 14, 16, 18, 6, 10, 12, 16, 4, 3, 2),
  Mental_Health = c(4, 3, 3, 4, 2, 3, 2, 4, 4, 5, 3, 4, 1, 2, 2, 4, 3, 3, 4, 2, 5, 4, 5, 1, 3, 4, 2, 1, 2, 1),
  Social_Media_Hours_Per_Week = c(20, 10, 18, 16, 13, 20, 15, 16, 12, 22, 15, 24, 5, 4, 15, 25, 23, 15, 18, 15, 13, 16, 12, 24, 18, 12, 14, 28, 13, 22),
  Gaming_Hours_Per_Week = c(10, 6, 5, 10, 8, 3, 15, 8, 2, 5, 14, 10, 12, 7, 5, 16, 18, 8, 5, 2, 13, 5, 4, 12, 6, 8, 12, 14, 15, 20),
  Sport_Hours_Per_Week = c(2, 10, 40, 14, 7, 7, 14, 5, 3, 12, 10, 0, 8, 0, 2, 8, 10, 9, 8, 0, 2, 3, 5, 0, 5, 7, 7, 1, 2, 0)
), class = "data.frame",row.names = c(NA, -30L))

#calculate the Euclidean distances between data points based on standardized variables
x1 <- dist(scale(data[, c("GPA", "English_Level", "Social_Integration","Study_Hours_Per_Week","Mental_Health","Social_Media_Hours_Per_Week","Mental_Health",
                            "Social_Media_Hours_Per_Week","Gaming_Hours_Per_Week","Sport_Hours_Per_Week")],center = FALSE))
distance_matrix<-as.matrix(round(as.matrix(x1), 2))
#The distance between data point 8 and data point 1 is 0.71, which is the smallest non-zero distance shown in the matrix. 
#This indicates that data points 8 and 1 are very close to each other in the multidimensional space.
#The distance between data point 3 and data point 24 is 4.50, which is the largest distance shown in the matrix. 
#This indicates that data points 3 and 24 are far apart in the multidimensional space.

#heapmap of distance matrix
heatmap(distance_matrix)

#mean of the variables
cm <- colMeans(data)
cm
#Mean of GPA is 3.55; mean of English_Level is 94.53; mean of Social_Integration is 3.63; mean of Study_Hours_Per_Week is 10.33; mean of Mental_Health is 3.00;
#Mean of Social_Media_Hours_Per_Week  is 16.43; mean of Gaming_Hours_Per_Week is 9.26; mean of Sport_Hours_Per_Week is 6.70.

#variance and covariance of variables
S <- cov(data)   
S
#Variance of GPA is 0.13; variance of English_Level is 49.71; variance of Social_Integration is 5.90; variance of Study_Hours_Per_Week is 29.82;
#Variance of Mental_Health is 1.52; variance of Social_Media_Hours_Per_Week is 30.94; variance of Gaming_Hours_Per_Week is 23.51; variance of Sport_Hours_Per_Week is 57.53.
#The values of Sport_Hours_Per_Week is more dispersed.

#Positive Covariance: For instance, the covariance between GPA and English_Level is 1.85, indicating that these two variables tend to change in the same direction. 
#Generally, students with higher GPAs also have higher English levels.
#Negative Covariance: For example, the covariance between GPA and Gaming_Hours_Per_Week is -1.1189, suggesting that these two variables tend to change in opposite directions.
#Typically, students with higher GPAs spend less time playing games each week.
#The covariance between GPA and Study_Hours_Per_Week is 1.53,which indicates a positive relationship between GPA, and study time.

#calculate Mahalanobis distance
d <- apply(data, MARGIN = 1, function(data)t(data - cm) %*% solve(S) %*% (data - cm))
d

#Most of the observations have Mahalanobis distances ranging from 2 to 15. However, the distance of 21.21 for the third observation is significantly higher than the others, possibly indicating that it is far from the dataset's center (mean) in the multidimensional space. 
#Compared to most data points, it may exhibit greater deviation.

#Q-Q plot for variables(e.g.GPA,English_Level,Study_Hours_Per_Week)
qqnorm(data[,"GPA"], main = "GPA")
qqline(data[,"GPA"])
#The Q-Q plot indicates that the distribution of GPA is generally close to a normal distribution, especially in the central region of the data. 
#However, there is a slight deviation which suggests that the distribution may slightly depart from normality in the higher scoring areas. 

qqnorm(data[,"English_Level"], main = "English_Level")
qqline(data[,"English_Level"])
#The Q-Q plot indicates that the distribution of English_Level is generally close to a normal distribution, especially in the central region of the data. 

qqnorm(data[,"Study_Hours_Per_Week"], main = "Study_Hours_Per_Week")
qqline(data[,"Study_Hours_Per_Week"])
#This Q-Q plot indicates that the data distribution of Study_Hours_Per_Week is generally close to a normal distribution, especially in the middle region of the data. 
#There is a slight skewness in the high-value area of the data. This may be because there are indeed some students in the sample whose study time far exceeds that of other students.

# Order the data by GPA and split into two groups
data <- data[order(data$GPA, decreasing = TRUE), ]
top <- data[1:15, "Study_Hours_Per_Week"]
bottom <- data[16:30, "Study_Hours_Per_Week"]
#t-tests
t_test_result <- t.test(top, bottom, var.equal=TRUE)
print(t_test_result)    
#There are 28 degrees of freedom.
#The test statistic is 5.911, indicating a significant difference between the two groups' means relative to the standard error.
#With a p-value of approximately 2.326e-06, the result is highly significant, suggesting a strong evidence against the null hypothesis. This indicates a significant difference in mean weekly study hours between the two groups.
#The mean weekly study hours for the top 15 students is 14.333333 hours, compared to 6.333333 hours for the bottom 15 students, showing that students with higher GPAs tend to study more hours per week.It suggests a positive correlation between study time and academic performance.

#Leverne test
#Categorize GPA into 'High', 'Medium', 'Low' groups
library(car)
data$GPA_group <- cut(data$GPA,
                      breaks = quantile(data$GPA, probs = c(0, 1/3, 2/3, 1)),
                      labels = c("low", "median", "high"),
                      include.lowest = TRUE)
leveneTest(Sport_Hours_Per_Week ~ GPA_group, data = data)
#p=0.5421>0.05,cannot reject H0.
#The results of the Levene's test indicate that there is no significant difference in the variances of Sport_Hours_Per_Week among the different GPA groups. This implies that we can assume the variances are homogeneous.

#ANOVA
summary(aov(Mental_Health ~ GPA_group, data = data))
#F(2, 27) = 4.083, p = 0.0282
#The ANOVA results indicate a significant difference in Mental_Health scores among different GPA groups.
