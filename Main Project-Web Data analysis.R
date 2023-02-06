# Web Data Analysis 
# A web analytics data of datadb website is given for analysis
# Their database has keywords (contd):
# that states the keywords of time in the page, source group, bounces, exits, unique page views, and visits.

#Problem 1: Get a basic understanding of the dataset and to prepare for further analysis.
#Problem 2: The team needs to know whether the unique page view value depends on visits.
#Problem 3: Find out the probable factors from the dataset, which could affect the exits 
#Problem 4: Find the variables which possibly have an effect on the time on page.
#Problem 5: Help the team in determining the factors that are impacting the bounce. 

#Importing the web analytics dataset 

library(readxl)
mydata <- read_excel("Web Data Analysis.xlsx")
View(mydata)
print(mydata)
head(mydata)
str(mydata)
names(mydata)

#Problem 1: Get a basic understanding of the dataset and to prepare for further analysis.

summary(mydata)

#Problem 2: The team needs to know whether the unique page view value depends on visits.(correlation)

cor(mydata$Uniquepageviews,mydata$Visits, method="pearson")


#Problem 3: Find out the probable factors from the dataset, which could affect the exits (logistics regression)


a1<-aov(Exits~.,data = mydata)
summary(a1)

#Problem 4: Find the variables which possibly have an effect on the time on page.(one way anova)

a2<-aov(Timeinpage~.,data=mydata)
summary(a2)


#Problem 5: Help the team in determining the factors that are impacting the bounce. (Logistics regression)
#As bounce is a categorical variable here, we go on with logistics regression to find out (contd)
#the factors that impact the bounce rate


#Applying logistics regression 
# We convert the bounce rate into a categorical variable

mydata$Bounces<-as.factor(mydata$Bounces)
log_reg<-glm(Bounces~Timeinpage+Exits+Visits+Continent+Sourcegroup, data=mydata, family = "binomial")
summary(log_reg)

















