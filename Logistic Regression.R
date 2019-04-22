#install.packages("h2o")
library(h2o)
library(data.table)
library(purrr)
library(ggplot2)
#install.packages("hrbrthemes")
library(hrbrthemes)

library(tidyverse) 
library(MASS)
library(car)
library(e1071)
library(caret)

library(caTools)
library(pROC)
#install.packages("ggcorrplot")
library(ggcorrplot)
#install.packages("cowplot")
library(cowplot)
library(ggplot2)
library(readxl)
teldata <- read.csv("C:/Users/sawan/OneDrive/Desktop/Backup KPS/Rutgers Courses/Sem 2/MA/project/Telco-Customer-Churn_Dataset.csv")
summary(teldata)
colSums(is.na(teldata))
telmissing<-teldata[is.na(teldata$TotalCharges),] 
telmissing
teldata <- na.exclude(teldata)
colSums(is.na(teldata))
head(teldata)
str(teldata)
levels(teldata$Partner) <- c(0,1)
teldata$Dependents <-factor(teldata$Dependents)
levels(teldata$Dependents) <- c(0,1)
teldata$PhoneService <-factor(teldata$PhoneService)
levels(teldata$PhoneService) <- c(0,1)
teldata$PaperlessBilling <-factor(teldata$PaperlessBilling)
levels(teldata$PaperlessBilling) <- c(0,1)
teldata$Churn <-factor(teldata$Churn)
levels(teldata$Churn) <- c(0,1)
#triple factors
teldata$Contract <-factor(teldata$Contract)
levels(teldata$Contract) <- c(0,2,1)
teldata$DeviceProtection <-factor(teldata$DeviceProtection)
levels(teldata$DeviceProtection) <- c(0,2,1)
teldata$TechSupport <-factor(teldata$TechSupport)
levels(teldata$TechSupport) <- c(0,2,1)
teldata$StreamingTV <-factor(teldata$StreamingTV)
levels(teldata$StreamingTV) <- c(0,2,1)
teldata$StreamingMovies <-factor(teldata$StreamingMovies)
levels(teldata$StreamingMovies) <- c(0,2,1)
teldata$MultipleLines <-factor(teldata$MultipleLines)
levels(teldata$MultipleLines) <- c(0,2,1)
teldata$InternetService <-factor(teldata$InternetService)
levels(teldata$InternetService) <- c(0,2,1)
teldata$OnlineSecurity <-factor(teldata$OnlineSecurity)
levels(teldata$OnlineSecurity) <- c(0,2,1)
teldata$OnlineBackup <-factor(teldata$OnlineBackup)
levels(teldata$OnlineBackup) <- c(0,2,1)
teldata$gender <-factor(teldata$gender)
levels(teldata$gender) <- c(0,1)
summary((teldata))
str(teldata)
logistic<- glm(Churn ~., data=teldata, family="binomial")#glm (Generalized Linear Model)
summary(logistic) #details of logistic function
##Residual deviance and AIC is to big for this dataset
## Now calculate the overall "Pseudo R-squared" and its p-value
ll.null <- logistic$null.deviance/-2
ll.null
ll.proposed <- logistic$deviance/-2
ll.proposed
## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null ##R^2
#This can be interpreted as overall effect size
## The p-value for the R^2
1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic$coefficients)-1))
## now we can plot the data
predicted.data <- data.frame(probability.of.Churn=logistic$fitted.values,Churn=teldata$Churn)
predicted.data <- predicted.data[order(predicted.data$probability.of.Churn, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

## Lastly, we can plot the predicted probabilities for each sample having
## Churn and color by whether or not the customer leave the telcom service
ggplot(data=predicted.data, aes(x=rank, y=probability.of.Churn)) +
  geom_point(aes(color=Churn), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted Probability of Customer Churn")
ggsave("TelecomChurn_probabilities.pdf")



