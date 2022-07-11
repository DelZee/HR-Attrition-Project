install.packages("plyr")
install.packages("caTools")
install.packages("class")
install.packages("gains")
library(gains)
require(plyr)
library(caTools)
library(class)
install.packages("reshape2")
library(reshape2)
install.packages("caret")
library(caret)
setwd("C:/Users/delis/Downloads")
getwd()
data<-read.csv("ThesisData.csv", header = TRUE, sep = ",")


data = data [,-1]
data = data [,-8]
data = data [,-8]
data = data [,-12]
data = data [,-18]
data = data [,-19]
data = data [,-21]
data = data [,-22]
data = data [,-25]
data = data [,-25]
data = data [,-25]
head (data)
str (data)



#factorising variables
data$Attrition <- factor(data$Attrition,
                         levels = c("No",
                                    "Yes"),
                         labels = c(0,1))


data$BusinessTravel<- factor(data$BusinessTravel,
                             levels = c("Non-Travel",
                                        "Travel_Frequently",
                                        "Travel_Rarely"),
                             labels = c(0,1,2))

data$Department <- factor (data$Department,
                           levels = c("Human Resources",
                                      "Research & Development",
                                      "Sales"),
                           labels = c(0,1,2))

data$EducationField <- factor (data$EducationField,
                               levels = c("Human Resources",
                                          "Life Sciences",
                                          "Marketing",
                                          "Medical",
                                          "Other",
                                          "Technical Degree"),
                               labels = c(0,1,2,3,4,5))

data$Gender <- factor(data$Gender,
                      levels = c("Female",
                                 "Male"),
                      labels = c(0,1))


data$JobRole <- factor (data$JobRole,
                        levels = c("Healthcare Representative",
                                   "Human Resources",
                                   "Laboratory Technician",
                                   "Manager",
                                   "Manufacturing Director",
                                   "Research Director",
                                   "Research Scientist",
                                   "Sales Executive",
                                   "Sales Representative"),
                        labels = c(0,1,2,3,4,5,6,7,8))

data$MaritalStatus <- factor (data$MaritalStatus,
                              levels = c("Divorced",
                                         "Married",
                                         "Single"),
                              labels = c(0,1,2))


data$OverTime <- factor(data$OverTime,
                        levels = c("No",
                                   "Yes"),
                        labels = c(0,1))


#Converting Data Type as Numeric
data$Attrition <- as.numeric(as.character(data$Attrition))
data$BusinessTravel <- as.numeric(as.character(data$BusinessTravel))
data$Department <- as.numeric(as.character(data$Department))
data$EducationField <- as.numeric(as.character(data$EducationField))
data$Gender <- as.numeric(as.character(data$Gender))
data$JobRole <- as.numeric(as.character(data$JobRole))
data$MaritalStatus <- as.numeric(as.character(data$MaritalStatus))
data$OverTime <- as.numeric(as.character(data$OverTime))


#partition the data set

set.seed(1)
train.index <- sample(c(1:dim(data)[1]), dim(data)[1]*0.7)
train.df <- data[train.index, ]
test.df <- data[-train.index, ]



#Addressing Class imbalance
table(train.df$Attrition)

# balanced data set with both over and under sampling
balancedtrain <- ovun.sample(Attrition~., data=train.df,
                             N=1724,
                             seed=1, method="over")$data

table(balancedtrain$Attrition)

# run logistic model, and show coefficients for Training Data
lm.fittrain <- glm(as.factor(Attrition) ~ ., data = balancedtrain, family = "binomial")
summary(lm.fittrain)
data.frame(summary(lm.fittrain)$coefficients, odds = exp(coef(lm.fittrain))) 
round(data.frame(summary(lm.fittrain)$coefficients, odds = exp(coef(lm.fittrain))), 5)

predtrain <- predict(lm.fittrain, balancedtrain)
gaintrain <- gains(balancedtrain$Attrition, predtrain, groups=100)

plot(c(0,gaintrain$cume.pct.of.total*sum(balancedtrain$Attrition))~    
       c(0,gaintrain$cume.obs)
       ,xlab="# casestrain", ylab="Cumulativetrain", main="", type="l")
lines(c(0,sum(balancedtrain$Attrition))~
        c(0, dim(train.df)[1]), lty=2)

#Confusion Matrix for Logistic Regression Training data 
confusionMatrix(table(ifelse(predtrain > 0.5, 1, 0), as.factor(balancedtrain$Attrition)))

#Confusion Matrix for Logistic Regression test data 
predtest <- predict(lm.fittrain, balancedtest)
confusionMatrix(table(ifelse(predtest > 0.5, 1, 0), as.factor(balancedtest$Attrition)))
                                                                                                                                              










