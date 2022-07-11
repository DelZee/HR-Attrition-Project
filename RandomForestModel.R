
install.packages("plyr")
install.packages("caTools")
install.packages("class")
install.packages("moments")
install.packages("ROSE")
library(ROSE)
library(moments)
library(lessR)
require(plyr)
library(caTools)
library(class)
install.packages("reshape2")
library(reshape2)
install.packages("caret")
library(caret)
install.packages("randomForest")
library(randomForest)
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
setwd("C:/Users/delis/Downloads")
getwd()
data<-read.csv("ThesisData.csv", header = TRUE, sep = ",")
is.null(data)




#Creating Correlation Matrix
cmat <- round(cor(data),2)
head(cmat)

# reduce the size of correlation matrix
melted_cmat <- melt(cmat)
head(melted_cmat)


# plotting the correlation heatmap
library(ggplot2)
ggplot(data= melted_cmat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust=0.5))+
  coord_fixed()+ geom_text(aes(label = value), color = "white", size = 2)

##Scatterplot for Age and years in current role
ggplot(data, aes(x=data$Age, y=data$YearsInCurrentRole)) + geom_point()


##Scatterplot for Age and MonthlyRate
ggplot(data, aes(x=Age, y=MonthlyRate)) + geom_point()

##Scatterplot for MonthlyRate and yearsAtCompany
ggplot(data, aes(x=MonthlyRate, y=YearsAtCompany)) + geom_point()

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
View(data)


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
                                N=nrow(train.df), p=0.5, 
                                seed=1, method="both")$data

table(balancedtrain$Attrition)

# balanced data set with both over and under sampling

table(test.df$Attrition)
balancedtest <- ovun.sample(Attrition~., data=test.df,
                             N=nrow(test.df), p=0.5, 
                             seed=1, method="both")$data

table(balancedtest$Attrition)



# balanced data set with over-sampling training set
balanced.over.train <- ovun.sample(Attrition~., data=train.df, 
                                  p=0.5, seed=1, 
                                  method="over")$data

table(balanced.over.train$Attrition)


# balanced data set with over-sampling test set
data.balanced.test <- ovun.sample(Attrition~., data=test.df, 
                                  p=0.5, seed=1, 
                                  method="over")$data

table(data.balanced.test$Attrition)


#  Default classification tree
default.ct <- rpart(as.factor(Attrition) ~ ., data = balancedtrain, method = "class")
default.ct

# plot for default classification tree
prp(default.ct, type = 1, extra = 4, under = TRUE, split.font = 1, varlen = -10)

##generate confusion matrix for basic Classification tree for training data
default.ct.point.pred.train <- predict(default.ct, train.df, type = "class")
confusionMatrix(default.ct.point.pred.train, as.factor(train.df$Attrition))

#  Default classification tree for test data
default.cttest <- rpart(as.factor(Attrition) ~ ., data = test.df, method = "class")
default.cttest

# plot for default classification tree test set
prp(default.cttest, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

##generate confusion matrix for basic Classification tree for test data
default.ct.point.pred.test <- predict(default.cttest, test.df, type = "class")
confusionMatrix(default.ct.point.pred.test, as.factor(test.df$Attrition))


##creating the random forest for training data
rftrain<- randomForest(as.factor(Attrition)~.,data = balancedtrain, 
                       ntree=1500,mtry=4, nodesize=5, importance = TRUE)
rftrain
## variable importance plot
varImpPlot(rftrain, type = 2)
##variable importance
varImp(rftrain)


##generate confusion matrix for random forest for training data
rf.predtrain<- predict(rftrain, balancedtrain)
confusionMatrix(rf.predtrain,as.factor(balancedtrain$Attrition))



##creating the random forest for test data
##rftest<- randomForest(as.factor(Attrition)~.,data = test.df, ntree=500,mtry=4, nodesize=5, importance = TRUE)
##rftest
## variable importance plot
##varImpPlot(rftest, type = 1)
##variable importance
##varImp(rftest)


##generate confusion matrix for random forest for test data
rf.predtest<- predict(rftrain, balancedtest)
confusionMatrix(rf.predtest,as.factor(balancedtest$Attrition))


