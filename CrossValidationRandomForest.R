install.packages("plyr")
install.packages("caTools")
install.packages("class")
install.packages("moments")
install.packages("ggplot2")
install.packages("scales")
library(scales)
library(ggplot2)
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
class(data$Attrition)
require(ggplot2)
require(lattice)
install.packages("tidyverse")
install.packages("caret")
install.packages("datarium")
install.packages("ROSE")
library(ROSE)
library(tidyverse)
library(caret)
library(datarium)
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

table(train.df$Attrition)
table(test.df$Attrition)

# balanced data set with  over sampling training
balancedtrain <- ovun.sample(Attrition~., data=train.df,
                             N=1724, 
                             seed=1, method="over")$data
table(balancedtrain$Attrition)

##validating model with k-fold method for Random Forest (Note for RF,
##As the model considers numeric variable as Regression instead of classification
## Do not change to numeric)

# Define the control for the CV model
set.seed(125)
train_control <- trainControl(method = "cv", number = 5, search = "grid")

##Running the  default model without hyper parameters
modeltrain <- train(Attrition~ ., data = balancedtrain,
                    method= "rf",
                    metric = "Accuracy",
                    trControl = train_control)
print(modeltrain)

##OOB estimate of the model
modeltrain$finalModel


##Improving Accuracy.Let's try to get a higher score.
##Searching for best mtry
set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1:23))
rf_mtry <- train(Attrition~.,
                 data = balancedtrain,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = train_control,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 500)
print(rf_mtry)



##best value of mtry
best_mtry <-rf_mtry$bestTune$mtry
best_mtry
max(rf_mtry$results$Accuracy)


##Finding the best node size
set.seed(1234)
store_nodesize <- list()
tuneGrid <- expand.grid(.mtry = best_mtry )
for (nodesize in c(5,10,20,50,100)) {
  set.seed(1234)
  rf_nodesize <- train(Attrition~.,
                   data = balancedtrain,
                   method = "rf",
                   metric = "Accuracy",
                   tuneGrid = tuneGrid,
                   trControl = train_control,
                   importance = TRUE,
                   nodesize = nodesize,
                   ntree = 500)
  current_iteration <- toString(nodesize)
  store_nodesize[[current_iteration]] <- rf_nodesize
}


results_nodesize <- resamples(store_nodesize)
summary(results_nodesize)



##Searching for best Max nodes(Can also try with higher max nodes c(20:30))
##Best max node is 30
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(6: 30)) {
  set.seed(1234)
  rf_maxnode <- train(Attrition~.,
                      data = balancedtrain,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = train_control,
                      importance = TRUE,
                      nodesize = 5,
                      maxnodes = maxnodes,
                      ntree = 500)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)


##searching for best ntrees.
store_maxtrees <- list()
for (ntree in c(50, 100, 150, 250, 300, 350, 400, 450, 500, 550, 600, 800, 1000)) {
  set.seed(5678)
  rf_maxtrees <- train(Attrition~.,
                       data = balancedtrain,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = train_control,
                       importance = TRUE,
                       nodesize = 5,
                       maxnodes = 30,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)
dotplot(results_tree)


##You have your final model. You can train the random forest with the following parameters:
##ntree =350: 350 trees will be trained
#node size is 5
##mtry=3 : 3 features is chosen for each iteration
##maxnodes = 30: Maximum 30 nodes in the terminal nodes (leaves)

fit_rf <- train(Attrition~.,
                balancedtrain,
                method = "rf",
                metric = "Accuracy",
                tuneGrid = tuneGrid,
                trControl = train_control,
                importance = TRUE,
                nodesize = 5,
                ntree = 350,
                maxnodes = 30)
fit_rf
fit_rf$finalModel

##Evaluating training model

prediction <- predict(fit_rf, balancedtrain)
confusionMatrix(prediction, balancedtrain$Attrition)

##Evaluating the Final Testing Model(The original test set). No balancing was applied. 

predictiontest <- predict(fit_rf, test.df)
confusionMatrix(predictiontest, test.df$Attrition)



##Visualizing the plot
varImp(fit_rf)
plot(varImp(fit_rf, scale = F), main = "Variable Importance : Random Forest 5 fold CV")




##Variable importance plot alternate way
x <- varImp(fit_rf, scale = TRUE)
rownames(x$importance)
importance <- data.frame(rownames(x$importance), x$importance$`1`)
names(importance) <- c('Platform', 'Importance')
##Order the data from greatest importance to least important
importance <- transform(importance, Platform = reorder(Platform, Importance))
# Plot the data with ggplot.
ggplot(data=importance, aes(x=Platform, y=Importance)) +
  geom_bar(stat = 'identity',colour = "blue", fill = "blue") + coord_flip()


