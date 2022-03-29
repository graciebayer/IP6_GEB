# Importing the dataset
dataset=read.csv('heart.data.csv')

#Displaying the count of null values per column
colSums(is.na(dataset))

# Missing data
#na. rm = TRUE to exclude missing values
dataset$biking[is.na(dataset$biking)]<- mean(dataset$biking, na.rm=TRUE)
dataset$smoking[is.na(dataset$smoking)]<- mean(dataset$smoking, na.rm=TRUE)
dataset$heart.disease[is.na(dataset$heart.disease)]<- mean(dataset$heart.disease, na.rm=TRUE)
colSums(is.na(dataset))

#Create multiple copies of the dataset with no missing data
dataset1<-dataset
dataset2<-dataset
dataset3<-dataset
dataset4<-dataset

##################################################################
## Multiple Linear Regression

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
split=sample.split(dataset1$heart.disease, SplitRatio=0.8)
training_set=subset(dataset1, split == TRUE)
testing_set=subset(dataset1, split == FALSE)

# Fitting Multiple Linear Regression to the Training set
regressor_MLR <- lm(formula=heart.disease~.,data=training_set)

y_pred=predict(regressor_MLR,newdata=testing_set)

# Predicting the Validation set results
y_pred=predict(regressor_MLR, newdata=testing_set)

#new <- data.frame( )
#predict(regressor, newdata = new)
new <- data.frame(biking=45.09720266, smoking=21.38562013)
predict(regressor_MLR, newdata = new)
new1 <- data.frame(biking=8.279743388, smoking=6.423719525)
predict(regressor_MLR, newdata = new1)
new2 <- data.frame(biking=42.34586343, smoking=20.74132754)
predict(regressor_MLR, newdata = new2)
new3 <- data.frame(biking=30.77425409, smoking=23.61017497)
predict(regressor_MLR, newdata = new3)

library(caret)
#RMSE
#sqrt(mean((dataset$y_test-y_pred)^2))
RMSE(testing_set$heart.disease,y_pred)
R2(testing_set$heart.disease,y_pred)

########################################################
#Support Vector Regressor
# Splitting the dataset into the Training set and Test set
split=sample.split(dataset2$heart.disease, SplitRatio=0.8)
training_set=subset(dataset2, split == TRUE)
testing_set=subset(dataset2, split == FALSE)

# Fitting SVR to the dataset
library(e1071)
regressor_SVR=svm(formula=heart.disease~., data=training_set, type='eps-regression',
                  kernel='radial')

# Predicting the Validation set results
y_pred=predict(regressor_SVR, newdata=testing_set)

#new <- data.frame( )
new <- data.frame(biking=45.09720266, smoking=21.38562013)
predict(regressor_SVR, newdata = new)
new1 <- data.frame(biking=8.279743388, smoking=6.423719525)
predict(regressor_SVR, newdata = new1)
new2 <- data.frame(biking=42.34586343, smoking=20.74132754)
predict(regressor_SVR, newdata = new2)
new3 <- data.frame(biking=30.77425409, smoking=23.61017497)
predict(regressor_SVR, newdata = new3)

#RMSE
#sqrt(mean((dataset$y_test-y_pred)^2))
RMSE(testing_set$heart.disease,y_pred)
R2(testing_set$heart.disease,y_pred)

########################################################
#Decision Tree Regressor
# Splitting the dataset into the Training set and Test set
split=sample.split(dataset3$heart.disease, SplitRatio=0.8)
training_set=subset(dataset3, split == TRUE)
testing_set=subset(dataset3, split == FALSE)

# Fitting to the dataset
library(rpart)
regressor_DT=rpart(formula=heart.disease~., data=training_set)

# Predicting the Validation set results
y_pred=predict(regressor_DT, newdata=testing_set)

#new <- data.frame( )
new <- data.frame(biking=45.09720266, smoking=21.38562013)
predict(regressor_DT, newdata = new)
new1 <- data.frame(biking=8.279743388, smoking=6.423719525)
predict(regressor_DT, newdata = new1)
new2 <- data.frame(biking=42.34586343, smoking=20.74132754)
predict(regressor_DT, newdata = new2)
new3 <- data.frame(biking=30.77425409, smoking=23.61017497)
predict(regressor_DT, newdata = new3)

#RMSE
#sqrt(mean((dataset$y_test-y_pred)^2))
RMSE(testing_set$heart.disease,y_pred)
R2(testing_set$heart.disease,y_pred)

########################################################
#Random Forest Regressor
# Splitting the dataset into the Training set and Test set
split=sample.split(dataset4$heart.disease, SplitRatio=0.8)
training_set=subset(dataset4, split == TRUE)
testing_set=subset(dataset4, split == FALSE)

# Fitting to the dataset
library(randomForest)
set.seed(1234)
regressor_RF=randomForest(x=training_set[,1:2], y=training_set$heart.disease, ntree = 20)

# Predicting the Validation set results
y_pred=predict(regressor_RF, newdata=testing_set)

#new <- data.frame( )
new <- data.frame(biking=45.09720266, smoking=21.38562013)
predict(regressor_RF, newdata = new)
new1 <- data.frame(biking=8.279743388, smoking=6.423719525)
predict(regressor_RF, newdata = new1)
new2 <- data.frame(biking=42.34586343, smoking=20.74132754)
predict(regressor_RF, newdata = new2)
new3 <- data.frame(biking=30.77425409, smoking=23.61017497)
predict(regressor_RF, newdata = new3)

#RMSE
#sqrt(mean((dataset$y_test-y_pred)^2))
RMSE(testing_set$heart.disease,y_pred)
R2(testing_set$heart.disease,y_pred)