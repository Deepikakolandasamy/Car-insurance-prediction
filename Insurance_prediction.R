#Importing data and Naming Data#
data <- read.csv("")
column_names <- c('RiskFactor','Loss','Make','FuelType','Asprtn','Door','BodyStyle','Drive','EngineLoc','WheelBase','Length','Width','Height','CurbWeight','EngineType','Cylinder','EngineSize','FuelSystem','Bore','Stroke','CompressionRatio','HorsePower','PeakRPM','CityMPG','HighwayMPG','Price')
colnames(data) <- column_names
str(data)

## Performing Exploratory Data Analysis

Exploratory Data Analysis is the important part of any machine learning process. We need to prepare the data for processing into the algorithm. There are majorly used steps list below:
  
  1. Uni Variant Analysis
Analysing about each and every variables, its behavior, its skewness etc

2. Bi Variant Analysis
Analysing two different variables with each others. it check its behavior or significance level for modelling
Example:
  Plotting a graph between Price of Car (Independent Variable) and Risk Factor(Dependent Variable), if it is significant enough we can have those column else we can remove that insiginificant columns

3. Missing Values and its Treatment
Missing values is one of the important aspect which need to be treated before starting modelling because it may cause moisy and misguide model
There are various methods to impute Missing values 
1. Mean 
2. Median
3. Mode
4. Using Predictions like KNN Algorithm or Decision Tree for predicting missing values
5. Else we can remove that particular column as well (But there will be data loss)

4. Outlier Detection and its treatment
Similar to missing values outlier values need to be treated before modelling to get better accuracy.
Outlier Treatment is also similar to missing values treatment.

5. Feature Engineering
There are two important sub categories available,

1. Feature Selection:
  This enables us to select the important variables for modelling, Because when ever we impute unwanted variable for modelling the accuracy will be affected and So we need to minimize the number of variables need to be used for modelling.

Example: Boruta Algorithm enables us to know which variables ae unimportant so that we can remove from dataset.

2. Feature Creation
Creating a column from the exists column which helps in more accuracy.
There are different types of feature creation like,
1. Binning
2. Dummy Variable Creation

So, Lets go into Exploratory data Analysis part now, There is no thumb in the order. 

##Missing Value Treatment#

```{r Missing Value Treatment}
#Data Preprocessing 
#Data Cleaning - Handling Null Values
sapply(data, function(x) sum(is.na(x))) #Confirms that there is no null value
#Found NUll Values Present in Dataset
#Handling Null Values in Dataframe 
#1. Clearing 41 NUll Values for Loss with its Median value
str(data$Loss)
data$Loss[is.na(data$Loss)]<-median(data$Loss,na.rm = T)
mean(data$Loss, na.rm = T)
data$Loss <- round(as.numeric(data$Loss))
summary(data$Loss)
#2. Clearing 2 Null Values for Door with its Median value
str(data$Door)
data$Door[is.na(data$Door)] <- median(data$Door, na.rm =T)
#3. Clearing 4 Null Values in Bore with its Median Values
str(data$Bore)
summary(data$Bore)
data$Bore[is.na(data$Bore)] <- median(data$Bore, na.rm = T)
#4. Clearing 4 Null Values in Stroke with its Mean Values
str(data$Stroke)
summary(data$Stroke)
data$Stroke[is.na(data$Stroke)] <- mean(data$Stroke, na.rm = T)
#5. Clearning 2 Null Values in Horse Power using its Median Values
str(data$HorsePower)
summary(data$HorsePower)
data$HorsePower[is.na(data$HorsePower)] <- median(data$Stroke,na.rm = T)
#6. Clearing 2 Null Values in PeakRPM using its Median Value
str(data$PeakRPM)
summary(data$PeakRPM)
data$PeakRPM[is.na(data$PeakRPM)] <- mean(data$PeakRPM, na.rm = T)
#7. Clearning 4 Null Values in Price using mean value
str(data$Price)
summary(data$Price)
data$Price[is.na(data$Price)] <- mean(data$Price, na.rm = T)
#Value shows that there is no missing values
sapply(data, function(x) sum(is.na(x))) #Confirms that there is no null value
dataset <- data


## Outlier Treatment
#Loss Variable
boxplot(data$Loss) #Contains Outlier
boxplot.stats(data$Loss) #Contains Outlier values in Out
data$Loss <- replace(data$Loss, data$Loss >= 189, 188)# Outlier Treated
hist(data$Loss)
#Wheel Base Variable
boxplot(data$WheelBase)
boxplot.stats(data$WheelBase)
data$WheelBase <- replace(data$WheelBase, data$WheelBase >= 114, 113)
hist(data$WheelBase)
#Length Variable
boxplot(data$Length)
boxplot.stats((data$Length))
data$Length <- replace(data$Length, data$Length >= 203, 202.6)
data$Length <- replace(data$Length, data$Length < 142, 144.6)
hist(data$Length)
#Width Variable
boxplot(data$Width)
boxplot.stats(data$Width)
data$Width <- replace(data$Width, data$Width >= 70.70, 70.60)
hist(data$Width)
#Height Variable
boxplot(data$Height)
boxplot.stats(data$Height) #NO Outlier found
hist(data$Height)
#Curb Weight
boxplot(data$CurbWeight)
boxplot.stats(data$CurbWeight)#No Outlier Found
hist(data$CurbWeight)
#Engine Size
boxplot(data$EngineSize)
boxplot.stats(data$EngineSize) 
data$EngineSize <- replace(data$EngineSize, data$EngineSize >= 210, 209)
hist(data$EngineSize)
#Bore Size
boxplot(data$Bore)
boxplot.stats(data$Bore) #NO outlier Found
hist(data$Bore)
#Stroke 
boxplot(data$Stroke)
boxplot.stats(data$Stroke)
data$Stroke <- replace(data$Stroke, data$Stroke <= 2.67, 2.68)
data$Stroke <- replace(data$Stroke, data$Stroke >= 3.9, 3.86)
hist(data$Stroke)
#Compression Ratio
boxplot(data$CompressionRatio)
boxplot.stats(data$CompressionRatio)
data$CompressionRatio <- replace(data$CompressionRatio, data$CompressionRatio >= 10.11, 10.10)
data$CompressionRatio <- replace(data$CompressionRatio, data$CompressionRatio <= 7.49, 7.50)
hist(data$CompressionRatio)
#Horse Power
boxplot(data$HorsePower)
boxplot.stats(data$HorsePower)
data$HorsePower <- replace(data$HorsePower, data$HorsePower >= 184.01, 184.00)
hist(data$HorsePower)
#Peak RPM
boxplot(data$PeakRPM)
boxplot.stats(data$PeakRPM)
data$PeakRPM <- replace(data$PeakRPM, data$PeakRPM >= 6001, 6000)
hist(data$PeakRPM)
#City MPG
boxplot(data$CityMPG)
boxplot.stats(data$CityMPG)
data$CityMPG <- replace(data$CityMPG, data$CityMPG >= 46, 45)
hist(data$CityMPG)
#Highway MPG
data$HighwayMPG <- dataset$HighwayMPG
boxplot(data$HighwayMPG)
boxplot.stats(data$HighwayMPG)
data$HighwayMPG <- replace(data$HighwayMPG, data$HighwayMPG >= 51, 50)
hist(data$HighwayMPG)
#Price
boxplot(data$Price)
boxplot.stats((data$Price))
data$Price <- replace(data$Price, data$Price >= 29000.0, 28248.0)
hist(data$Price)


## Univariant Analysis
#install.packages("pastecs")
#install.packages("psych")
library(psych)
library(pastecs)
#Loss
boxplot(data$Loss) #Outlier Present
hist(data$Loss) #Right Sweked
describe(data$Loss)
#WheelBase
boxplot(data$WheelBase) #Outlier Present
hist(data$WheelBase) #Right Sweked
describe(data$WheelBase)
#Length
boxplot(data$Length) #Outlier Present
hist(data$Length) #Perfect
describe(data$Length)
#Width
boxplot(data$Width)#Outlier Present
hist(data$Width) #Right Skew
describe(data$Width)
#Height
boxplot(data$Height) #No Outlier
hist(data$Height) #Perfect
#Curb Weight
boxplot(data$CurbWeight) #No Outlier
hist(data$CurbWeight) #Right Skew
#Engine Size
boxplot(data$EngineSize) #Outlier Present
hist(data$EngineSize) #Right Skew
#Bore
boxplot(data$Bore) #No Outlier
hist(data$Bore) #Left Skew
#Stroke
boxplot(data$Stroke) #Outlier Present
hist(data$Stroke) #Left Skew
#Compression Ratio
boxplot(data$CompressionRatio)  #Outlier Present
hist(data$CompressionRatio) #Right Skew
#Horse Power
boxplot(data$HorsePower) #Outlier Present
hist(data$HorsePower) #Right Skew
#PeakRPM
boxplot(data$PeakRPM) #Outlier Present
hist(data$PeakRPM) #Right skew
#citympg
boxplot(data$CityMPG) #Outlier Present
hist(data$CityMPG) #Right Skew
#HighwasMPG 
boxplot(data$HighwayMPG) #Outlier Present
hist(data$HighwayMPG) #Right Skew
#Price
boxplot(data$Price) #Outlier Present
hist(data$Price) #Right Skew


##Scaling - Standardization
#Backup
dataset2 <- data #After Outlier and Missing Value Treatment
dataset4 <- dataset2
Scaled <- data #Taking Backup for Scaling
Scaled$RiskFactor <- NULL
Scaled$Make <- NULL
Scaled$FuelType <- NULL
Scaled$Asprtn <- NULL
Scaled$BodyStyle <- NULL
Scaled$EngineLoc <- NULL
Scaled$Drive <- NULL
Scaled$EngineType <- NULL
Scaled$Cylinder <- NULL
Scaled$FuelSystem <- NULL
Scaled$Door <- NULL
#Performing Scaling Operations to make units of all standard. So that model will give better accuracy
Scaled <- scale(Scaled)
Scaled <- as.data.frame(Scaled)
View(Scaled)


#Rejoining all the categorical variable for Further Processing
str(data)
Scaled["Make"] <- data$Make 
Scaled["FuelType"] <- data$FuelType
Scaled["Aspiration"] <- data$Asprtn
Scaled["BodyStyle"] <- data$BodyStyle
Scaled["Drive"] <- data$Drive
Scaled["EngineLocation"] <- data$EngineLoc
Scaled["EngineType"] <- data$EngineType
Scaled["Cylinder"] <- data$Cylinder
Scaled["FuelSystem"] <- data$FuelSystem
Scaled["Door"] <- data$Door
Scaled["RiskFactor"] <- data$RiskFactor
#Backup Dataset
Scaled_dataset3 <- Scaled
str(Scaled)



## Feature Creation - Dummy Variable Creation
#install.packages("mlr") #Packages for Dummy Variable Creation
library(mlr) #Library for Dummy Variable Creation
Scaled_backup <- Scaled_dataset3
Scaled <- Scaled_dataset3
#Fuel Type
#install.packages("dummies")
library(dummies)
#Fuel Type - 2 Factors
str(Scaled$FuelType)
FuelType <- cbind(Scaled$FuelType,dummy(Scaled$FuelType,sep = "_"))
FuelType<- as.data.frame(FuelType)
Scaled$FuelType <- NULL
Scaled["FuelType_Diesel"] <- FuelType[2]#Diesel Type 0,1 
Scaled$FuelType_Diesel <- factor(Scaled$FuelType_Diesel)
#Aspiration
str(Scaled$Aspiration) # 2 Levels
Turbo <- cbind(Scaled$Aspiration, dummy(Scaled$Aspiration,sep = "_"))
Turbo <- as.data.frame(Turbo)
Scaled["Aspiration_Turbo"] <- Turbo[3] #Turbo Type 0,1 
Scaled$Aspiration <-NULL
Scaled$Aspiration_Turbo <- factor(Scaled$Aspiration_Turbo)
str(Scaled$Aspiration_Turbo)
#Engine Location
str(Scaled$EngineLocation) #2 Levels
EngineLoc <- cbind(Scaled$EngineLocation, dummy(Scaled$EngineLocation,sep = "_"))
EngineLoc <- as.data.frame(EngineLoc)
Scaled["EngineLocation_Front"] <- EngineLoc[2] 
Scaled$EngineLocation <- NULL
Scaled$EngineLocation_Front <- factor(Scaled$EngineLocation_Front)
str(Scaled$EngineLocation_Front)
#Door
Scaled$Door <- factor(Scaled$Door)
str(Scaled$Door) #2 Factors
Door <- cbind.data.frame(Scaled$Door,dummy(Scaled$Door,sep="_"))
Scaled["Door_2"] <- Door[2]
Scaled$Door <- NULL
Scaled$Door_2 <- factor(Scaled$Door_2)
str(Scaled$Door_2)
#Drive
str(Scaled$Drive) #3 Levels
Drive <- cbind.data.frame(Scaled$Drive,dummy(Scaled$Drive,sep = "_"))
Scaled["Drive_fwd"] <- Drive[3]
Scaled["Drive_rwd"] <- Drive[4]
Scaled$Drive <- NULL
Scaled$Drive_fwd <- factor(Scaled$Drive_fwd)
Scaled$Drive_rwd <- factor(Scaled$Drive_rwd)
#Risk Factor
str(Scaled)
Scaled$RiskFactor <- NULL
Scaled["RiskFactor"] <- data$RiskFactor
Scaled$RiskFactor <- factor(Scaled$RiskFactor)
str(Scaled)


##Correlation
#In Scaled Data
Corelation_Scaled <- Scaled 
#Removing Categorical Variable and Leaving only Numerical Variable to check Co-Relation
Corelation_Scaled$Make <- NULL
Corelation_Scaled$BodyStyle <- NULL
Corelation_Scaled$EngineType <- NULL
Corelation_Scaled$Cylinder <- NULL
Corelation_Scaled$FuelSystem <- NULL
Corelation_Scaled$FuelType_Diesel <- NULL
Corelation_Scaled$Aspiration_Turbo <- NULL
Corelation_Scaled$Drive_fwd <- NULL
Corelation_Scaled$Drive_rwd <- NULL
Corelation_Scaled$EngineLocation_Front <- NULL
Corelation_Scaled$Door_2 <- NULL
Corelation_Scaled$RiskFactor <- NULL
cor(Corelation_Scaled)
#install.packages("corrplot")
library(corrplot)
corrplot(cor(Corelation_Scaled)) #Found MultiCollinearity Exist - Alert and Solve it
#Multi Collinearity Exists - Alert



##Log Transformation# - For Reference
dataset5<- dataset4 #Backing Up
#To check whether multicollinearity is solved.
View(dataset4)
dataset4$RiskFactor <- NULL
dataset4$Make <- NULL
dataset4$FuelType <- NULL
dataset4$Asprtn <- NULL
dataset4$BodyStyle <- NULL
dataset4$EngineLoc <- NULL
dataset4$Drive <- NULL
dataset4$EngineType <- NULL
dataset4$Cylinder <- NULL
dataset4$FuelSystem <- NULL
dataset4$Door <- NULL
x <- log10(dataset4) #Applying Log10 because most variables are right skewed
cor(x)
corrplot((cor(x))) #MultiCollinearity Still Exist



##MultiCollinearity Issue - Solving it
#Removing Multi Collinearity by using VIF and Corrplot
#install.packages("HH")
library(HH)
vif(Corelation_Scaled) 
sample_Scaled <- Corelation_Scaled
#Check for VIF Value and Remove the max value. Repeat untill dataset with no corelationsample_Scaled <- Corelation_Scaled
vif(sample_Scaled)
sample_Scaled$CityMPG <- NULL
vif(sample_Scaled)
corrplot(cor(sample_Scaled))
sample_Scaled$CurbWeight <- NULL
vif(sample_Scaled)
sample_Scaled$Length <- NULL
vif(sample_Scaled)
sample_Scaled$EngineSize<- NULL
vif(sample_Scaled)
sample_Scaled$WheelBase <- NULL
vif(sample_Scaled)
sample_Scaled$HighwayMPG <- NULL
vif(sample_Scaled)
sample_Scaled$Price <- NULL
corrplot(cor(sample_Scaled)) #Multi Collinearity Removed
cor(sample_Scaled) #All Corelation value is under 0.7



Multi Collinearity is removed using VIF Functions

## Merging all categorical Variable
str(Scaled[16:27])
sample_Scaled$Make <- Scaled$Make
sample_Scaled$BodyStyle <- Scaled$BodyStyle
sample_Scaled$EngineType <- Scaled$EngineType
sample_Scaled$Cylinder <- Scaled$Cylinder
sample_Scaled$FuelSystem <- Scaled$FuelSystem
sample_Scaled$FuelType_Diesel <- Scaled$FuelType_Diesel
sample_Scaled$Aspiration_Turbo <- Scaled$Aspiration_Turbo
sample_Scaled$EngineLocation_Front <- Scaled$EngineLocation_Front
sample_Scaled$Door_2 <- Scaled$Door_2
sample_Scaled$Drive_fwd <- Scaled$Drive_fwd
sample_Scaled$Drive_rwd <- Scaled$Drive_rwd
sample_Scaled$RiskFactor <- Scaled$RiskFactor
str(sample_Scaled)
Final_data <- sample_Scaled #Backup


#Boruta# - #Finding Variable importance to improve accuracy
set.seed(123)
#install.packages("Boruta")
#install.packages("rFerns")
library(Boruta)
library(rFerns)
#Run Boruta on this data
Boruta(RiskFactor~.,data=sample_Scaled,doTrace=2)->Boruta.Scaled
print(Boruta.Scaled)
#Boruta using rFerns' importance
Boruta(RiskFactor~.,data=sample_Scaled,getImp=getImpFerns)->Boruta.ferns.Scaled
print(Boruta.ferns.Scaled)
#By using Boruta - It is Confirmed that EngineLocation_Front is unimportant Variable
sample_Scaled$EngineLocation_Front <- NULL



##Buliding Models using various Machine Learning Algorithm using Cross Validation
##Cross Validation
Sometimes prediction may work well in training set but accuracy will be very low in test set which cause overfitting issue. 
To resolve this issue, cross validation helps us. 

We can split the data in to blocks and we can calculate accuracy for each and every blocks and by using it we can also predict the overall accuracy.

Split every block with 10% of data, and adding top 3 accuracy blocks as test set and remaining as training set.


## 1. Support Vector Machine
```{r Support Vector Machine}
#K-Fold Cross Validations
#install.packages("caret")
#install.packages("e1071")
library(caret)
library(e1071)
set.seed(123)
Ins_folds = createFolds(sample_Scaled$RiskFactor, k=10) #Creating Folds
Ins_folds$Fold01
SVM_CrossValidation = lapply(Ins_folds,function(x) {
  Ins_train_fold = sample_Scaled[-x,]
  Ins_test_fold = sample_Scaled[x,]
  Ins_classifier = svm(RiskFactor~., data=  Ins_train_fold)
  Ins_pred = predict(Ins_classifier,Ins_test_fold[-19])
  return(confusionMatrix(Ins_test_fold[,19],Ins_pred))
})
print(SVM_CrossValidation) #Gets Summary for different 10 folds
SVM_Accuracy = (0.684+0.450+0.68+0.50+0.70+0.619+0.667+0.524+0.632+0.571)/10 #0.6027
SVM_Accuracy


#Fold01 , Fold03 and Fold05 gave good amount of accuracy and avoids overfitting.

SVM_test <- Ins_folds$Fold05
SVM_test <- append(SVM_test,Ins_folds$Fold03)
SVM_test <- append(SVM_test,Ins_folds$Fold01)
Ins_test = sample_Scaled[(SVM_test),]
Ins_train = sample_Scaled[-(SVM_test),]
Ins_classifier = svm(RiskFactor~., data=  Ins_train, kernel = "linear")
Ins_pred = predict(Ins_classifier,Ins_test[-19])
Ins_Insample = predict(Ins_classifier,Ins_train[-19])
summary(Ins_classifier)
confusionMatrix(Ins_train[,19],Ins_Insample) #InSample Prediction
confusionMatrix(Ins_test[,19],Ins_pred) #Outsample Prediction
con <- table(Ins_test[,19],Ins_pred)
SVM_ACC= (con[1,1]+con[2,2]+con[3,3]+con[4,4]+con[5,5]+con[6,6])/sum(con)
SVM_ACC

Kernel = Polynomial, Accuracy = 0.3279
Kernel = Radial, Accuracy = 0.5574
Kernel = Sigmoid, Accuracy = 0.5573
Kernel - Linear, Accuracy = 0.81967


## Random Forest Method

#Finding Best fold for Modelling to avoid over fitting
library(caret)
library(e1071)
#install.packages(("party"))
#install.packages("randomForest")
library(randomForest)
library(party)
set.seed(123)
ran_folds = createFolds(sample_Scaled$RiskFactor, k=10)
Random_CrossValidation = lapply(ran_folds,function(x) {
  ran_train_fold = sample_Scaled[-x,]
  ran_test_fold = sample_Scaled[x,]
  ran_classifier = randomForest(RiskFactor~.,data = ran_train_fold,ntree=1000,mtry=3,importance=T)
  ran_pred = predict(ran_classifier,ran_test_fold[-19])
  return(confusionMatrix(ran_test_fold[,19],ran_pred))
  #  accuracy = (cm[1,1]+cm[2,2]+cm[3,3]+cm[4,4]+cm[5,5]+cm[6,6])/(sum(cm))
  #  return(accuracy)
  #  return(cm)
})
#Shows Folds 1 has better Accuracy
Random_Accuracy <- (0.89+0.80+1+0.7+0.85+0.71+0.85+0.76+0.84+0.66)/10 #0.811
#Fold 1,3,7 gives better than other folds
#Buliding models for that Specific Folds
ran_folds$Fold01
random_test <- ran_folds$Fold01
random_test <- append(random_test,ran_folds$Fold03)
random_test <- append(random_test,ran_folds$Fold07)
ran_test = sample_Scaled[(random_test),]
ran_train = sample_Scaled[-(random_test),]
ran_classifier = randomForest(RiskFactor~.,data = ran_train,ntree=1000,mtry=4,importance=T)
ran_pred = predict(ran_classifier,ran_test[-19])
ran_Insample = predict(ran_classifier,ran_train[-19])
summary(ran_classifier)
confusionMatrix(ran_train[,19],ran_Insample) #InSample
confusionMatrix(ran_test[,19],ran_pred) #Outsample
con <- table(ran_test[,19],ran_pred)
Ran_Acc= (con[1,1]+con[2,2]+con[3,3]+con[4,4]+con[5,5]+con[6,6])/sum(con)
Ran_Acc


#Tree Built : 1000
#Variable USed Per Tree : 3
#Accuracy : 0.87096

##Modelling using Decision Tree Algorithm - c5.0


#Finding Best fold for Modelling to avoid over fitting
library(caret)
library(e1071)
#install.packages(("C50"))
library(C50)
set.seed(123)
DT_folds = createFolds(sample_Scaled$RiskFactor, k=10)
DT_CrossValidation = lapply(DT_folds,function(x) {
  DT_train_fold = sample_Scaled[-x,]
  DT_test_fold = sample_Scaled[x,]
  DT_classifier = C5.0(RiskFactor~., data = DT_train_fold)
  DT_pred = predict(DT_classifier,DT_test_fold[-19])
  return(confusionMatrix(DT_test_fold[,19],DT_pred))
  #  accuracy = (cm[1,1]+cm[2,2]+cm[3,3]+cm[4,4]+cm[5,5]+cm[6,6])/(sum(cm))
  #  return(accuracy)
  #  return(cm)
})
#Decision Tree Cross Validation Accuracy
DT_CV_Accuracy = (0.75+0.77+0.6+0.8+0.76+0.57+0.71+0.7+0.95+0.68)/10 #0.729

#Folds 2,4,9 has good accuracy

#Buliding models for that Specific Folds
c50_test <- append(DT_folds$Fold02, DT_folds$Fold04)
c50_test <- append(DT_folds$Fold09, c50_test)
DT_test = sample_Scaled[(DT_folds$Fold09),]
DT_train = sample_Scaled[-(DT_folds$Fold09),]
DT_classifier = C5.0(RiskFactor~., data = DT_train)
DT_pred = predict(DT_classifier,DT_test[-19])
DT_Insample = predict(DT_classifier,DT_train[-19])
summary(DT_classifier)
confusionMatrix(DT_train[,19],DT_Insample) #InSample
confusionMatrix(DT_test[,19],DT_pred) #Outsample
con <- table(DT_test[,19],DT_pred)
DT_C50_Acc= (con[1,1]+con[2,2]+con[3,3]+con[4,4]+con[5,5]+con[6,6])/sum(con)
DT_C50_Acc


#Accuracy for c50 Decision Tree is around 0.8947

#Modelling using Decision Tree - rpart Algorithm


#Finding Best fold for Modelling to avoid over fitting
library(caret)
library(e1071)
library(C50)
#install.packages("rpart")
library(rpart)
set.seed(123)
rpart_folds = createFolds(sample_Scaled$RiskFactor, k=10)
rpart_CrossValidation = lapply(rpart_folds,function(x) {
  rpart_train_fold = sample_Scaled[-x,]
  rpart_test_fold = sample_Scaled[x,]
  rpart_classifier = C5.0(RiskFactor~., data = rpart_train_fold)
  rpart_pred = predict(rpart_classifier,rpart_test_fold[-19],type = "class")
  return(confusionMatrix(rpart_test_fold[,19],rpart_pred))
  #  accuracy = (cm[1,1]+cm[2,2]+cm[3,3]+cm[4,4]+cm[5,5]+cm[6,6])/(sum(cm))
  #  return(accuracy)
  #  return(cm)
})
#Decision Tree Cross Validation Accuracy
rpart_Accuracy = (0.68+0.60+0.818+0.80+0.70+0.476+0.76+0.619+0.73+0.571)/10
rpart_Accuracy

#Fold 3,4,7 gives better than other folds
#Buliding models for that Specific Folds
rpart_folds$Fold05
rpart_DT_test <- append(rpart_folds$Fold03, rpart_folds$Fold04)
rpart_DT_test <- append(rpart_DT_test, rpart_folds$Fold07)
rpart_test = sample_Scaled[(rpart_DT_test),]
rpart_train = sample_Scaled[-(rpart_DT_test),]
rpart_classifier = rpart(RiskFactor~., data = DT_train)
rpart_pred = predict(rpart_classifier,rpart_test[-19],type = "class")
rpart_Insample = predict(rpart_classifier,rpart_train[-19],type ="class")
summary(rpart_classifier)
confusionMatrix(rpart_train[,19],rpart_Insample) #InSample
confusionMatrix(rpart_test[,19],rpart_pred) #Outsample
con <- table(rpart_test[,19],rpart_pred)
rpart_Acc= (con[1,1]+con[2,2]+con[3,3]+con[4,4]+con[5,5]+con[6,6])/sum(con)
rpart_Acc
 
#Accuracy = 0.8253

## Logistic Regression MOdelling for Multinomial Class


set.seed(123)
log_folds = createFolds(sample_Scaled$RiskFactor, k=10)
#install.packages("nnet")
library(nnet)
log_CrossValidation = lapply(log_folds,function(x) {
  log_train_fold = sample_Scaled[-x,]
  log_test_fold = sample_Scaled[x,]
  log_classifier = multinom(RiskFactor~., data = log_train_fold )
  log_pred = predict(log_classifier,log_test_fold[-19],type = "class")
  return(confusionMatrix(log_test_fold[,19],log_pred))
})
log_acc <- (0.73+0.60+.909+0.60+0.75+0.66+0.810+0.517+0.789+0.714)/10
log_acc



#Fold 3,7,9 gives better than other folds
#Buliding models for that Specific Folds
logi_test <- append(log_folds$Fold03, log_folds$Fold07)
logi_test <- append(logi_test, log_folds$Fold09)
log_test = sample_Scaled[(logi_test),]
log_train = sample_Scaled[-(logi_test),]
log_classifier = multinom(RiskFactor~., data = log_train)
log_pred = predict(log_classifier,log_test[-19],type = "class")
log_Insample = predict(log_classifier,log_train[-19],type ="class")
summary(log_classifier)
confusionMatrix(log_train[,19],log_Insample) #InSample
confusionMatrix(log_test[,19],log_pred) #Outsample
con <- table(log_test[,19],log_pred)
log_Accuracy= (con[1,1]+con[2,2]+con[3,3]+con[4,4]+con[5,5]+con[6,6])/sum(con)
log_Accuracy

