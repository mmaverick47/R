# Call the libraries

install.packages("tidyr")
install.packages("kernlab")
install.packages("e1071")
install.packages("arulesViz")
install.packages("ggplot2")


library(tidyr)
library(kernlab)
library(e1071)
library(arulesViz)
library(ggplot2)
library(gridExtra)


# Step 1: Load the data

air <- airquality

# Clean the data

air <- replace_na(air,as.list(colMeans(air,na.rm=T)))

# Step 2: Create train and test data sets

# Create a list of random index for air data and store the index
dim(air)
randIndex <- sample(1:dim(air)[1])
length(randIndex)

# In order to split data, create a 2/3 cutpoint and round the number

cutpoint2_3 <- floor(2*dim(air)[1]/3)

# Create train data set, which contains the first 2/3 of overall data

trainData <- air[randIndex[1:cutpoint2_3],]
dim(trainData)
head(trainData)

# Create test data set, which contains the left 1/3 of the overall data

testData <- air[randIndex[(cutpoint2_3+1):dim(air)[1]],]
dim(testData)
head(testData)

model <- lm(Ozone ~ ., data=trainData)
summary(model)
lmPred <- predict(model, testData)

str(lmPred)
compTable3 <- data.frame(testData[,1],lmPred)
colnames(compTable3) <- c("test", "Pred")
sqrt(mean((compTable3$test-compTable3$Pred)^2))

# Create a linear model

compTable3$error <- abs(compTable3$test - compTable3$Pred)
Plot3 <- data.frame(compTable3$error, testData$Temp, testData$Wind)
colnames(Plot3) <- c("error", "Temp", "Wind")
lm.plot <- ggplot(Plot3, aes(x=Temp, y=Wind)) + geom_point(aes(size=error, color=error)) +ggtitle("lm")
lm.plot

# Step 3: Build a model using KSVM & visualize the results

# 1) Build a model (using the "ksvm" function, trying to predict ozone).
# You can use all the possible attributes that you think are helpful

ksvmOutput <- ksvm(Ozone~., 
                  data = trainData, 
                  kernel = "rbfdot", 
                  kpar = "automatic",
                  C = 10, 
                  cross = 10, 
                  prob.model = TRUE 
)
ksvmOutput

# 2) Test the model on the testing dataset & compute the RMS error

ksvmPred <- predict(ksvmOutput, 
                  testData, 
                  type = "votes" 
)

# Create a comparison dataframe that contains the exact "Ozone" value and the predicted "Ozone" value
# use for RMSE calc 

compTable <- data.frame(testData[,1], ksvmPred[,1])

# change the column names to "test" and "Pred"
colnames(compTable) <- c("test","Pred")

# compute the Root Mean Squared Error
sqrt(mean((compTable$test-compTable$Pred)^2)) 

# 3) Plot the results. Use a scatter plot. X-axis as temp, y- axis as wind,
# point size and color as error, as defined by actual ozone level minus 
# predicted ozone level

# compute absolute error for each case
compTable$error <- abs(compTable$test - compTable$Pred)

# create a new dataframe contains error, temp, and wind
ksvmPlot <- data.frame(compTable$error, testData$Temp, testData$Wind, testData$Ozone)

# assign column names
colnames(ksvmPlot) <- c("error","Temp","Wind", "Ozone")

# plot result using ggplot, setting "Temp" as x-axis and "Wind" as y-axis
plot.ksvm <- ggplot(ksvmPlot, aes(x=Temp,y=Wind)) +  geom_point(aes(size=error, color=error)) +
  ggtitle("ksvm")

plot.ksvm

# 4) Compute models and plot the results for "svm" (in the e1071 package) and "lm"
# Generate similar charts for each model

# - svm model

svmOutput <- svm(Ozone~., 
                   data = trainData, 
                   kernel = "linear", 
                   kpar = "automatic",
                   C = 10, 
                   cross = 10, 
                   prob.model = TRUE 
)
svmOutput


svmPred <- predict(svmOutput, 
                    testData, 
                    type = "votes" 
)

compTable2 <- data.frame(testData[,1], ksvmPred[,1])
colnames(compTable2) <- c("test","Pred")
sqrt(mean((compTable2$test-compTable2$Pred)^2)) 

# 3) Plot the results. 

compTable2$error <- abs(compTable2$test - compTable2$Pred)

svmPlot <- data.frame(compTable2$error, testData$Temp, testData$Wind, testData$Ozone)

colnames(svmPlot) <- c("error","Temp","Wind", "Ozone")

# plot result using ggplot, setting "Temp" as x-axis and "Wind" as y-axis
plot.svm <- ggplot(svmPlot, aes(x=Temp,y=Wind)) +  geom_point(aes(size=error, color=error)) +
  ggtitle("svm")

plot.svm



# Show all 3 results (charts) in one window, using the "grid.arrange" function

grid.arrange(plot.ksvm,plot.svm, ncol=2, nrow=2, top="Model Comparison")

# Step 4: Create a "goodOzone" variable

# calculate average Ozone
meanOzone <- mean(air$Ozone,na.rm=TRUE)

# create a new variable named "goodOzone" in train data set
# goodOzone = 0 if Ozone is below average Ozone
# googOzone = 1 if Ozone is eaqual or above the average ozone
trainData$goodOzone <- ifelse(trainData$Ozone<meanOzone, "low", "high")

# do the same thing for test dataset
testData$goodOzone <- ifelse(testData$Ozone<meanOzone, "low", "high")

# remove "Ozone" from train data
trainData <- trainData[,-1]

# remove "Ozone" from test data
testData <- testData[,-1]


# Step 5: See if we can do a better job of predicting "good" and "bad" days

# convert "goodOzone" in train data from numeric to factor
trainData$goodOzone <- as.factor(trainData$goodOzone)

# convert "goodOzone" in test data from numeric to factor
testData$goodOzone <- as.factor(testData$goodOzone)

# 1) Build a model (using "ksvm" function, trying to predict "goodOzone")
# Can use all possible attributes or select attributes you think are helpful

ksvmGood <- ksvm(goodOzone~., 
                data=trainData, 
                kernel="rbfdot", 
                kpar="automatic",
                C=10, 
                cross=10, 
                prob.model=TRUE 
)
ksvmGood

# 2) Test the model on the testing dataset and compute % of "goodOzone" correctly predicted

goodPred <- predict(ksvmGood,  testData )

# create a dataframe that contains the exact "goodOzone" value and the predicted "goodOzone"
compGood1 <- data.frame(testData[,6], goodPred)

# change column names
colnames(compGood1) <- c("test","Pred")

# Compute the percentage of correct cases
perc_ksvm <- length(which(compGood1$test==compGood1$Pred))/dim(compGood1)[1]
perc_ksvm

# Confusion Matrix
#   
results <- table(original=compGood1$test, pred=compGood1$Pred)
print(results)

# 3) Plot the results. X-axis is temp, y-axis is wind, shape is predicted (good or bad),
# color is actual value of "goodOzone", size is if prediction was correct (larger
# symbols should be the observations the model got wrong)

# determine the prediction is "correct" or "wrong" for each case
compGood1$correct <- ifelse(compGood1$test==compGood1$Pred,"correct","wrong")

# create a new dataframe contains correct, tempreture and wind, and goodZone
Plot_ksvm <- data.frame(compGood1$correct,testData$Temp,testData$Wind,testData$goodOzone,compGood1$Pred)

# change column names
colnames(Plot_ksvm) <- c("correct","Temp","Wind","goodOzone","Predict")

# polt result using ggplot
# size representing correct/wrong; color representing actual good/bad day; shape representing predicted good/bad day.
plot.ksvm.good <- ggplot(Plot_ksvm, aes(x=Temp,y=Wind)) + 
  geom_point(aes(size=correct,color=goodOzone,shape = Predict))+
  ggtitle("ksvm - good/bad ozone")

plot.ksvm.good

# 4) Compute models and plot the results for "svm" and "nb"

# SVM Model
svmGood2 <- svm(goodOzone~.,data=trainData,kernel="radial",C=10,cross=10,prob.model=TRUE)
svmGood2

#test the svm model
goodPred2 <- predict(svmGood2,testData)

compGood2 <- data.frame(testData[,6],goodPred2)
colnames(compGood2) <- c("test","Pred")
perc_svm <- length(which(compGood2$test==compGood2$Pred))/dim(compGood2)[1]
perc_svm

#confusion matrix
results2 <- table(test=compGood2$test,pred=compGood2$Pred)
print(results2)

#plot results of svm model
compGood2$correct <- ifelse(compGood2$test==compGood2$Pred,"correct","wrong")
plot.svm <- data.frame(compGood2$correct,testData$Temp,testData$Wind,testData$goodOzone,compGood2$Pred)
colnames(plot.svm) <- c("correct","Temp","Wind","goodOzone","Predict")
plot.svm.good <- ggplot(plot.svm,aes(x=Temp,y=Wind)) + geom_point(aes(size=correct,color=goodOzone,shape=Predict)) + ggtitle("svm - ggod/bad ozone")
plot.svm.good

# NB model
#Use the Function "naiveBayes"

nbGood2 <- naiveBayes(goodOzone~.,data=trainData,kernel="radial",C=10,cross=10,prob.model=TRUE)
nbGood2

#test the nb model
goodPred3 <- predict(nbGood2,testData)

compGood3 <- data.frame(testData[,6],goodPred3)
colnames(compGood3) <- c("test","Pred")
perc_nb <- length(which(compGood3$test==compGood3$Pred))/dim(compGood3)[1]
perc_nb

#confusion matrix
results3 <- table(test=compGood3$test,pred=compGood3$Pred)
print(results3)

#plot results of svm model
compGood3$correct <- ifelse(compGood3$test==compGood3$Pred,"correct","wrong")

#plot results of nb model
compGood3$correct <- ifelse(compGood3$test==compGood3$Pred,"correct","wrong")
plot.nb <- data.frame(compGood3$correct,testData$Temp,testData$Wind,testData$goodOzone,compGood3$Pred)
colnames(plot.nb) <- c("correct","Temp","Wind","goodOzone","Predict")
plot.nb.good <- ggplot(plot.nb,aes(x=Temp,y=Wind)) + geom_point(aes(size=correct,color=goodOzone,shape=Predict)) + ggtitle("nb - ggod/bad ozone")
plot.nb.good

# 5) Show all 3 results in one window, using grid.arrange

grid.arrange(plot.ksvm.good,plot.svm.good, plot.nb.good, ncol=3, nrow=3, top="Model Comparison")

# Step 6: Which are the best models for this data?

# Review what you have done and state which is the best and why

#