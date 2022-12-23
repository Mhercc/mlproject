# ML Project Write Up

## Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: [http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har] (see the section on the Weight Lifting Exercise Dataset).


## Data

The training data for this project are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.

## Machine Learning course project

library(lattice); library(ggplot2); library(caret); library(randomForest); library(rpart); library(rpart.plot);

set.seed(1234)

## loading training and testing data

trainingURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testingURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

## data load and clean up

training <- read.csv(url(trainingURL), na.strings = c("NA", "#DIV/0!", ""))
testing <- read.csv(url(testingURL), na.strings = c("NA", "#DIV/0!", ""))

dim(training); dim(testing)

## delete columns with all missing values

training <- training[,colSums(is.na(training)) == 0]
testing <- testing[,colSums(is.na(testing)) == 0]

## delete variables that are irrelevant, they are found in columns 1-7

training <- training[, -c(1:7)]
testing <- testing[, -c(1:7)]

## partitioning training dataset into 70% for training and 30% for testing
inTrain <- createDataPartition(y=training$classe, p=0.7, list = FALSE)

myTraining <- training[inTrain, ]

myTesting <- training[-inTrain, ]

dim(myTraining); dim(myTesting)

## building the prediction models
## the decision tree model

dtmodel <- rpart(classe~., data = myTraining, method = "class")

dtpredict <- predict(dtmodel, myTesting, type = "class")

#plot the decision tree

rpart.plot(dtmodel, main = "Classification Tree", extra=102, under=TRUE, faclen = 0)

confusionMatrix(dtpredict, as.factor(myTesting$classe))

## building the random forest model

rfmodel <- randomForest(as.factor(classe)~., data = myTraining)

rfpredict <- predict(rfmodel, myTesting, type = "class")

confusionMatrix(rfpredict, as.factor(myTesting$classe))

##random forest performed better than decision trees with an accuracy of 0.9956 (95% CI: 0.9935-0.9971)

## using the test set to calculate the out-of-sample error

predict2 <- predict(rfmodel, testing, type = "class")
predict2
