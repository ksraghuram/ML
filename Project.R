train <- read.csv("train.csv")

train <- train[,-c(12,13,14,15,16,17,20,23,26,69,70,71,72,73,74,87,88,89,90,91,92,95,98,101,125,126,127,128,129,130,133,136,139)]
install.packages("caret")
library(caret)
library(ggplot2)
library(randomForest)
train[is.na(train)] <- 0


nzv <- nearZeroVar(train, saveMetrics = FALSE)

train <- train[,-nzv]

intrain <- createDataPartition(train$classe, p = 0.6, list = FALSE)

training <- train[intrain,]
testing <- train[-intrain,]


mod <- train(classe~. ,data = training, preProcess = c("center", "scale"))


mod <- preProcess(training, method = c("center", "scale"))


training <- predict(mod, training)
testing <- predict(mod, testing)

training <- training[,-1]
install.packages("e1071")
library(ggplot2)
library(e1071)

control <- rfeControl(functions=rfFuncs, method="cv", number=2)

results <- rfe(training[,1:57], training[,58], sizes=c(1:57), rfeControl=control)

print(results)

predictors(results)


ranfo <- randomForest( classe~. ,
                      data = training, do.trace = 100)

pred <- predict(ranfo, testing, type = "class")

confusionMatrix(pred, testing$classe)
  
table(testing$classe, pred)

ranfo

str(trainloan)
trainloan[,c(1,4,5,6,7,10,11)] <- as.numeric(trainloan[,c(1,4,5,6,7,10,11)])

trainloan[,14] <- as.integer(trainloan[,14])
testd <- read.csv("test.csv")

testd$classe1 <- c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")
testd$classe1 <- as.factor(testd$classe)


testd$classe
maintest <- data.frame(c(testd$raw_timestamp_part_1 , testd$cvtd_timestamp, testd$num_window, testd$roll_belt , testd$classe))

predictionmain <- predict(ranfo, testd)


testdata <- data.frame("raw_timestamp_part_1 "," cvtd_timestamp"," num_window"," roll_belt" , "classe")
testdata <- as.data.frame(testdata)
testdata <- testd[testdata]


str(training$roll_belt)

levels(testd$cvtd_timestamp) = levels(training$cvtd_timestamp)


totalData <- rbind(trainData, testData)
for (f in 1:length(names(totalData))) {
  levels(trainData[, f]) <- levels(totalData[, f])
}


qnorm(0.975)



4
3
predictionmain
