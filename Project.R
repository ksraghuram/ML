train <- read.csv("train.csv")

train <- train[,-c(12,13,14,15,16,17,20,23,26,69,70,71,72,73,74,87,88,89,90,91,92,95,98,101,125,126,127,128,129,130,133,136,139)]

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

library(ggplot2)

control <- rfeControl(functions=rfFuncs, method="cv", number=2)

results <- rfe(training[,1:57], training[,58], sizes=c(1:57), rfeControl=control)

print(results)

predictors(results)


ranfo <- randomForest( classe~raw_timestamp_part_1 + cvtd_timestamp + roll_belt+num_window + magnet_dumbbell_z + magnet_dumbbell_y,
                      data = training, do.trace = 50)

pred <- predict(ranfo, testing, type = "class")

confusionMatrix(pred, testing$classe)
  
table(testing$classe, pred)

ranfo

str(trainloan)
trainloan[,c(1,4,5,6,7,10,11)] <- as.numeric(trainloan[,c(1,4,5,6,7,10,11)])

trainloan[,14] <- as.integer(trainloan[,14])
