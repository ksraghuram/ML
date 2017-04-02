library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)


inTrain <- createDataPartition(seg$Case, p = 0.7, list = FALSE)

seg <- data.frame(segmentationOriginal)

training111 <- seg[inTrain,]
testing111 <- seg[-inTrain,]

set.seed(125)

tree <- train(Class~., data = training111, method = "rpart")

tree$finalModel
library(rattle)

fancyRpartPlot(tree$finalModel)
plot(tree$finalModel)
tree$finalModel
rpart <- as.party(tree)

install.packages("partykit")
library(partykit)
data(olive)

olive <- data.frame(olive)
intrain <- createDataPartition(olive$Area, p = 0.7, list = FALSE)

traiining <- olive[intrain,]
tessting <- olive[-intrain,]

newtree <- train(Area~., data = traiining, method = "rpart")

newdata = as.data.frame(t(colMeans(olive)))

prediction <- predict(newtree, newdata)
prediction




install.packages("ElemStatLearn")
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]


set.seed(13234)
fit <- train(chd ~ age + alcohol + obesity + tobacco + ldl + typea, data = trainSA, mrethod = "glm", family = "binomial")
fit$bestTune

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
fitt <- fit$finalModel
predictions <- predict(fitt)
predictions
missClass(testSA$chd,predictions)







library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
library(randomForest)

ranfo <- randomForest(vowel.train$y~., data = vowel.train)
print(ranfo)


varImp(ranfo)


#Creating a progress bar to know the status of CV
progress.bar <- create_progress_bar("text")
progress.bar$init(k)