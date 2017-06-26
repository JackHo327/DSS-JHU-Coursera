# Predicting with trees
# Pros:
#     Easy to interpret
#     Better performance in nonlinear settings
# Cons:
#     Without pruning/cross-validation can lead to overfitting
#     Harder to estimate uncertainty
#     Results may be variable
# Example: iris
data(iris)
library(ggplot2)
library(caret)
table(iris$Species)
set.seed(123)
inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = F)
train <- iris[inTrain,]
test <- iris[-inTrain,]
modelFit <- train(Species ~ ., data = train, method = "rpart")
print(modelFit$finalModel)
#setosa versicolor virginica
#50 50 50
#CART
#105 samples
#4 predictor
#3 classes:'setosa', 'versicolor', 'virginica'
#No pre - processing
#Resampling:Bootstrapped(25 reps)
#Summary of sample sizes:105, 105, 105, 105, 105, 105, ...
#Resampling results across tuning parameters:
#cp         Accuracy   Kappa
#0.0000000 0.9292793 0.8926587
#0.4571429 0.7442022 0.6252885
#0.5000000 0.5340752 0.3298759
#Accuracy was used to select the optimal model using the largest value.
#The final value used for
#the model was cp = 0.
#> print(modelFit$finalModel)
#n = 105
#node), split, n, loss, yval, (yprob)
#* denotes terminal node
#1) root 105 70 setosa(0.33333333 0.33333333 0.33333333)
#2) Petal.Length < 2.45 35 0 setosa(1.00000000 0.00000000 0.00000000) *
#3) Petal.Length >= 2.45 70 35 versicolor(0.00000000 0.50000000 0.50000000)
#6) Petal.Width < 1.65 38 3 versicolor(0.00000000 0.92105263 0.07894737) *
#7) Petal.Width >= 1.65 32 0 virginica(0.00000000 0.00000000 1.00000000) *
#plot(modelFit$finalModel, uniform = TRUE, main = "Classification Tree")
#text(modelFit$finalModel, use.n = TRUE, all = TRUE, cex = 0.6)
#library(rpart.plot)
#rpart.plot::rpart.plot(modelFit$finalModel)
#library(rattle)
#fancyRpartPlot(modelFit$finalModel)

# Bagging- Bootstrap aggregating
# Resample the dataset and recalculate predictions
# Average or majority vote
# Notes:
#       Similar bias
#       Reduced variance
#       More useful for non-linear functions
library(ElemStatLearn)
library(caret)
install.packages("party")
library(party)
data(ozone, package = "ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)
# normal ways
ll <- matrix(NA, nrow = 10, ncol = 155)
for (i in 1:10)
{
      ss <- sample(1:dim(ozone)[1], replace = T)
      ozone0 <- ozone[ss,]
      ozone0 <- ozone0[order(ozone$ozone),]
      loess0 <- loess(temperature ~ ozone, data = ozone0, span = 0.2)
      ll[i,] <- predict(loess0,newdata=data.frame(ozone=1:155))
}
plot(ozone$ozone, ozone$temperature, pch = 19, cex = 0.5)
for (i in 1:10)
{
      lines(1:155, ll[i,], col = "grey", lwd = 2)
      lines(1:155, apply(ll,2,mean), col = "red", lwd = 2)
}
# Bagging in caret
# three methods:
#     set the 'method' variable
#               bagEarth
#               bagEarthGCV
#               treebag
#               bagFDAGCV
set.seed(123)
inTrain <- createDataPartition(y = ozone$temperature, p = 0.7, list = F)
training <- ozone[inTrain,]
testing <- ozone[-inTrain,]

modelFit <- train(temperature ~ ozone, data = training, method = "treebag",trControl = trainControl(method = "cv", number = 10, returnResamp = "all"))
plot(testing$ozone, testing$temperature)
lines(testing$ozone, predict(modelFit, newdata = testing), col = "red")
# self-made bagging in caret
#predictors <- data.frame(ozone = ozone$ozone)
#temperature <- ozone$temperature
#treebag <- bag(predictors, temperature, B = 10, bagControl = bagControl(fit = ctreeBag$fit, predict = ctreeBag$pred, aggregate = ctreeBag$aggregate))
#plot(ozone$ozone, temperature)
#points(ozone$ozone, predict(treebag$fits[[1]]$fit, predictors), pch = 19, col = "red")
#points(ozone$ozone, predict(treebag, newdata = predictors), pch = 19, col = "blue")

# Random forest
# Bootstrap samples
# At each split, bootstrap variables
# Grow multiple trees and vote
# Pros:
#      Accuracy
# Cons:
#      Speed
#      Interpretability
#      Sometimes Overfitting - could use cross-validation to solve this problem
# Example: iris- Random forest
data(iris)
library(ggplot2)
library(caret)
#table(iris$Species)
set.seed(123)
inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = F)
train <- iris[inTrain,]
test <- iris[-inTrain,]
modelFit <- train(Species ~ ., data = train, method = "rf", trControl = trainControl(method = "cv", number = 10), prox = TRUE)
getTree(modelFit$finalModel, k = 2)
irisP <- classCenter(train[, c(3, 4)], train$Species, modelFit$finalModel$proximity)
irisP <- as.data.frame(irisP)
irisP$Species <- rownames(irisP)
qplot(Petal.Width, Petal.Length, col = Species, data = train) + geom_point(aes(x = Petal.Width, y = Petal.Length, col = Species), size = 5, shape = 4, data = irisP)
pred <- predict(modelFit, newdata = test)
confusionMatrix(pred, test$Species)
test$predRight <- pred==test$Species
qplot(Petal.Width, Petal.Length, col = predRight, data = test, main = "newdata pred", shape = Species) + geom_point()
#modelFit$finalModel$importance
#             MeanDecreaseGini
#Sepal.Length    7.986539
#Sepal.Width     1.293917
#Petal.Length    28.996959
#Petal.Width     31.075546
#rfcv(trainx = train[, -5], trainy = train[, 5], cv.fold = 10, step = 0.5)
#$n.var
#[1] 4 2 1
#$error.cv
#4 2 1
#0.02857143 0.02857143 0.03809524
#$predicted
#  ... ...

# Boosting
# Take lots of weak predictors
# Weight them and add them up
# Get a stronger predictor
# caret has:
#           gbm: boosting with trres
#           mboost: model based boosting
#           ada: statistical boosting based on additive logistic regression
#           gamBoost: for boosting generalized additive models
library(ISLR)
library(caret)
library(dplyr)
library(ggplot2)
data(Wage)
Wage <- Wage %>% select(-logwage)
set.seed(123)
inTrain <- createDataPartition(Wage$wage, p = 0.7, list = F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
modelFit <- train(wage~.,data=training,method="gbm",verbose=FALSE)
print(modelFit)
qplot(testing$wage, predict(modelFit, newdata = testing))

# Model based prediction
# Assume the data follow a probabilistic model
# Use Bayes' theorem to identify optimal classifiers
# Pros:
#      Can take advantage of structure of the adta
#      May be computationally convenient
#      Are reasonably accurate on real problems
# Cons:
#      Make additional assumptions about hte data
#      When the model is incorrect you may get reduced accuracy
# Example: iris
data(iris)
library(ggplot2)
names(iris)
table(iris$Species)
set.seed(123)
inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
modelA <- train(Species ~ ., data = training, method = "lda", trControl = trainControl(method = "cv", number = 10))
modelB <- train(Species ~ ., data = training, method = "nb", trControl = trainControl(method = "cv", number = 10))
predA <- predict(modelA, testing)
predB <- predict(modelB, testing)
table(predA, predB)
#          predB
#predA     setosa versicolor virginica
#setosa      15       0         0
#versicolor  0        13        1
#virginica   0        1         15
# Thus, Naive Bayes is quite similar to linear discriminant analysis
equalPred <- (predA == predB)
qplot(Petal.Width, Sepal.Width, colour=equalPred, data=testing)