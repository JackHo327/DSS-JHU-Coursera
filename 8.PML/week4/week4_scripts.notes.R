# Regularized Regression
# Fit a regression model
# Penalize (or shrink) large coefficients
# Pros:
#      Can help with the bias/variance tradeoff
#      Can help with model selection
# Cons:
#      Maybe computationally demanding on large data sets
#      Does not perform as well as random forests and boosting
# Model Selection:
# 1). Divide data into train/test/validation
# 2). Treat validation as test data, train all competing models on the train data and pick the best one on validation.
# 3). To appropriately assess performance on new data apply to test set
# 4). May need to loop 1). - 3). steps
# * limited data; computational complexity
# Control the variance by regularizing/shrinking the coefficients
# PRSS- penalized form of the sum of squares
# -Penalty reduces complexity
# -Penalty reduces variance
# Penalty respects structure of the problem
# Ridge regression
# Lasso
#                 They have differences in the "penality"
# in caret: rigde,lasso,relaxo

# Combining predictors/Ensembling method
# Combine classifiers by averageing/voting
# Combining classifiers improves accuracy
# Combining classifiers reduces interpretability
# Boosting, bagging, and random forests are various on this theme
# These methods can combine similar classifiers
# Model stacking/Model ensembling can combine different classifiers
# Example: Model stacking
library(ISLR)
library(ggplot2)
library(caret)
library(mgcv)
data(Wage)
Wage <- Wage[, - (ncol(Wage) - 1)]
set.seed(123)
# create a build data set and a validation data set
inBuild <- createDataPartition(Wage$wage, p = 0.7, list = F)
validation <- Wage[-inBuild,]
buildData <- Wage[inBuild,]
set.seed(456)
inTrain <- createDataPartition(y = buildData$wage, p = 0.7, list = F)
training <- buildData[inTrain,]
testing <- buildData[-inTrain,]
mod1 <- train(wage ~ ., data = training, method = "glm")
mod2 <- train(wage ~ ., data = training, method = "rf", trControl = trainControl(method = "cv"), number = 3)
pred1 <- predict(mod1, newdata = testing)
pred2 <- predict(mod2, newdata = testing)
qplot(pred1, pred2, color = wage, data = testing)
# fit a model that combines the predictors
preDF <- data.frame(pred1, pred2, wage = testing$wage)
combModelFit <- train(wage ~ ., method = "gam", data = preDF)
combPred <- predict(combModelFit, preDF)
# see rmse on training dataset
sqrt(sum((pred1 - testing$wage) ^ 2))
sqrt(sum((pred2 - testing$wage) ^ 2))
sqrt(sum((combPred - testing$wage) ^ 2))
# apply models on validation datasets
pred1V <- predict(mod1, validation);
pred2V <- predict(mod2, validation)
predVDF <- data.frame(pred1 = pred1V, pred2 = pred2V)
combPredV <- predict(combModelFit, predVDF)
# see rmse on validation dataset
sqrt(sum((pred1V - validation$wage) ^ 2))
sqrt(sum((pred2V - validation$wage) ^ 2))
sqrt(sum((combPredV - validation$wage) ^ 2))
# Even simple blending can be useful
# Typical model for binary/multiclass data
# - BUild an od number of models
# - Predict with each model
# - Predict the class by majority vote

# Forecasting
# Deal with time sequence data|spatial data
# Example: Google data
library(quantmod)
library(forecast)
from.date <- as.Date("01/01/08", format = "%m/%d/%y")
to.date <- as.Date("12/31/13", format = "%m/%d/%y")
getSymbols(Symbols = "GOOGL", src = "google", from = from.date, to = to.date)
head(GOOGL)
mGoogle <- to.monthly(GOOGL)
googleOpen <- Op(mGoogle)
ts1 <- ts(googleOpen, frequency = 12)
plot(ts1, xlab = "Years+1", ylab = "GOOGL")
# Decompose the time series-- will decompose data into
# Trend- Consistently increasing pattern over time
# Seasonal- When there is a pattern over a fixed period of time that recurs
# Cyclic- When data rises and falls over non fixed periods
plot(decompose(ts1), xlab = "Years+1")
# train data
ts1Train <- window(ts1, start = 1, end = 5)
ts1Test <- window(ts1, start = 5, end = (7 - 0.1))
#ts1Train
plot(ts1Train)
# forecasting- do Moving-average smoothing
ma1 <- ma(ts1Train, order = 3)
#fcastets<- forecast(ma1, h = 5)
#plot(fcastets)
#lines(ts1Test, col = "red")
lines(ts1Test, col = "red")
# forcesting- do Exponential smoothing
ets1 <- ets(ts1Train, model = "MMM")
fcastets <- forecast(ets1)
plot(fcastets)
lines(ts1Test, col = "red")
accuracy(fcastets, ts1Test)
# Be aware of spurious correlations
# Be careful how far you predict (extrapolation)
# Be wary of dependencies over time- seasonal effects over time

# Unsupervised Prediction
# Sometimes you do not know the label
# To build a predictor:
#                      - Create clusters 
#                      - Name Clusters
#                      - Build predictor for cluster
# In a new data set
#                  - go and predict clusters
# Example: iris
data(iris)
library(ggplot2)
library(caret)
set.seed(123)
# here split data is aiming to create "new dataset"
inTrain <- createDataPartition(iris$Species, p = 0.7, list = F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
kMeans1 <- kmeans(training[, -5], centers = 3)
training$clusters <- as.factor(kMeans1$cluster)
qplot(Petal.Width, Petal.Length, colour = clusters, data = training)
table(kMeans1$cluster, training$Species)
# Build predictor
modelFit <- train(clusters ~ ., data = training[, -5], method = "rpart")
table(predict(modelFit, training), training$Species)
testClusterPred <- predict(modelFit, testing)
table(testClusterPred, testing$Species)

# Quiz 4
# Q1
library(ElemStatLearn)
library(caret)
data(vowel.train)
data(vowel.test)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
#modelFitRF <- train(y ~ ., data = vowel.train, method = "rf", trControl = trainControl(method = "cv", number = 10))
#modelFitBST <- train(y ~ ., data = vowel.train, method = "gbm", trControl = trainControl(method = "cv", number = 10), verbose = F)
modelFitRF <- train(y ~ ., data = vowel.train, method = "rf")
modelFitBST <- train(y ~ ., data = vowel.train, method = "gbm",verbose = F)
predRF <- predict(modelFitRF, newdata = vowel.test)
predBST <- predict(modelFitBST, newdata = vowel.test)
confusionMatrix(predRF, vowel.test$y)$overall[1]
confusionMatrix(predBST, vowel.test$y)$overall[1]
confusionMatrix(predBST, predRF)$overall[1]
predDF <- data.frame(predRF, predBST, y = vowel.test$y)
# Accuracy among the test set samples where the two methods agree
sum(pred_rf[predDF$predRF == predDF$predBST] ==
        predDF$y[predDF$predRF == predDF$predBST]) /
    sum(predDF$predRF == predDF$predBST)

# Q2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3 / 4)[[1]]
training = adData[inTrain,]
testing = adData[-inTrain,]
set.seed(62433)
modelFitRF <- train(diagnosis ~ ., data = training, method = "rf")
modelFitGBM <- train(diagnosis ~ ., data = training, method = "gbm",verbose=F)
modelFitLDA <- train(diagnosis ~ ., data = training, method = "lda")
predRF <- predict(modelFitRF,newdata=testing)
predGBM <- predict(modelFitGBM, newdata = testing)
prefLDA <- predict(modelFitLDA, newdata = testing)
confusionMatrix(predRF, testing$diagnosis)$overall[1]
confusionMatrix(predGBM, testing$diagnosis)$overall[1]
confusionMatrix(prefLDA, testing$diagnosis)$overall[1]
predDF <- data.frame(predRF, predGBM, prefLDA, diagnosis = testing$diagnosis, stringsAsFactors = F)
modelStack <- train(diagnosis ~ ., data = predDF, method = "rf")
combPred <- predict(modelStack, predDF)
confusionMatrix(combPred, testing$diagnosis)$overall[1]

# Q3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3 / 4)[[1]]
training = concrete[inTrain,]
testing = concrete[-inTrain,]
set.seed(233)
modelFit <- train(CompressiveStrength ~ ., data = training, method = "lasso")
library(elasticnet)
plot.enet(modelFit$finalModel, xvar = "penalty", use.color = TRUE)


# Q4
library(lubridate) # For year() function below
library(forecast)
dat = read.csv("./gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
modelForecast <- bats(y = tstrain)
pred1 <- forecast(modelForecast, h = dim(testing)[1], level = .95)
sum(pred1$lower < testing$visitsTumblr & pred1$upper > testing$visitsTumblr) / dim(testing)[1]

# Q5
set.seed(3523)
library(AppliedPredictiveModeling)
library(caret)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3 / 4)[[1]]
training = concrete[inTrain,]
testing = concrete[-inTrain,]
set.seed(325)
modelFit2 <- svm(CompressiveStrength ~ ., data = training)
predSVM2 <- predict(modelFit2, newdata = testing)
RMSE(predSVM2,obs = testing$CompressiveStrength)