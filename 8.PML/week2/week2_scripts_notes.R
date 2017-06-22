options(sci = 999)
# "caret" package
# Some preprocessing (cleaning)
# - preProcess()
# Data splitting
# - createDataPartition
# - createResample
# - createTimeSlices
# Train/testing functions
# - train
# - predict
# Model comparison
# - confusionMatrix
## Example: Data splitting
library(caret)
library(kernlab)
data(spam)
set.seed(123)
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)
# fit a model
set.seed(123)
modelFit <- train(type ~ ., data = training, method = "glm")
predictions <- predict(modelFit, newdata = testing)
# evaluate the results
confusionMatrix(predictions, testing$type)
## Example: K-Fold
set.seed(123)
folds <- createFolds(y = spam$type, k = 10, list = TRUE, returnTrain = TRUE)
sapply(folds, length)
#Fold01 Fold02 Fold03 Fold04 Fold05 Fold06 Fold07 Fold08 Fold09 Fold10
# 4141   4142   4140   4141   4142   4140   4141   4141   4141  4140
# return only test sets
folds_test <- createFolds(y = spam$type, k = 10, list = TRUE, returnTrain = FALSE)
sapply(folds_test, length)
#Fold01 Fold02 Fold03 Fold04 Fold05 Fold06 Fold07 Fold08 Fold09 Fold10
#  460   460    460    460    459    461    461    460    460    460
folds[[1]][1:10]
#[1] 1 2 3 4 5 6 7 8 9 10
folds_test[[1]][1:10]
#[1] 1 47 73 89 93 101 103 129 130 136
## Example: Resampling
set.seed(123)
folds_resample <- createResample(y = spam$type, times = 10, list = TRUE)
sapply(folds_resample, length)
#Resample01 Resample02 Resample03 Resample04 Resample05 Resample06 Resample07 Resample08
#4601         4601        4601       4601       4601       4601       4601      4601
#Resample09 Resample10
#4601          4601
folds_resample[[1]][1:10]
#[1] 1 3 3 4 6 6 6 12 17 17
## Example: Time Slices
set.seed(123)
tme <- 1:1000
folds_time <- createTimeSlices(y = tme, initialWindow = 20, horizon = 10)
folds_time$train[[1]]
#[1] 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
folds_time$test[[1]]
#[1] 21 22 23 24 25 26 27 28 29 30

# Training options
## Example: SPAM
library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)
# fit a model
set.seed(123)
modelFit <- train(type ~ ., data = training, method = "glm")
# train(x, y, method = "rf", preProcess = NULL, ...,
#weights = NULL, metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
#maximize = ifelse(metric %in% c("RMSE", "logLoss"), FALSE, TRUE),
#trControl = trainControl(), tuneGrid = NULL,
#tuneLength = ifelse(trControl$method == "none", 1, 3))
# 'weights' is useful when meet with unbalancing data which has a lot more examples of one type than another.
# 'metric' = ifelse(is.factor(y), "Accuracy", "RMSE")
# 'trControl' = trainControl()
# trainControl(method = "boot", number = ifelse(grepl("cv", method), 
#10, 25), repeats = ifelse(grepl("cv", method), 1, number),
#p = 0.75, search = "grid", initialWindow = NULL, horizon = 1,
#fixedWindow = TRUE, skip = 0, verboseIter = FALSE, returnData = TRUE,
#returnResamp = "final", savePredictions = FALSE, classProbs = FALSE,
#summaryFunction = defaultSummary, selectionFunction = "best",
#preProcOptions = list(thresh = 0.95, ICAcomp = 3, k = 5,
#freqCut = 95 / 5, uniqueCut = 10, cutoff = 0.9), sampling = NULL,
#index = NULL, indexOut = NULL, indexFinal = NULL, timingSamps = 0,
#predictionBounds = rep(FALSE, 2), seeds = NA, adaptive = list(min = 5,
#alpha = 0.05, method = "gls", complete = TRUE), trim = FALSE,
#allowParallel = TRUE) 
# 'method' can be "boot" (bootstrapping), "boot632" (bootstrapping with adjustment), "cv" (cross validation), "repeatedcv" (repeated cross validation) and "LOOCV" = leave one out cross validation

# Plotting predictions
# Example: Wages
if (!require(ISLR)) install.packages("ISLR", repos = "https://cloud.r-project.org/")
library(ISLR)
library(ggplot2)
library(caret)
library(Hmisc)
library(grid)
data(Wage)
summary(Wage)
set.seed(123)
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
# plot features
featurePlot(x = training[, c("age", "education", "jobclass")], y = training$wage, plot = "pairs")
qplot(age, wage, data = training, colour = jobclass)
qq <- qplot(age, wage, data = training, colour = education)
qq + geom_smooth(method = "lm", formula = y ~ x)
cutWage <- cut2(x = training$wage, g = 3)
table(cutWage)
p1 <- qplot(cutWage, age, data = training, fill = cutWage) + geom_boxplot()
p2 <- qplot(cutWage, age, data = training, fill = cutWage, geom = c("boxplot", "jitter"))
grid.arrange(p1, p2, ncol = 2)
table(cutWage, training$jobclass)
#cutWage      1. Industrial 2. Information
#[20.1, 91.7)      445           258
#[91.7, 119.0)     365           358
#[119.0, 318.3]    268           408
t1 <- table(cutWage, training$jobclass)
prop.table(t1, 1)
# 1 = each row; 2 = each col; NULL = all
#cutWage       1. Industrial 2. Information
#[20.1, 91.7)     0.6330014      0.3669986
#[91.7, 119.0)    0.5048409      0.4951591
#[119.0, 318.3]   0.3964497      0.6035503
qplot(wage, colour = education, data = training, geom = "density")

# Basic preprocessing
# training and testing sets must be preprocessed in the same way
## Example: preProcessing
library(caret)
library(kernlab)
data(spam)
set.seed(123)
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve, main = "", xlab = "ave. capital run length")
mean(training$capitalAve)
sd(training$capitalAve)
# when the data is highly skewed, you could standardize thes data
trainCapAve <- training$capitalAve
# (xi - mean(x))/sd(x)
trainCapAveStd <- (trainCapAve - mean(trainCapAve)) / sd(trainCapAve)
#scale(trainCapAve, center = mean(trainCapAve), scale = sd(trainCapAve))
mean(trainCapAveStd)
sd(trainCapAveStd)
# if standardize the variable in the training set, then the one in the test set must also be standardized, and !!! when doing the standardization process, the mean must be the mean in the training set and the same for the standard deviation.
testCapAve <- testing$capitalAve
# (xi - mean(x))/sd(x)
testCapAveStd <- (testCapAve - mean(trainCapAve)) / sd(trainCapAve)
# use preProcess()
preObj <- preProcess(x = training[, -58], method = c("center", "scale"))
trainCapAvS <- predict(preObj, training[, -58])$capitalAve
mean(trainCapAvS)
sd(trainCapAvS)
# apply to the test set
testCapAvs <- predict(preObj, testing[, -58])$capitalAve
mean(testCapAvs)
sd(testCapAvs)
# add preprocess when build the model
modelFit <- train(type ~ ., data = training, preProcess = c("center", "scale"), method = "glm")
# Transformations in preProcess()
# Box-Cox transformation- normalize the data
par(mfrow = c(2, 2))
hist(trainCapAvS)
qqnorm(trainCapAvS)
preObj <- preProcess(training[, -58], method = "BoxCox")
trainsCapAveS <- predict(preObj, training[, -58])$capitalAve
hist(trainsCapAveS)
qqnorm(trainsCapAveS)
# Imputations in preProcess()
set.seed(123)
training$capAve <- training$capitalAve
selectNA <- rbinom(n = dim(training)[1], size = 1, prob = 0.5) == 1
training$capAve[selectNA] <- NA
# Impute and standardize
preObj <- preProcess(x = training[, -58], method = "knnImpute")
capAve <- predict(preObj, training[, -58])$capAve
# standardize em
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth - mean(capAveTruth)) / sd(capAveTruth)
# see difference
quantile(capAve - capAveTruth)
quantile((capAve - capAveTruth)[selectNA])
quantile((capAve - capAveTruth)[!selectNA])

# Covariate creation
# covariates are sometimes called features/predictors
# 2 levels of covariate creation
# Level 1: From raw data to covariates
# Depends heavily on application
# The balancing act is summarization vs. information loss
# Level 2: Transforming tidy covariates
# The raw data might not be related very well to the outcome, so they should be transformed a little bit.
# More necessary for some methods (regression, svn) than for others (classification trees)
# Should be done only on the training set
# Can be done by doing exploratory analysis
# The new covariates should be added to the df
## Example: level 2: transform covariates
data(spam)
# for example, if the og capitalAve is not very related to the outcome, then we try to square it
spam$capAveSq <- spam$capitalAve ^ 2
## Example: Covariate creation
library(ISLR)
library(ggplot2)
library(caret)
library(Hmisc)
library(grid)
data(Wage)
#summary(Wage)
set.seed(123)
inTrain <- createDataPartition(Wage$wage, p = 0.7, list = F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
# Basic idea- convert factor variables into indicator variables
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass, data = training)
head(predict(dummies, newdata = training))
training$jobclass <- as.character(training$jobclass)
testing$jobclass <- as.character(testing$jobclass)
training$jobclass <- ifelse(training$jobclass == "1. Industrial", 1, 2)
testing$jobclass <- ifelse(testing$jobclass == "1. Industrial", 1, 2)
# Removing zero covariates (the predictors which have no variability)
nsv <- nearZeroVar(x = training, saveMetrics = TRUE)
nsv
# spline basis - bs() to help to fit curvy lines, not the straight line
# bs() to create polynomial variables
library(splines)
bsBasis <- bs(x = training$age, df = 3)
# df = 3, then you will get three "new" variables
#bsBasis
# 1: corresponds to the actual age values (been scaled for computational purposes)
# 2: corresponds to the age squared
# 3: corresponds to the age cubed
#         1         2          3
#[1,] 0.0000000 0.0000000 0.000000000
#[2,] 0.4308138 0.2910904 0.065560908
#[3,] 0.3063341 0.4241549 0.195763821
#[4,] 0.4241549 0.3063341 0.073747105
#[5,] 0.3776308 0.0906314 0.007250512
#[6,] 0.4403553 0.2596967 0.051051492
# fitting curves with splines
#lm1 <- lm(wage ~ bsBasis, data = training)
#plot(training$age, training$wage, pch = 19, cex = 0.5)
#points(training$age,predict(lm1,newdata=training),col="red",pch=19,cex=0.5)
# splines on the test set
training <- cbind(training,bsBasis)
testing <- cbind(testing, predict(object = bsBasis, newx = testing$age))
names(training)[13:15] <- c("a", "b", "c")
names(testing)[13:15] <- c("a", "b", "c")
training <- training[, -c(2,3, 7)]
testing <- testing[, -c(2,3, 7)]
modelFit <- train(wage ~ ., data = training, preProcess = "knnImpute", trControl = trainControl(method = "cv", number=10), method = "glm")

modelFit$finalModel
modelFit$results
#MSE: sum((Prediction_i - Truth_i)^2)/n
#RMSE: sqrt(sum((Prediction_i - Truth_i)^2)/n)
sqrt(sum((predict(modelFit, newdata = testing) - testing$wage / testing$wage) ^ 2) / nrow(testing))

set.seed(123)
inTrain <- createDataPartition(Wage$wage, p = 0.7, list = F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
# Basic idea- convert factor variables into indicator variables
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass, data = training)
predict(dummies, newdata = training)
training$jobclass <- as.character(training$jobclass)
testing$jobclass <- as.character(testing$jobclass)
training$jobclass <- ifelse(training$jobclass == "1. Industrial", 1, 2)
testing$jobclass <- ifelse(testing$jobclass == "1. Industrial", 1, 2)
training <- training[, - c(3, 7)]
testing <- testing[, - c(3, 7)]
modelFit <- train(wage ~ ., data = training, preProcess = "knnImpute", trControl = trainControl(method = "cv", 10), method = "glm")
modelFit$finalModel
modelFit$results
#MSE: sum((Prediction_i - Truth_i)^2)/n
#RMSE: sqrt(sum((Prediction_i - Truth_i)^2)/n)
sqrt(sum((predict(modelFit, newdata = testing) - testing$wage / testing$wage) ^ 2) / nrow(testing))

# Preprocessing with Principal Components Analysis (PCA)
# PCA can help to reduce the dimension of the data-- reducing the correlation among variables.
library(caret)
library(kernlab)
data(spam)
set.seed(123)
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
mad <- abs(cor(training[,-58]))
# Every variable should has a correlation value which is equal to 1 to itself, so here just set the diagnal of the correlation matrix as 0
diag(mad) <- 0
which(mad >0.8,arr.ind = T)
names(spam)[c(34, 32, 40)]
#[1] "num415" "num857" "direct"
par(mfrow = c(1,3))
plot(spam[, 32], spam[, 34])
plot(spam[, 32], spam[, 40])
plot(spam[, 34], spam[, 40])
# To reduce the correlation, there are two ways to do:
# Way-1 (statistical) : Find a new set of multivariate variables that are uncorrelated and explain as much variance as possible
# Way-2 (data compression) : Put all variables in one matrix, find the best matrix created with fewer variables (lower rank) that explains the original data
# PCA/SVD
# SVD (singular value decomposition): If X is the matrix with each variable in a column and each observation in a row then the SVD is a "matrix decomposition":
#                             X = UDV^T
# where the columns of U are orthogonal (left singular vectors), the columns of V are orthogonal (right singular vectors) and D is a diagonal matrix (singular values)
# PCA is euqal to the right singular vectors if first scale (subtract the mean and divide by the standard deviation) the variables.
smallSpam <- spam[, c(32, 34, 40)]
prComp <- prcomp(smallSpam)
plot(prComp$x[, 1], prComp$x[, 2])
# Using preProcess()
# log10(spam[,-58]+1)-- make data a little bit more Gaussian
typeColor <- ((spam$type == "spam") * 1 + 1)
preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2) 
spamPC <- predict(preProc, log10(spam[, -58] + 1))
plot(spamPC[, 1], spamPC[, 2], col = typeColor)
# Apply to training set
data(spam)
set.seed(123)
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
preProc <- preProcess(log10(training[, -58] + 1), method = "pca", pcaComp = 2)
trainPC <- predict(preProc, log10(training[, -58] + 1))
trainPC$type = training$type
modelFit <- train(type ~ ., data = trainPC, method = "glm")
testPC <- predict(preProc, log10(testing[, -58] + 1))
testPC$type = testing$type
confusionMatrix(testing$type,predict(modelFit,testPC))
# Alternative way
modelFit <- train(type ~ ., method = "glm", preProcess = "pca", data = training)
confusionMatrix(testing$type,predict(modelFit,testing))

# Predicting with regression - single variable linear model
library(caret)
data(faithful)
set.seed(123)
inTrain <- createDataPartition(y = faithful$waiting,p = 0.5,list = F)
trainFaith <- faithful[inTrain,]
testFaith <- faithful[-inTrain,]
par(mfrow=c(3,1))
plot(trainFaith$waiting,trainFaith$eruptions,xlab="waiting",ylab="eruptions/duration",col="salmon",pch=19,main="training")
lm1 <- lm(eruptions ~ ., data = trainFaith)
summary(lm1)
lines(trainFaith$waiting,lm1$fitted.values,lwd=3,col="blue")
# predict a new variable, say a new waiting time: 80
coef(lm1)[1] + coef(lm1)[2]*80
# or
predict(lm1,newdata=data.frame(waiting = 80))
plot(testFaith$waiting, testFaith$eruptions, xlab = "waiting", ylab = "eruptions/duration", col = "salmon", pch = 19, main = "testing")
lines(testFaith$waiting, predict(lm1, testFaith), lwd = 3, col = "blue")
# calculate RMSE for training
sqrt(mean((lm1$fitted.values - trainFaith$eruptions) ^ 2))
# calculate RMSE for testing
sqrt(mean((predict(lm1, testFaith) - testFaith$eruptions) ^ 2))
# calculate the regression interval
pred1 <- predict(lm1, newdata = testFaith, interval = "prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions,col = "salmon", pch = 19)
matlines(testFaith$waiting[ord],pred1[ord,],type="l",col = c(1,2,2),lty = c(1,2,2),lwd=3)
# Using 'caret'
modelFit <- train(eruptions~waiting,data=trainFaith,method="lm",trControl=trainControl(method = "cv"))
summary(modelFit$finalModel)
pred1 <- predict(modelFit,newdata=testFaith)
rmse <- sqrt(mean((pred1 - testFaith$eruptions) ^ 2))
# or use RMSE(pred,obs)
rmse == RMSE(pred1, testFaith$eruptions)

# Predicting with regression - multivariate variables linear model
library(ISLR)
data(Wage)
library(ggplot2)
library(caret)
set.seed(123)
Wage <- subset(Wage, select = -c(logwage))
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
modelFit <- train(wage ~ age + jobclass + education, data = training, method = "lm")
finalModel <- modelFit$finalModel
print(modelFit)
plot(finalModel, 1, pch = 19, cex = 0.5, col = "#00000010")
plot(finalModel$residuals, pch = 19)
# .... model testing and diagnose






