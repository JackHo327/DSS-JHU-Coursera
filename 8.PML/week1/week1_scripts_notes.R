# components of a predictor
# question -> input data -> features -> algorithm -> paramaters -> evaluation
library(kernlab)
data(spam)
#head(spam)
plot(density(spam$your[spam$type == "nonspam"]), col = "blue", main = "", xlab = "Frequency of 'your'")
lines(density(spam$your[spam$type == "spam"]), col = "red")
abline(v = 0.5, col="black")
# from the plot, the spam emails have more "your"s than nonspam emails have when the Freq is bigger than about 0.5.
# So, we say when the Freq of "your" is bigger than 0.5, then the emial is "spam" and if it is smaller than 0.5, then it is nonspam
prediction <- ifelse(spam$your > 0.5, "spam", "nonspam")
table(prediction, spam$type) / length(spam$type)


# Garbage in = Garbage out, so collecting the right data after regonizing the quesiton is the most important thing


# In smaple error: the error rate you get on the same data set you used to build your predictor. Sometimes called resubstitution error.
# Out of sample error: the error rate you get on a new data set (test data set). Sometimes called generalization error.
# Ususally, the out of sampel error is what we care about becasue the in sample error < out of sample error which is due to the overfitting problem. (noise)
# overfitting:
#             Data have two parts:
#                   1). Signal
#                   2). Noise
# The goal of a predictor is to find the signal. People can always create great in-sample predictors, then both signal and noise can be captured well. However, such predictors will preform bad when they meet with new data. 
