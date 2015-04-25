## Packagues and libraries needed.
library(lattice)
library(ggplot2)
library(caret)
library(kernlab)
library(randomForest)
library(corrplot)
## check if a data folder exists; if not then create one
if (!file.exists("file")) {dir.create("file")}
### set up file URL 
f1 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
root1 <- "./file/pml-training.csv"
f2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
root2 <- "./file/pml-testing.csv"
### Download data frame
download.file(f1, destfile = root1)
download.file(f2, destfile = root2)
dateDownloaded <- date()
### Read data fraME
data.training <- read.csv("./file/pml-training.csv", na.strings= c("NA",""," "))
# Clean data frame (eliminate NA's)
data.training.NA <- apply(data.training, 2, function(x) {sum(is.na(x))})
data.training.NONA <- data.training[,which(data.training.NA == 0)]
# Remove columns lables 
data.training.NONA <- data.training.NONA[8:length(data.training.NONA)]
# Prepare data frame in to testing, training and test (cross validation)
inTrain <- createDataPartition(y = data.training.NONA$classe, p = 0.7, list = FALSE)
training <- data.training.NONA[inTrain, ]
test <- data.training.NONA[-inTrain, ]
# Correlation test
cor_matrix <- cor(training[, -length(training)])
corrplot(cor_matrix, order = "FPC", method = "circle", type = "lower", tl.cex = 0.8,  tl.col = rgb(0, 0, 0))
# Run fit a model to predict the classe using the other variables
fit_model <- randomForest(classe ~ ., data = training)
# Create a crossvalidate with test data (test <- data.training.NONA[-inTrain, ] this mean the 30% of ramaining data)
prediction <- predict(fit_model, test)
new_matrix(test$classe, prediction)
# Use the same method in test data
data.test <- read.csv("./file/pml-testing.csv", na.strings= c("NA",""," "))
data.test.NA <- apply(data.test, 2, function(x) {sum(is.na(x))})
data.test.NONA <- data.test[,which(data.test.NA == 0)]
data.test.NONA <- data.test.NONA[8:length(data.test.NONA)]
# Prediction classes on test 
prediction.test <- predict(fit_model, data.test.NONA)
print(prediction.test)



