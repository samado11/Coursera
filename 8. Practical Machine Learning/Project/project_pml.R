# START: 2015.12.16
# END: 

# Data Importing
raw.train <- read.csv("pml-training.csv", stringsAsFactors = F)
raw.test <- read.csv("pml-testing.csv", stringsAsFactors = F)
colnames(raw.train)
raw.train$classe <- factor(raw.train$classe)

# Data Munging
# Extrating columns containing the NA value in test data set
nacolumn.test <- NULL
for (i in 1:dim(raw.test)[2]) {
    if (sum(!is.na(raw.test[, i])) == 0) {
        nacolumn.test <- c(nacolumn.test, colnames(raw.test)[i])
    }
}
# Removing columns that have same names with extracted columns above
# Cleaming the data frame
exclu.column <- c(nacolumn.test, "X", "user_name", "cvtd_timestamp", "raw_timestamp_part_1", 
                  "raw_timestamp_part_2", "new_window")
df.train <- raw.train[!names(raw.train) %in% exclu.column]

# Creating partion of training dataset for validation of the model
library(caret)
inTrain <- createDataPartition(df.train$classe, p= 0.7, list = F)
training <- df.train[inTrain, ]
validating <- df.train[-inTrain, ]
# Preprocessing with PCA
x.comp <- prcomp(training[, -54])
summary(x.comp)
# I choose five priciple components(about 80% propotion)

preProc <- preProcess(training[, -54], method = "pca", pcaComp = 5)
trainPC <- predict(preProc, training[, -54]); trainPC <- cbind(trainPC, training$classe)
validPC <- predict(preProc, validating[, -54]); validPC <- cbind(validPC, validating$classe)
names(trainPC)[6] <- "classe"; names(validPC)[6] <- "classe" 

# To choose the best model
resultModel <- data.frame(model = NULL, traindata_accuracy = NULL, validdata_accuracy = NULL)
applyModel <- c("rf", "svmRadial", "nnet")
library(parallel); cl <- makeCluster(4)
for (m in applyModel) {
    model <- train(classe ~ ., method = m, data = trainPC)
    trainout <- confusionMatrix(predict(model, newdata = trainPC), trainPC$classe)
    testout <- confusionMatrix(predict(model, newdata = validPC), validPC$classe)
    tmp <- data.frame(model = m, traindata_accuracy = trainout$overall[1], validdata_accuracy = testout$overall[1])
    resultModel <- rbind(resultModel, tmp)
}
stopCluster(cl = NULL)
print(resultModel)
# RandomForest Algorithm is best, So I have choosen the RF
model <- train(classe ~., method = "rf", data = rbind(trainPC, validPC))

# To test algorithm through the test dataset
df.test <- raw.test[!names(raw.test) %in% exclu.column]
testPC <- predict(preProc, df.test[, -54])
df.test <- cbind(df.test, predict(model, newdata = testPC))

# Mddeling method for avoid of over-fitting
