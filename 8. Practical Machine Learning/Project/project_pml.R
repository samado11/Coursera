# START: 2015.12.16
# END: 2015.12.26

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

# EDA
boxplot(df.train[, 1:10])
boxplot(df.train[, 11:20])
boxplot(df.train[, 21:30])
boxplot(df.train[, 31:40])
boxplot(df.train[, 41:53])

# Imputing outliers to median value for avoiding over-fitting
for (i in 1:(dim(df.train)[2] - 1)) {
    df.train[df.train[, i] %in% boxplot.stats(df.train[, i])$out, i] <- median(df.train[, i])
}

# Creating partition of training dataset for validation of the model
if (!require(caret)) { install.packages("caret") }
inTrain <- createDataPartition(df.train$classe, p= 0.7, list = F)
training <- df.train[inTrain, ]
validating <- df.train[-inTrain, ]
# Preprocessing with PCA
x.comp <- prcomp(training[, -54])
summary(x.comp)
# I choose eight priciple component(about 90% propotion)

preProc <- preProcess(training[, -54], method = "pca", pcaComp = 8)
trainPC <- predict(preProc, training[, -54]); trainPC <- cbind(trainPC, training$classe)
validPC <- predict(preProc, validating[, -54]); validPC <- cbind(validPC, validating$classe)
names(trainPC)[9] <- "classe"; names(validPC)[9] <- "classe" 

# To choose the best model
if (!require(doParallel)) { install.packages("doParallel") }
registerDoParallel(cores = 4)

resultModel <- data.frame(model = NULL, 
                          traindata_accuracy = NULL, 
                          traindata_error_rate = NULL, 
                          validdata_accuracy = NULL, 
                          validdata_error_rate = NULL)
applyModel <- c("rf", "lda", "nb")
for (m in applyModel) {
    model <- train(classe ~ ., method = m, data = trainPC)
    trainout <- confusionMatrix(predict(model, newdata = trainPC), trainPC$classe)
    testout <- confusionMatrix(predict(model, newdata = validPC), validPC$classe)
    tmp <- data.frame(model = m, 
                      traindata_accuracy = trainout$overall[1], 
                      traindata_error_rate = 1 - trainout$overall[1], 
                      validdata_accuracy = testout$overall[1], 
                      validdata_error_rate = 1 - testout$overall[1])
    resultModel <- rbind(resultModel, tmp)
}
print(resultModel)

# RandomForest Algorithm is best, So I have choosen the RF
fit <- train(classe ~., method = "rf", data = rbind(trainPC, validPC))

# To test algorithm through the test dataset
df.test <- raw.test[!names(raw.test) %in% exclu.column]

for (i in 1:(dim(df.test)[2])) {
    df.test[df.test[, i] %in% boxplot.stats(df.test[, i])$out, i] <- median(df.test[, i])
}

testPC <- predict(preProc, df.test[, -54])
df.test <- cbind(df.test, predict(fit, newdata = testPC))
names(df.test)[55] <- "classe_predicted"

# Exporting the results of test dataset
pml_write_files <- function(x) {
    n = length(x)
    original_wd <- getwd()
    setwd(paste0(original_wd, "/output/"))
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
    }
    setwd(original_wd)
}
pml_write_files(df.test[, 56])