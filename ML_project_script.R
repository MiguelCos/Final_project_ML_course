#### Loading packages ####

library(tidyverse)
library(caret)

#### Load datasets ####

pml_training <- read_csv("pml-training.csv")
pml_testing <- read_csv("pml-testing.csv")

#### Select features and variables (training set) ####

# Near zero variance variables elimination 

nvz <- nearZeroVar(pml_training, saveMetrics = TRUE)
nvz <- mutate(nvz, names = row.names(nvz))
nvz <- filter(nvz, nzv == TRUE)

train_1 <- select(pml_training, -one_of(nvz$names))

# Eliminate colunms with too much NAs

train_2 <- train_1[, colSums(is.na(train_1)) == 0]

# Eliminate first columns with no aparent meaning for prediction.

train_set <- select(train_2, -one_of(colnames(train_2)[1:6]))

#### Data slicing ####

set.seed(1001)

train_index <- createDataPartition(train_set$classe, p = 0.7, list = FALSE)

training <- train_set[train_index,]
testing <- train_set[-train_index,]

dim(training)
dim(testing)

#### Model building ####

trainCVmethod <- trainControl(method="cv", number=5, verboseIter=FALSE)

modelrf1 <- train(classe ~ ., data = training, method = "rf", trControl = trainCVmethod)

modelrf1
modelrf1$finalModel


#### Model evaluation ####

prediction1 <- predict(modelrf1, newdata = testing)
confusionMatrix(factor(testing$classe), prediction1)

#### Prediction on testing set (the one with 20 observations) ####

# Select features and variables in the test set 

# Near zero variance variables elimination 

nvz <- nearZeroVar(pml_training, saveMetrics = TRUE)
nvz <- mutate(nvz, names = row.names(nvz))
nvz <- filter(nvz, nzv == TRUE)

test_1 <- select(pml_testing, -one_of(nvz$names))

# Eliminate colunms with too much NAs

test_2 <- test_1[, colSums(is.na(train_1)) == 0]

# Eliminate first columns with no aparent meaning for prediction.

test_set <- select(test_2, -one_of(colnames(train_2)[1:6], "problem_id"))

#### Test prediction ####

blind_pred <- predict(modelrf1, newdata = test_set)

table(blind_pred, pml_testing$problem_id)
