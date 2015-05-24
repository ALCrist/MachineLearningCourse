##  The script creates a prediction model using the data from: 
##
##  Ugulino, W.; Cardador, D.; Vega, K.; Velloso, E.; Milidiu, R.; Fuks, H. 
##  Wearable Computing: Accelerometers' Data Classification of Body Postures and Movements. 
##  Proceedings of 21st Brazilian Symposium on Artificial Intelligence. 
##  Advances in Artificial Intelligence - SBIA 2012. In: Lecture Notes in 
##  Computer Science. , pp. 52-61. Curitiba, PR: Springer Berlin / Heidelberg, 2012. 
##  ISBN 978-3-642-34458-9. DOI: 10.1007/978-3-642-34459-6_6. 
##
##  Process Outline:
##  1.  Load packages need to perform analysis and create model
##  2.  Load training data
##  3.  Clean and subset data to prepare for modeling
##  4.  Split data into training and test sets
##  5.  Train data to create model
##  6.  Check results of model on training data
##  7.  Check results of model on testing data
##  8.  Plots
##
##
##  1.  Load packages need to perform analysis and create model
library(caret)
library(ggplot2)
setwd("~/.")
set.seed(5761)

##  2.  Load training data
if(!file.exists("./traindata")){
    train_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv "    
    dir.create("traindata", recursive=FALSE,showWarnings=TRUE)
    download.file(train_url, destfile = "./traindata/train_dataset.csv")
    dateDownloaded <- date()
}
    
##  3.  Clean and subset data to prepare for modeling
traindata <- read.csv("~/traindata/train_dataset.csv",
                      na.strings = c("#DIV/0!", " ","NA"), stringsAsFactors=TRUE)

cleanTrainData <- as.data.frame(traindata[,-which(colSums(is.na(traindata))>0)])
cleanTrainData <- cleanTrainData[,8:60]
cleanTrainData$classe <- as.factor(cleanTrainData$classe)

##  4.  Split data into training and test sets
InTraining <- createDataPartition(y=cleanTrainData$classe, p=.6, list=FALSE)
TrainSet <- cleanTrainData[InTraining,]
TestSet <- cleanTrainData[-InTraining,]

##  5.  Train data to create model
trnCtl <- trainControl(method="cv")
model4 <- train(as.factor(TrainSet$classe) ~., data=TrainSet, 
                method="rf", trControl= trnCtl)

##  6.  Check results of model on training data
Trainpredictions4 <- predict(model4, TrainSet)
confusionMatrix(Trainpredictions4, TrainSet$classe)

##  7.  Check results of model on testing data
Testpredictions4 <- predict(model4, TestSet)
confusionMatrix(Testpredictions4, TestSet$classe)

##  8.  Plots
plot(model4$finalModel, main="Final Model Error Rate Estimations")
