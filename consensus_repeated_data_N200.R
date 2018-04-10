#REPEATED DATA (measured N/2), 20 WORKERS

#--------------------------------------------------------------------------------------
#______________________________________________________________________________________
#Read the data
data_mushrooms_KAGGLE <- read.csv("mushroomsKAGGLE.csv")

#--------------------------------------
#DATA PREPROCESSING
#1. Dexp = (ID, labels); labels = (1,2) 1-eatable, 2 - poisonous

library(data.table)
#take only half of the data
N<-(length(data_mushrooms_KAGGLE[,1]))/2

#SIMULATE NOISY DATA for mixed =population of 100 workers. 
#rbinom(n, size, prob): n - number of observations, 
#size - number of trials, prob - prob of success of each trial

#Empty matrix of 200 workers. 
workers_l<-matrix(NA,nrow=N, ncol=200)

#First 20 workers are true experts, noise = 5%
for (i in 1:20) {
  nois = rbinom(N,1,0.05)
  lbs = mushrooms_Dexp[1:N,]$label
  lbs[mushrooms_Dexp[1:N,]$label==1 & nois]=2
  lbs[mushrooms_Dexp[1:N,]$label==2 & nois] = 1
  workers_l[,i]=lbs
}

#Second part of 4 workers are experts, noise = 20%
for (i in 21:40) {
  nois = rbinom(N,1,0.2)
  lbs = mushrooms_Dexp[1:N,]$label
  lbs[mushrooms_Dexp[1:N,]$label==1 & nois]=2
  lbs[mushrooms_Dexp[1:N,]$label==2 & nois] = 1
  workers_l[,i]=lbs
}

#Third part of 35 workers are amateurs, noise = 40%
for (i in 41:110) {
  nois = rbinom(N,1,0.4)
  lbs = mushrooms_Dexp[1:N,]$label
  lbs[mushrooms_Dexp[1:N,]$label==1 & nois]=2
  lbs[mushrooms_Dexp[1:N,]$label==2 & nois] = 1
  workers_l[,i]=lbs
}

#Last fourth part of 45 workers are adversaries, 
#completely random labeling; probability for every label = 0.5
for (i in 111:200) {
  nois = 1+rbinom(N,1,0.50)
  workers_l[,i]=nois
}

#data table = (ID, Dexp, w1,w2....... w10)
#data table with ID, Dexp labels and 10 workers labels with 30% noise.
mushrooms_Crowd=as.data.table(workers_l)

#MAJORITY VOTING
#library for majority voting
library(mclust) 
#matrix only with the labels, without IDs
labels_All<-workers_l

#the same, but directly with matrix, not data frame to matrix and without IDs.
consensus<-rep(0, times=N)
for (i in 1:N) {
  consensus[i]<-(majorityVote(workers_l[i,]))$majority
}

head(consensus)

consensus<-as.factor(consensus)
consensus = sapply(consensus, function(x){ifelse(x== 2, 'p', 'e')})
consensus<-as.factor(consensus)

#compare if all values of both vectors are the same:
#compare if all values of both vectors are the same:
all.equal(data_mushrooms_KAGGLE[1:N,1],consensus)


#DATA PARTITIONING: SEPARATE ORIGINAL AND CONSENSUS DATA TO TRAIN AND TEST DATA
set.seed(1234)
#replace the firts column of original data with the consensus labels
dataConsensus<-cbind(consensus, data_mushrooms_KAGGLE[1:N,-1])
fractionOfDataForTraining = 0.7 #70% train and 30% test data. 

partitionData <- function( data, fractionOfDataForTraining)
{
  numberOfRows <- nrow( data )
  randomRows   <- runif(numberOfRows)
  flag         <- randomRows <= fractionOfDataForTraining
  trainData <- data[ flag, ]
  testData  <- data[ !flag, ]
  dataSetSplit <- list( trainData = trainData, testData = testData )
  dataSetSplit
}

paritionedData <- partitionData(dataConsensus, fractionOfDataForTraining)
trainData   <- paritionedData$trainData
testData    <- paritionedData$testData

#Choose x1 = odor and x2=spore.print.color for building classification models. 

#CLASSIFICATION
library( caret )
library( pROC )

#schema = classification model: y = x1+x2
schema        <- consensus ~ odor + spore.print.color
actualResults <- ifelse( testData$consensus == "p", TRUE, FALSE )

#construct confusion matrix
getConfusionMatrix <- function( probabilityOfTests, actuals = actualResults, threshold = 0.4 )
{
  predictions    <- ifelse ( probabilityOfTests > threshold, 'p', 'e' )
  confusionMatrix( as.factor(testData$consensus), as.factor(predictions) )
}

#plot ROC curve
plotROCCurve <- function( predictionResults, title, color = "red" )
{
  plot( roc( testData$consensus, predictionResults, direction="<" ), 
        print.auc=TRUE, col = color, lwd = 3, main = title )
}


#LOGISTIC REGRESSION
glm.Model         <- glm( schema, data = trainData, family = "binomial" )
glm.Prediction    <- predict( glm.Model, testData, type = "response" )

#NAIVE BAYES CLASSIFICATOR
library( e1071 )
nb.Model         <- naiveBayes( schema, data = trainData )
nb.Prediction    <- predict( nb.Model, newdata = testData, type = "raw" )

#Classification using Neural Nets
library( nnet )

nn.Model      <- nnet( schema, data = trainData, size = 10, maxit = 200 )
nn.Prediction <- predict( nn.Model, newdata = testData, type = "raw")

#Classification using Random Forest
library( randomForest )

rf.Model          <- randomForest( schema, data = trainData )
rf.Prediction     <- predict( rf.Model, newdata = testData )
rf.BoolPrediction <- rf.Prediction == "p"

#Classification using Decision Tree
library(rpart)
dt.Model      <- rpart( schema, data = trainData )
dt.Prediction <- predict( dt.Model, newdata = testData )

#plot table with classification results

#modelsPredictions<-c(glm.Prediction, nb.Prediction[, 2], nn.Prediction, rf.BoolPrediction, dt.Prediction[,2])
#It does not work because the elements of modelsPredictions[i] are numerical values, not objects. 
#Accuracy<-sapply(modelsPredictions, function(x) getConfusionMatrix(x)$overall[[1]]) 

Accuracy<-c(getConfusionMatrix(glm.Prediction)$overall[[1]], getConfusionMatrix(nb.Prediction[, 2])$overall[[1]], getConfusionMatrix(nn.Prediction)$overall[[1]], getConfusionMatrix(rf.BoolPrediction)$overall[[1]], getConfusionMatrix(dt.Prediction[,2])$overall[[1]])
Sensitivity<-c(getConfusionMatrix(glm.Prediction)$byClass[[1]], getConfusionMatrix(nb.Prediction[, 2])$byClass[[1]], getConfusionMatrix(nn.Prediction)$byClass[[1]], getConfusionMatrix(rf.BoolPrediction)$byClass[[1]], getConfusionMatrix(dt.Prediction[,2])$byClass[[1]])
Specifity<-c(getConfusionMatrix(glm.Prediction)$byClass[[2]], getConfusionMatrix(nb.Prediction[, 2])$byClass[[2]], getConfusionMatrix(nn.Prediction)$byClass[[2]], getConfusionMatrix(rf.BoolPrediction)$byClass[[2]], getConfusionMatrix(dt.Prediction[,2])$byClass[[2]])

#print confusion tables
print("Confusion tables: ")
print("Logistic regression")
print(getConfusionMatrix(glm.Prediction)$table)
print("Naive Bayes")
print(getConfusionMatrix(nb.Prediction[, 2])$table)
print("Neural Nets")
print(getConfusionMatrix(nn.Prediction)$table)
print("Random Forest")
print(getConfusionMatrix(rf.BoolPrediction)$table)
print("Decision Tree")
print(getConfusionMatrix(dt.Prediction[,2])$table)

classificationResults<-data.frame(Accuracy, Sensitivity, Specifity)
row.names(classificationResults)<-c("Logistic Regression", "Naive Bayes", "Neural Nets", "Random Forest", "Decision Tree")
classificationResults

#Plot ROC curves
glmRoc <- roc( testData$consensus, glm.Prediction, direction="<" )
nbRoc  <- roc( testData$consensus, nb.Prediction[, 2], direction="<" )
nnRoc  <- roc( testData$consensus, nn.Prediction, direction="<" )
rfRoc  <- roc( testData$consensus, as.numeric( rf.Prediction == "p"), direction="<" )
dtRoc  <- roc( testData$consensus, dt.Prediction[, 2], direction="<" )

plot( glmRoc, print.auc = TRUE, col = "blue", main = "ROC Curves for Classifications" )
plot( nbRoc, add = TRUE, print.auc = TRUE, col = 'red',    print.auc.y = .2 )
plot( nnRoc, add = TRUE, print.auc = TRUE, col = 'orange', print.auc.y = .4 )
plot( rfRoc, add = TRUE, print.auc = TRUE, col = 'green',  print.auc.y = .6 )
plot( dtRoc, add = TRUE, print.auc = TRUE, col = 'violet', print.auc.y = .8 )


#--------------------------------------------------------------------------------------
#______________________________________________________________________________________









