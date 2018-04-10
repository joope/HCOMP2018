#Crowdsourcing project - part 1 - majority voting

#--------------------------------------------------------------------------------------
#______________________________________________________________________________________
#same data, but with column names, repeat the same preprocessing steps as before:
#
#1. 
#Read the data
data_mushrooms_KAGGLE <- read.csv("mushroomsKAGGLE.csv")
#clases y - first column. 
dim(data_mushrooms_KAGGLE)
head(data_mushrooms_KAGGLE)
str(data_mushrooms_KAGGLE)

#--------------------------------------
#DATA PREPROCESSING
#1. Dexp = (ID, labels); labels = (1,2) 1-eatable, 2 - poisonous

#change levels of first column to e=0, p=1. 
#levels(data_mushrooms[,1])<-c(0,1) #'0','1'
#setattr(data_mushrooms[,1],"levels",c(0,1)) #c(0,1)

library(data.table)
N<-length(data_mushrooms_KAGGLE[,1])

#SIMULATE NOISY DATA for mixed =population of 1000 workers. 
#rbinom(n, size, prob): n - number of observations, 
#size - number of trials, prob - prob of success of each trial

#Empty matrix of 50 workers. 
workers_l<-matrix(NA,nrow=N, ncol=500)

#First 50 workers are true experts, noise = 5%
for (i in 1:50) {
  nois = rbinom(N,1,0.05)
  lbs = mushrooms_Dexp$label
  lbs[mushrooms_Dexp$label==1 & nois]=2
  lbs[mushrooms_Dexp$label==2 & nois] = 1
  workers_l[,i]=lbs
}

#Second part of 50 workers are experts, noise = 20%
for (i in 51:100) {
  nois = rbinom(N,1,0.2)
  lbs = mushrooms_Dexp$label
  lbs[mushrooms_Dexp$label==1 & nois]=2
  lbs[mushrooms_Dexp$label==2 & nois] = 1
  workers_l[,i]=lbs
}

#Third part of 150 workers are amateurs, noise = 40%
for (i in 101:250) {
  nois = rbinom(N,1,0.4)
  lbs = mushrooms_Dexp$label
  lbs[mushrooms_Dexp$label==1 & nois]=2
  lbs[mushrooms_Dexp$label==2 & nois] = 1
  workers_l[,i]=lbs
}

#Last fourth part of 250 workers are adversaries, 
#completely random labeling; probability for every label = 0.5
for (i in 251:500) {
  nois = 1+rbinom(N,1,0.50)
  workers_l[,i]=nois
}

#data table = (ID, Dexp, w1,w2....... w10)
#data table with ID, Dexp labels and 10 workers labels with 30% noise.
mushrooms_Crowd=as.data.table(workers_l)
head(mushrooms_Crowd)

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
all.equal(data_mushrooms_KAGGLE[,1],consensus)

#for mixed population of 1000 workers: 250 - 0.05% noise; 
#250 - 20% noise, 250 - 40% noise, 250 random 0.5 prob of succes - consensus = true labels. 


#DATA PARTITIONING: SEPARATE ORIGINAL AND CONSENSUS DATA TO TRAIN AND TEST DATA
#Splitting of these data sets should be randomly done.
#The function to partition the data takes the data frame and a decimal between 0 and 1 
#that represents the fraction of data to dedicate for the training data as parameters.

set.seed(1234)
#replace the firts column of original data with the consensus labels
dataConsensus<-cbind(consensus, data_mushrooms_KAGGLE[,-1])
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
#common schema: use only class and odor for classification.
#it is possible also to use other X variables, but since we are 
#not focused to investigate classification in this partricular set, 
#we decided to simplify the problem and use only x1 = odor in our classifications.

#foraccuracy of the model use a confusion matrix via the caret library
#use ROC curve via the pROC library
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

logRegResults<-getConfusionMatrix( glm.Prediction )
print( getConfusionMatrix( glm.Prediction ))
plotROCCurve( glm.Prediction, "ROC Curve for Logistical Regression")


#NAIVE BAYES CLASSIFICATOR
library( e1071 )
nb.Model         <- naiveBayes( schema, data = trainData )
nb.Prediction    <- predict( nb.Model, newdata = testData, type = "raw" )

NaiveBayesResults<-getConfusionMatrix( nb.Prediction )
print( getConfusionMatrix( nb.Prediction[, 2] ))
plotROCCurve( nb.Prediction[, 2], "ROC Curve for Naive Bayesian Classification")

#Classification using Neural Nets
library( nnet )

nn.Model      <- nnet( schema, data = trainData, size = 10, maxit = 200 )
nn.Prediction <- predict( nn.Model, newdata = testData, type = "raw")
NeuralNetsResults<-getConfusionMatrix( nn.Prediction )

print( getConfusionMatrix( nn.Prediction ))
plotROCCurve( nn.Prediction, "ROC Curve for Neural Nets")

#Classification using Random Forest
library( randomForest )

rf.Model          <- randomForest( schema, data = trainData )
rf.Prediction     <- predict( rf.Model, newdata = testData )
rf.BoolPrediction <- rf.Prediction == "p"

RandomForestResults<-getConfusionMatrix( rf.BoolPrediction )
print( getConfusionMatrix( rf.BoolPrediction ))

plotROCCurve( as.numeric( rf.Prediction == "p"), "ROC Curve for Random Forest")

#Classification using Decision Tree
library(rpart)
dt.Model      <- rpart( schema, data = trainData )
dt.Prediction <- predict( dt.Model, newdata = testData )

DecisionTreeResults<-getConfusionMatrix( dt.Prediction )
print( getConfusionMatrix( dt.Prediction[, 2] ))
plotROCCurve( dt.Prediction[, 2], "ROC Curve for Decision Tree")


#plot table with classification results

#modelsPredictions<-c(glm.Prediction, nb.Prediction[, 2], nn.Prediction, rf.BoolPrediction, dt.Prediction[,2])
#It does not work because the elements of modelsPredictions[i] are numerical values, not objects. 
#Accuracy<-sapply(modelsPredictions, function(x) getConfusionMatrix(x)$overall[[1]]) 

Accuracy<-c(getConfusionMatrix(glm.Prediction)$overall[[1]], getConfusionMatrix(nb.Prediction[, 2])$overall[[1]], getConfusionMatrix(nn.Prediction)$overall[[1]], getConfusionMatrix(rf.BoolPrediction)$overall[[1]], getConfusionMatrix(dt.Prediction[,2])$overall[[1]])
Sensitivity<-c(getConfusionMatrix(glm.Prediction)$byClass[[1]], getConfusionMatrix(nb.Prediction[, 2])$byClass[[1]], getConfusionMatrix(nn.Prediction)$byClass[[1]], getConfusionMatrix(rf.BoolPrediction)$byClass[[1]], getConfusionMatrix(dt.Prediction[,2])$byClass[[1]])
Specifity<-c(getConfusionMatrix(glm.Prediction)$byClass[[2]], getConfusionMatrix(nb.Prediction[, 2])$byClass[[2]], getConfusionMatrix(nn.Prediction)$byClass[[2]], getConfusionMatrix(rf.BoolPrediction)$byClass[[2]], getConfusionMatrix(dt.Prediction[,2])$byClass[[2]])

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























#----------------------------------
#Read the spambase data
data_spambase <- read.table("data_spambase.txt", header=FALSE, sep=",")
#classes y - last column. 
dim(data_spambase)
head(data_spambase)

#-----------------------------------
#Read the tic-tac-toe data
tic_tac_toe_game <- read.table("tic_tac_toe_game.txt", header=FALSE, sep=",")
#classes y - last column. 
dim(tic_tac_toe_game)
head(tic_tac_toe_game)
