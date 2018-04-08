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
#1=e, 2=p
mushrooms_Dexp=data.table(ID=c(1:N), label=c(data_mushrooms_KAGGLE[,1]))
head(mushrooms_Dexp)

#SIMULATE 30% NOISE
#rbinom(n, size, prob): n - number of observations, 
#size - number of trials, prob - prob of success of each trial
#30% noise
nois = rbinom(N,1,0.3)
label = mushrooms_Dexp$label
label[mushrooms_Dexp$label==1 & nois] = 2
label[mushrooms_Dexp$label==2 & nois] = 1

#data table for Worker_1:
mushrooms_D_worker_1=data.table(ID=c(1:N), ID_worker=rep(1,N), label=label) 

#SIMULATE NOISY DATA (30% noise)  FOR 10 WORKERS. 
workers_l<-matrix(NA,nrow=N, ncol=10)
#i = number of workers =10
for (i in 1:10) {
  nois = rbinom(N,1,0.3)
  lbs = mushrooms_Dexp$label
  lbs[mushrooms_Dexp$label==1 & nois]=2
  lbs[mushrooms_Dexp$label==2 & nois] = 1
  workers_l[,i]=lbs
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
  consensus[i]<-(majorityVote(labels_All[i,]))$majority
}

head(consensus)

consensus<-as.factor(consensus)
consensus = sapply(consensus, function(x){ifelse(x== 2, 'p', 'e')})
consensus<-as.factor(consensus)

head(consensus)
#consensus==data_mushrooms_KAGGLE[,1] - not always true. 

#DATA PARTITIONING: SEPARATE ORIGINAL AND CONSENSUS DATA TO TRAIN AND TEST DATA
#Splitting of these data sets should be randomly done.
#The function to partition the data takes the data frame and a decimal between 0 and 1 
#that represents the fraction of data to dedicate for the training data as parameters.

set.seed(1234)
#replace the firts column of original data with the consensus labels
dataConsensus<-cbind(consensus, data_mushrooms_KAGGLE[,-1])
names(dataConsensus)[names(dataConsensus) == "consensus"] = "class" 
head(dataConsensus)

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
schema        <- class ~ odor + spore.print.color
actualResults <- ifelse( testData$class == "p", TRUE, FALSE )

#construct confusion matrix
getConfusionMatrix <- function( probabilityOfTests, actuals = actualResults, threshold = 0.4 )
{
  predictions    <- ifelse ( probabilityOfTests > threshold, 'p', 'e' )
  confusionMatrix( as.factor(testData$class), as.factor(predictions) )
}

#plot ROC curve
plotROCCurve <- function( predictionResults, title, color = "red" )
{
  plot( roc( testData$class, predictionResults, direction="<" ), 
        print.auc=TRUE, col = color, lwd = 3, main = title )
}


#LOGISTIC REGRESSION
glm.Model         <- glm( schema, data = trainData, family = "binomial" )
glm.Prediction    <- predict( glm.Model, testData, type = "response" )

print( getConfusionMatrix( glm.Prediction ))
plotROCCurve( glm.Prediction, "ROC Curve for Logistical Regression")


#NAIVE BAYES CLASSIFICATOR
library( e1071 )
nb.Model         <- naiveBayes( schema, data = trainData )
nb.Prediction    <- predict( nb.Model, newdata = testData, type = "raw" )

print( getConfusionMatrix( nb.Prediction[, 2] ))
plotROCCurve( nb.Prediction[, 2], "ROC Curve for Naive Bayesian Classification")

#Classification using Neural Nets
library( nnet )

nn.Model      <- nnet( schema, data = trainData, size = 10, maxit = 200 )
nn.Prediction <- predict( nn.Model, newdata = testData, type = "raw")


print( getConfusionMatrix( nn.Prediction ))
plotROCCurve( nn.Prediction, "ROC Curve for Neural Nets")

#Classification using Random Forest
library( randomForest )

rf.Model          <- randomForest( schema, data = trainData )
rf.Prediction     <- predict( rf.Model, newdata = testData )
rf.BoolPrediction <- rf.Prediction == "p"

print( getConfusionMatrix( rf.BoolPrediction ))
plotROCCurve( as.numeric( rf.Prediction == "p"), "ROC Curve for Random Forest")

#Classification using Decision Tree
library(rpart)
dt.Model      <- rpart( schema, data = trainData )
dt.Prediction <- predict( dt.Model, newdata = testData )

print( getConfusionMatrix( dt.Prediction[, 2] ))
plotROCCurve( dt.Prediction[, 2], "ROC Curve for Decision Tree")


#plot table with classification results

#modelsPredictions<-c(glm.Prediction, nb.Prediction[, 2], nn.Prediction, rf.BoolPrediction, dt.Prediction[,2])
#It does not work because the elements of modelsPredictions[i] are numerical values, not objects. 
#Accuracy<-sapply(modelsPredictions, function(x) getConfusionMatrix(x)$overall[[1]]) 

Accuracy<-c(getConfusionMatrix(glm.Prediction)$overall[[1]], getConfusionMatrix(nb.Prediction[, 2])$overall[[1]], getConfusionMatrix(nn.Prediction)$overall[[1]], getConfusionMatrix(rf.BoolPrediction)$overall[[1]], getConfusionMatrix(dt.Prediction[,2])$overall[[1]])
Sensitivity<-c(getConfusionMatrix(glm.Prediction)$byClass[[1]], getConfusionMatrix(nb.Prediction[, 2])$byClass[[1]], getConfusionMatrix(nn.Prediction)$byClass[[1]], getConfusionMatrix(rf.BoolPrediction)$byClass[[1]], getConfusionMatrix(dt.Prediction[,2])$byClass[[1]])
Specifity<-c(getConfusionMatrix(glm.Prediction)$byClass[[2]], getConfusionMatrix(nb.Prediction[, 2])$byClass[[2]], getConfusionMatrix(nn.Prediction)$byClass[[2]], getConfusionMatrix(rf.BoolPrediction)$byClass[[2]], getConfusionMatrix(dt.Prediction[,2])$byClass[[2]])

classificationResults<-data.frame(Accuracy, Sensitivity, Specifity)
row.names(classificationResults)<-c("Logistic Regression", "Naive Bayes", "Neural Nets", "Random Forest", "Decision Tree")
classificationResults

#Plot ROC curves
glmRoc <- roc( testData$class, glm.Prediction, direction="<" )
nbRoc  <- roc( testData$class, nb.Prediction[, 2], direction="<" )
nnRoc  <- roc( testData$class, nn.Prediction, direction="<" )
rfRoc  <- roc( testData$class, as.numeric( rf.Prediction == "p"), direction="<" )
dtRoc  <- roc( testData$class, dt.Prediction[, 2], direction="<" )

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
