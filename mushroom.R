## Functions

#SIMULATE NOISY DATA with X% noise FOR N WORKERS. 

#function to simulate noisy labels
simulateNoisyWorkers <- function(amount_of_workers, amount_of_noise, expertlabel){
  label_values <- unique(expertlabel)
  N=length(expertlabel)
  workers_with_noisy_labels<-matrix(NA,nrow=N, ncol=amount_of_workers)
  for (i in 1:amount_of_workers) {
    #generates which labels are noisy for each worker
    noise <- rbinom(N,1,amount_of_noise) # 0,1,1 etc
    labels <- expertlabel
    for(j in 1:N){
      # 1 == label with noise
      if(noise[j] == 1){
        #replace the chosen noisy labels with incorret values
        wrong_labels_for_row_j <- label_values[which(label_values != expertlabel[j])]
        labels[j]<- wrong_labels_for_row_j[sample(1:length(wrong_labels_for_row_j),1)]
      }
    }
    #save noisy labels to matrix
    workers_with_noisy_labels[,i] <- labels
  }
  
  return(workers_with_noisy_labels)
}


#------------------------------

#CONSENSUS ALG 1: MAJOTRITY VOTING to reduce multiple labels
#install.packages("mclust")
library(mclust) 

# apply the majority voting to worker labels
# cbind majority votes to data 
# data  cant have the original labels anymore
majorityVotingForLabels <- function(workers_labels,data){
  N <- nrow(workers_labels)
  labels_majorityvoting<-rep(0, times=N)
  for (i in 1:N) {
    labels_majorityvoting[i]<-(majorityVote(workers_labels[i,]))$majority
  }
  labels_majorityvoting <- as.factor(labels_majorityvoting)
  labels_majorityvoting<-cbind(labels_majorityvoting, data)
  names(labels_majorityvoting)[names(labels_majorityvoting) == "labels_majorityvoting"] = "class" 
  return(labels_majorityvoting)
}  

#CONSENSUS ALG 2: USE GOLDEN LABELS to reduce multiple labels with majority voting
goldenLabelCheckForLabels <- function(workers_labels){
  #TODO
  return(NULL)
}



#------------------------------

#PARTITION TO TEST AND TRAIN DATA

set.seed(1234)

fractionOfDataForTraining = 0.7 #70% train and 30% test data. 

partitionData <- function( data, fractionOfDataForTraining, expertlabels){
  numberOfRows <- nrow( data )
  randomRows   <- runif(numberOfRows)
  #print(randomRows)
  flag         <- randomRows <= fractionOfDataForTraining
  trainData <- data[ flag, ]
  testData  <- data[ !flag, ]
  dataSetSplit <- list( trainData = trainData, testData = testData, trainDataLabels=expertlabels[flag],testDataLabels=expertlabels[!flag] )
  dataSetSplit
}

#-----------------------------------

#https://rpubs.com/hcrews47/mushroom
#Mushroom data contains observations from 23 different mushrooms


# load mushroom data
mushrooms_original <- read.csv("mushroom.csv")

N<-nrow(mushrooms_original)
head(mushrooms_original)
data1 <- mushrooms_original
library(data.table)
data1_expertlabels <- data.table(ID=c(1:N), label=c(mushrooms_original$class))
data1_expertlabels <- data1_expertlabels$label


#partition for mushroom data without classes eatable/poisonous
paritionedData <- partitionData(data1[,-1], fractionOfDataForTraining,data1_expertlabels)
trainData   <- paritionedData$trainData
testData    <- paritionedData$testData
expertlabels_trainData <- paritionedData$trainDataLabels
expertlabels_testData <- paritionedData$testDataLabels


#Add noise to train data labels
amount_of_workers <- 20
noise_level <- 0.4 # x% amount incorret labels
#simulate multiple labels with noive level x%
data1_workers_labels_testData <-simulateNoisyWorkers(amount_of_workers,noise_level,expertlabels_testData)
#majorityvoting for noisy labels
data1_labels_majorityvoting <- majorityVotingForLabels(data1_workers_labels_testData, testData)
trainData <- data1_labels_majorityvoting

#calculate the amount of noise in majority voted labels
res <- data1_labels_majorityvoting$class == expertlabels_testData
length(res[res==TRUE])/(length(res))


#RUN 3 DIFFERENT CLASSIFICATION ALGORITHMS

library(e1071)

#Fit a model and support vector machine prediction
schema <- class ~ odor + spore.print.color
model_svm <- svm(schema , trainData)
pred <- predict(model_svm, testData)

#calculate correct predictions  
res <- as.numeric(pred) == expertlabels_testData
length(res[res==TRUE])/(length(res))

predictions_for_majorityvoted_labels <- sapply(as.numeric(pred), function(x){ifelse(x== "2", 'p', 'e')})
expertlabels_testData <- sapply(expertlabels_testData, function(x){ifelse(x== "2", 'p', 'e')})
table(predictions_for_majorityvoted_labels,expertlabels_testData)

#with 100% correct labels accuracy is 0.8548065

#Plot the predictions and the plot to see our model fit

#points(trainData$class, pred, col = "blue", pch=4)





#load 2nd data
#library(datasets)
#data(iris)
#head(iris)
#data2 <- iris

#iris_flowers=data.table(ID=c(1:nrow(iris)), label=c(iris$Species))
#data2_expertlabels <- iris_flowers$label

#load 3rd data
#TODO

