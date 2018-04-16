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

#function to simulate mixed noisy labels
#true experts 5% - 1/10, 
#experts 20% noise - 3/10 
#amateurs, 40% noise - 3/10, 
#and 3/10 adversaries aka random labels. 
#takes only values like 10, 20, 30 etc
simulateNoisyWorkersMixedPopulation <- function(amount_of_workers, expertlabel){
  amount_of_workers <- c(0.1,0.3,0.3,0.3)*amount_of_workers
  amount_of_noise <- c(0.05,0.2,0.4,0.75)

  mixed_population <- NA
  t_experts <- simulateNoisyWorkers(amount_of_workers[1],amount_of_noise[1],expertlabel)
  experts <- simulateNoisyWorkers(amount_of_workers[2],amount_of_noise[2],expertlabel)
  mixed_population <- cbind(t_experts, experts)
  amateurs <- simulateNoisyWorkers(amount_of_workers[2],amount_of_noise[3],expertlabel)
  mixed_population <- cbind(mixed_population, amateurs)
  adversasiers <- generateRandomLabels(amount_of_workers[4],expertlabel)
  mixed_population <- cbind(mixed_population, adversasiers)
  
  return(mixed_population)
}


generateRandomLabels <- function(amount_of_workers,expertlabel){
  classes <- unique(expertlabel)
  N <- length(expertlabel)
  workers_with_noisy_labels<-matrix(NA,nrow=N, ncol=amount_of_workers)
  for (i in 1:amount_of_workers) {
    workers_with_noisy_labels[,i] <- sample(1:tail(classes,1), N,T)
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
mushrooms_original <- read.csv("mushroomsKAGGLE.csv")


N<-nrow(mushrooms_original)
#head(mushrooms_original)
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
noise_level <- 0.50 # x% amount incorret labels
#simulate multiple labels with noive level x%
data1_workers_labels_testData <-simulateNoisyWorkers(amount_of_workers,noise_level,expertlabels_testData)
#data1_workers_labels_testData <- simulateNoisyWorkersMixedPopulation(amount_of_workers,expertlabels_testData)

#majorityvoting for noisy labels
data1_labels_majorityvoting <- majorityVotingForLabels(data1_workers_labels_testData, testData)
trainData <- data1_labels_majorityvoting

#calculate the amount of noise in majority voted labels
res <- data1_labels_majorityvoting$class == expertlabels_testData
length(res[res==TRUE])/(length(res))


#RUN 3 DIFFERENT CLASSIFICATION ALGORITHMS
#install.packages("e1071")
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

#-------------------------------------

#Plot the predictions and the plot to see our model fit
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(0,0,xlab="workers", ylab="accuracy",xlim = c(0,50),ylim = c(0.3,1),type = "n")
cl <- rainbow(5)

y3to30 <- c(3,5,10,20,30)
y3to50 <- c(3,5,10,20,30,40,50)

#30% noise
accuracy30 <- c(0.9941545,0.9941545,0.9941545,0.9941545,0.9941545)
lines(y3to30,accuracy30,col = cl[1],type = 'b')

#40% noise
accuracy40 <- c(0.9895616,0.9941545,0.9941545,0.9941545,0.9941545)
lines(y3to30,accuracy40,col = cl[2],type = 'b')

#45% noise
accuracy45 <- c(0.9812109,0.9707724,0.9837161,0.9837161,0.9837161)
lines(y3to30,accuracy45,col = cl[3],type = 'b')

#49 noise
accuracy49 <- c(0.593737,0.6772443,0.8964509,0.8939457,0.9177453,0.9453027,0.9453027)
lines(y3to50,accuracy49,col = cl[4],type = 'b')

#50% noise
accuracy50 <- c(0.3139875,0.5244259,0.514405,0.3348643,0.406263,0.8617954,0.34238)
lines(y3to50,accuracy50,col = cl[5],type = 'b')

#par(xpd=TRUE)
legend("topright", inset=c(-0.3,0), legend=c("30%", "40%","45%","49%","50%"),
       col=cl, lty=1:2, cex=0.8)

#load 2nd data
#library(datasets)
#data(iris)
#head(iris)
#data2 <- iris

#iris_flowers=data.table(ID=c(1:nrow(iris)), label=c(iris$Species))
#data2_expertlabels <- iris_flowers$label

#load 3rd data
#TODO

