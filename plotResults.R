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

fractionOfDataForTraining = 0.3 #70% train and 30% test data. 

partitionData <- function( data, fractionOfDataForTraining, expertlabels){
  data$class <- NULL
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

getAccuracy <- function(original_data, amount_of_workers, noise_level) {
  N<-nrow(original_data)
  #head(original_data)
  data1 <- original_data
  library(data.table)
  data1_expertlabels <- data.table(ID=c(1:N), label=c(original_data$class))
  data1_expertlabels <- data1_expertlabels$label
  
  
  #partition for mushroom data without classes eatable/poisonous
  paritionedData <- partitionData(data1, fractionOfDataForTraining,data1_expertlabels)
  trainData   <- paritionedData$trainData
  testData    <- paritionedData$testData
  expertlabels_trainData <- paritionedData$trainDataLabels
  expertlabels_testData <- paritionedData$testDataLabels
  
  #Add noise to train data labels

  #simulate multiple labels with noive level x%
  data1_workers_labels_testData <-simulateNoisyWorkers(amount_of_workers,noise_level,expertlabels_testData)
  #data1_workers_labels_testData <- simulateNoisyWorkersMixedPopulation(amount_of_workers,expertlabels_testData)
  
  #majorityvoting for noisy labels
  data1_labels_majorityvoting <- majorityVotingForLabels(data1_workers_labels_testData, testData)
  testData <- data1_labels_majorityvoting
  
  #calculate the amount of noise in majority voted labels
  res <- data1_labels_majorityvoting$class == expertlabels_testData
  length(res[res==TRUE])/(length(res))
  
  
  #RUN 3 DIFFERENT CLASSIFICATION ALGORITHMS
  #install.packages("e1071")
  library(e1071)
  
  #Fit a model and support vector machine prediction
  schema <- class ~ .
  nrow(trainData)
  nrow(testData)
  model_svm <- svm(schema , testData)
  pred <- predict(model_svm, trainData)
  
  #calculate correct predictions  
  res <- as.numeric(pred) == expertlabels_trainData
  length(res[res==TRUE])/(length(res))
}


# load spam data
# original_data <- read.csv('data_spambase.txt')
# original_data$class <- as.factor(original_data$X1)
# original_data$X1 <- NULL

# load mushroom data
# original_data <- read.csv("mushroomsKAGGLE.csv")

# load tic-tac-toe data
original_data <- read.csv('tic_tac_toe_game.txt')
original_data$class <- as.factor(original_data$positive)
original_data$positive <- NULL

amount_of_workers <- c(1, 5, 10, 15, 20, 25, 30)
noise_level <- c(0.1, 0.2, 0.3, 0.4, 0.5) # x% amount incorret labels
results <- matrix(nrow=7, ncol = 5)

for (r in 1:length(amount_of_workers))
  for (c in 1:length(noise_level))
    results[[r, c]] <- getAccuracy(original_data, amount_of_workers[r], noise_level[c])

results


#Plot the predictions and the plot to see our model fit
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(0,0,xlab="workers", ylab="accuracy",xlim = c(0,35),ylim = c(0.3,1),type = "n")
cl <- rainbow(7)

for (r in 1:nrow(results))
  lines(amount_of_workers, results[,r], col = cl[r])

#par(xpd=TRUE)
legend("topright", inset=c(-0.3,0), legend=c("10%", "20%","30%","40%","50%"),
       col=cl, lty=1:2, cex=0.8)

