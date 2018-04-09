#https://rpubs.com/hcrews47/mushroom
#Mushroom data contains observations from 23 different mushrooms


# load mushroom data
mushrooms_original <- read.csv("data/mushroom.csv")
library(data.table)
N<-length(mushrooms_original[,1])
head(mushrooms_original)

#load 2n data

#load 3rd data

#DATA PREPROCESSING FOR CREATING MULTIPLE WORKERS
# IDs ; labels = (1,2) 1-eatable, 2 - poisonous

mushrooms=data.table(ID=c(1:N), label=c(mushrooms_original$class))
head(mushrooms)
mushrooms$label

#SIMULATE NOISY DATA (30% noise) FOR 10 WORKERS. 

#function to simulate noisy labels
simulateNoisyWorkers <- function(amount_of_workers, amount_of_noise, expertlabel){
  workers_with_noisy_labels<-matrix(NA,nrow=N, ncol=amount_of_workers)
  for (i in 1:amount_of_workers) {
    noise <- rbinom(N,1,amount_of_noise)
    labels <- expertlabel
    labels[expertlabel==1 & noise] <- 2
    labels[expertlabel==2 & noise] <- 1
    workers_with_noisy_labels[,i] <- labels
  }
  return(workers_with_noisy_labels)
}


workers_labels<-simulateNoisyWorkers(10,0.3,mushrooms$label)

#confirm 70% labels are indetical with expert labels
res <- workers_labels == mushrooms$label
length(res[res==TRUE])/(10*nrow(res))

#------------------------------

#MAJOTRITY VOTING to reduce multiple labels
#install.packages("mclust")
library(mclust) 

# apply the majority voting to worker labels
majorityVotingForLabels <- function(workers_labels){
  N <- nrow(workers_labels)
  labels_majorityvoting<-rep(0, times=N)
  for (i in 1:N) {
    labels_majorityvoting[i]<-(majorityVote(workers_labels[i,]))$majority
  }
  
  #combine majority voted labels with original mushroom data
  labels_majorityvoting <- sapply(labels_majorityvoting, function(x){ifelse(x== "2", 'p', 'e')})
  labels_majorityvoting <- as.factor(labels_majorityvoting)
  
  return(labels_majorityvoting)
}  

labels_majorityvoting <- majorityVotingForLabels(workers_labels)
mushrooms_majorityvotinglabels<-cbind(labels_majorityvoting, mushrooms_original[,-1])
head(mushrooms_majorityvotinglabels)
names(mushrooms_majorityvotinglabels)[names(mushrooms_majorityvotinglabels) == "labels_majorityvoting"] = "class" 

#USE GOLDEN LABELS to reduce multiple labels

goldenLabelCheckForLabels <- function(workers_labels){
  #TODO
  return(NULL)
}


#------------------------------

#PARTITION TO TEST AND TRAIN DATA

#-----------------

#RUN 3 DIFFERENT CLASSIFICATION ALGORITHMS



# simulateNoisyDataset <- function(dataset, workerAccuracy){
#  simulated = dataset
#  simulated$noise <- rbinom(nrow(dataset), 1, workerAccuracy)
  
  # Simulate noise to classes. If noise == 0 swap classes
#  simulated$class[dataset$class == 1 & simulated$noise == 0] = 0
#  simulated$class[dataset$class == 0 & simulated$noise == 0] = 1
#  simulated$class[dataset$class == 1 & simulated$noise == 1] = 1
#  simulated$class[dataset$class == 0 & simulated$noise == 1] = 0
  
#  simulated$class = factor(simulated$class)
#  simulated$noise <- NULL
  
#  return(simulated)
#}

