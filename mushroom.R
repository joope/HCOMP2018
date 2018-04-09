#https://rpubs.com/hcrews47/mushroom
#Mushroom data contains observations from 23 different mushrooms


# load mushroom data
mushrooms_original <- read.csv("data/mushroom.csv")
library(data.table)
N<-nrow(mushrooms_original)
head(mushrooms_original)
data1 <- mushrooms_original

mushrooms=data.table(ID=c(1:N), label=c(mushrooms_original$class))
data1_expertlabels <- mushrooms$label

#load 2n data
library(datasets)
data(iris)
head(iris)
data2 <- iris

iris_flowers=data.table(ID=c(1:nrow(iris)), label=c(iris$Species))
data2_expertlabels <- iris_flowers$label

unique(data2_expertlabels)

#load 3rd data
#TODO


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
      # 1 == noisy label
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


data1_workers_labels<-simulateNoisyWorkers(1,0.7,data1_expertlabels)
data2_workers_labels<-simulateNoisyWorkers(1,0.7,data2_expertlabels)

#confirm 70% labels are indetical with expert labels
res <- data1_workers_labels == data1_expertlabels
length(res[res==FALSE])/(ncol(res)*nrow(res))

res <- data2_workers_labels == data2_expertlabels
length(res[res==FALSE])/(ncol(res)*nrow(res))

#------------------------------

#CONSENSUS ALG 1: MAJOTRITY VOTING to reduce multiple labels
#install.packages("mclust")
library(mclust) 

# apply the majority voting to worker labels
majorityVotingForLabels <- function(workers_labels){
  N <- nrow(workers_labels)
  labels_majorityvoting<-rep(0, times=N)
  for (i in 1:N) {
    labels_majorityvoting[i]<-(majorityVote(workers_labels[i,]))$majority
  }
  return(labels_majorityvoting)
}  

#CONSENSUS ALG 2: USE GOLDEN LABELS to reduce multiple labels with majority voting

goldenLabelCheckForLabels <- function(workers_labels){
  #TODO
  return(NULL)
}


#combine majority voted labels with original mushroom data
data1_labels_majorityvoting <- majorityVotingForLabels(data1_workers_labels)
data1_labels_majorityvoting <- sapply(data1_labels_majorityvoting, function(x){ifelse(x== "2", 'p', 'e')})
data1_labels_majorityvoting <- as.factor(data1_labels_majorityvoting)
mushrooms_majorityvotinglabels<-cbind(data1_labels_majorityvoting, data1[,-1])
names(mushrooms_majorityvotinglabels)[names(mushrooms_majorityvotinglabels) == "data1_labels_majorityvoting"] = "class" 

#combine majority voted labels with original iris data
data2_labels_majorityvoting <- majorityVotingForLabels(data2_workers_labels)
data2_labels_majorityvoting <- as.factor(data2_labels_majorityvoting)
iris_majorityvotinglabels<-cbind(data2_labels_majorityvoting, data2[,-5])
names(iris_majorityvotinglabels)[names(iris_majorityvotinglabels) == "data2_labels_majorityvoting"] = "class" 



#------------------------------

#PARTITION TO TEST AND TRAIN DATA

#-----------------

#RUN 3 DIFFERENT CLASSIFICATION ALGORITHMS




