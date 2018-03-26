#Ex. 1
library(glmnet)
#Read the mushrooms data
data_mushrooms <- read.table("data_mushrooms.txt", header=FALSE, sep=",")
#clases y - first column. 
dim(data_mushrooms)
head(data_mushrooms)

#--------------------------------------
#Data preprocessing
#1. Dexp = (ID, labels); labels = (1,2) 1-eatable, 2 - poisonous

#change levels of first column to e=0, p=1. 
#levels(data_mushrooms[,1])<-c(0,1) #'0','1'
#setattr(data_mushrooms[,1],"levels",c(0,1)) #c(0,1)

library(data.table)
N<-length(data_mushrooms[,1])
#1=e, 2=p
mushrooms_Dexp=data.table(ID=c(1:N), label=c(data_mushrooms[,1]))
head(mushrooms_Dexp)

#simulate 30% noise
#rbinom(n, size, prob): n - number of observations, 
#size - number of trials, prob - prob of success of each trial
nois = rbinom(N,1,0.3)
lbl = mushrooms_Dexp$label
lbl[mushrooms_Dexp$label==1 & nois] = 2
lbl[mushrooms_Dexp$label==2 & nois] = 1

#data table for Worker_1:
mushrooms_D_worker_1=data.table(ID=c(1:N), ID_worker=rep(1,N), label=lbl) 

#simulate noisy data, 30% noise for 10 workers. 
workers_l<-matrix(NA,nrow=N, ncol=10)
#i = number of workers
for (i in 1:10) {
  nois = rbinom(N,1,0.3)
  lbs = mushrooms_Dexp$label
  lbs[mushrooms_Dexp$label==1 & nois]=2
  lbs[mushrooms_Dexp$label==2 & nois] = 1
  workers_l[,i]=lbs
}

#data table = (ID, Dexp, w1,w2....... w10)
#data table with ID, Dexp labels and 10 workers labels with 30% noise.
mushrooms_Dexp_Crowd=data.table(ID=c(1:N), label=c(data_mushrooms[,1]), workers_l)
head(mushrooms_Dexp_Crowd)

#majority voting
#library for majority voting
library(mclust) 
#matrix only with the labels, without IDs
labels_All<-cbind(data_mushrooms[,1], workers_l)
 
#majority vote has 3 attributes, I take only majority label
#con<-rep(0, times=N)
for (i in 1:N) {
  con[i]<-(majorityVote(as.matrix(mushrooms_Dexp_Crowd[i,-1])))$majority  
}

#the same, but directly with matrix, not data frame to matrix and without IDs. 
 for (i in 1:N) {
   consensus[i]<-(majorityVote(labels_All[i,]))$majority
 }
 
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
