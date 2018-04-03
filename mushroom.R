#https://rpubs.com/hcrews47/mushroom
#Mushroom data contains observations from 23 different mushrooms

getCleanMushroomData <- function(mushrooms){
  mushrooms <- read.csv("./agaricus-lepiota.data.txt")
  #remove rows including ? values
  mushrooms[mushrooms=="?"] <-NA
  mushrooms <- mushrooms[complete.cases(mushrooms), ]
  
  #export labels from the data, 1 if edible, 0 if poisonous
  mushrooms$class = sapply(mushrooms$p, function(x){ifelse(x=='e', 1, 0)})
  mushrooms$p <- NULL
  return(mushrooms)
}


simulateNoisyDataset <- function(dataset, workerAccuracy){
  simulated = dataset
  simulated$noise <- rbinom(nrow(dataset), 1, workerAccuracy)
  
  # Simulate noise to classes. If noise == 0 swap classes
  simulated$class[dataset$class == 1 & simulated$noise == 0] = 0
  simulated$class[dataset$class == 0 & simulated$noise == 0] = 1
  simulated$class[dataset$class == 1 & simulated$noise == 1] = 1
  simulated$class[dataset$class == 0 & simulated$noise == 1] = 0
  
  simulated$class = factor(simulated$class)
  simulated$noise <- NULL
  
  return(simulated)
}

data <- getCleanMushroomData()

# create test and train sets with some ratio
smp_size <- floor(0.7 * nrow(dataset))

# set the seed for reproductibility
set.seed(123)
train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
train <- dataset[train_ind, ]
test <- dataset[-train_ind, ]

trainSimulated <- simulateNoisyDataset(train, 0.95)

# Visualize simulated errors
ifelse(train$class == trainSimulated$class, 1, 0)

#desicion tree
library(rpart)
#train tree
tree = rpart(class~., data=trainSimulated, control = rpart.control(cp = .0005)) 
#test prediction with test data set
tree_pred = predict(tree, test, type='class')
mean(tree_pred==test$class)
table(tree_pred, test$class)

#prediction with original data set
tree_pred_full = predict(tree, mushrooms, type='class')
mean(tree_pred_full==mushrooms$class)
table(tree_pred_full, mushrooms$class)

##kNN
library(class)
#not working at the moment
#knn(train, test, train$class, k=3)

#SVM
library(e1071)
#not working at the moment
svm_model <- svm(class~., data=train, type='C-classification', kernel='radial')
pred_train <-predict(svm_model,train) 
mean(pred_train==train$class) 