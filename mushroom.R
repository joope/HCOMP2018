#https://rpubs.com/hcrews47/mushroom
#Mushroom data contains observations from 23 different mushrooms

#load mushroom data
mushrooms <- read.csv("semma_project/agaricus-lepiota.data.txt")
#remove rows including ? values
mushrooms[mushrooms=="?"] <-NA
mushrooms <- mushrooms[complete.cases(mushrooms), ]

#export labels from the data
mushrooms$class = sapply(mushrooms$p, function(x){ifelse(x=='e', 'edible', 'poisonous')})
expertlabel <- mushrooms$class
mushrooms$p <- NULL
head(mushrooms)
#TODO: generate multiple labels + majority vote

#create test and train sets with 70/30 ratio
smp_size <- floor(0.70 * nrow(mushrooms))

## set the seed for reproductibility
set.seed(123)
train_ind <- sample(seq_len(nrow(mushrooms)), size = smp_size)
train <- mushrooms[train_ind, ]
test <- mushrooms[-train_ind, ]

head(train)

#desicion tree
library(rpart)
#train tree
tree = rpart(class~., data=train, control = rpart.control(cp = .0005)) 
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
#svm_model <- svm(class~., data=train, type='C-classification', kernel='radial')
#pred_train <-predict(svm_model,train) 
#mean(pred_train==train$class) 