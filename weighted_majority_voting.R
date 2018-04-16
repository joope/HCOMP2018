
weighted_majority_voting <- function(dataset)
{
  dataset <- dataset
  rows <- length(dataset[,i])
  cols <- length(dataset[i,])
  labels <- vector(rows)
  # list of zeros is initialised for worker scores
  scores <- list(rep(0,cols))
  # vector for all different possible labels
  labelspace <- vector()
  
  # library for general majority voting
  library(mclust)
  
  # score workers based on agreement with majority
  for (i in 1:rows){
  # get majority vote   
    freq <- (majorityVote(dataset[i,]))$majority
    for (j in 1:cols){
      # see if worker voted the most frequent label
      if (identical(dataset[i, j], freq)){
        # increment score if true
        scores[j] = scores[j] + 1
      }
      # collect unseen labels to labelspace vector
      if(!(dataset[i, j] %in% labelspace)){
        append(labelspace, dataset[i, j])
      }
    }
  }
  
  sortedscores <- scores 
  # sort scores
  sort(sortedscores)
  
  # choose threshold using appropriate column from sorted scores
  expert_threshold <- sortedscores[as.integer(cols-(2*(cols/3)))]
  # choose another threshold
  spammer_threshold <- sortedscores[as.integer(cols-(cols/3))]
  
  # majority vote with weights
  for (i in 1:rows){
    # initialise vector for giving scores for labels
    labelscores <- vector(length(labelspace))
    for (j in 1:cols){
      # determine weight based on worker scores
      weight <- 1
      if(scores[j]<expert_threshold){
        weight <- weight - 0.7
      }if(scores[j]<spammer_threshold){
        weight <- 0
      }
      # find labels index in labelspace
      index <- match(dataset[i, j], labelspace)
      # increment labels weight ponted by index
      labelscores[index] = labelscores[index] + weight 
    }
    best <- 0
    freq <- 1
    # iterate labels to see which has biggest weight
    for (k in 1:length(labelscores)){
      if(labelscores[k]>best){
        freq <- k
        best <- labelscores[k]
      }
    }
    # choose the label with biggest weight
    labels[i] <- labelspace[freq]
  }
  
  return(labels)
  
}
