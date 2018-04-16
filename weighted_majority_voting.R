
weighted_majority_voting <- function(labelset)
{
  # all labels given by different workers in matrix
  dataset <- labelset
  rows <- length(dataset[,1])
  cols <- length(dataset[1,])
  datatype <- typeof(dataset[1, 1])
  
  # vector for final labels (consensus labels)
  labels <- vector(datatype, length = rows)
  # vector of zeros is initialised for worker scores
  scores <- rep(0, cols)
  # vector for all different possible labels
  labelspace <- vector()
  
  # library for general majority voting
  library(mclust)
  
  # score workers based on agreement with majority
  for (i in 1:rows){
  # get majority vote   
    freq <- (majorityVote(dataset[i,]))$majority
    for (j in 1:cols){
      # see if worker voted for the most frequent label
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
  
  # copy counted scores
  sortedscores <- scores 
  # sort scores
  sort(sortedscores)
  
  # choose threshold using appropriate column from sorted scores
  expert_threshold <- sortedscores[as.integer(cols-(2*(cols/3)))]
  # choose another threshold
  spammer_threshold <- sortedscores[as.integer(cols-(cols/3))]
  # amount of labels:
  labelcount <- length(labelspace)
  
  # majority vote with weights
  for (i in 1:rows){
    # initialise vector for giving scores for labels
    labelscores <- rep(0, labelcount)
    for (j in 1:cols){
      # determine weight based on worker scores and thresholds
      weight <- 1
      if(scores[j]<expert_threshold){
        weight <- weight - 0.4
      }
      if(scores[j]<spammer_threshold){
        weight <- 0
      }
      # find label's index in labelspace
      index <- match(dataset[i, j], labelspace)
      # increment labels weight pointed by index
      labelscores[index] = labelscores[index] + weight 
    }
    best <- 0
    freqind <- 1
    # iterate labels to see which has biggest weight
    for (k in 1:labelcount){
      if(labelscores[k]>best){
        freqind <- k
        best <- labelscores[k]
      }
    }
    # choose the label with biggest weight as final label
    labels[i] <- labelspace[freqind]
  }
  
  # return final labels
  return(labels)
}
