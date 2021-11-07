# Exercise 2: Classification
# Javier Galindos
rm(list=ls())

library("rstudioapi") 
library("dplyr") # Manipulating variables

# Set working directory
setwd(dirname(getActiveDocumentContext()$path)) 
getwd()  

# loading the data from file
load(file="Data/JGdata.RData")
#data plotting - visualization
plot(x[,1],x[,2], col=scales::alpha(labels+1,0.3), pch=20, main ='JG Dataset')

# KNN

# Load distance function
source("JG_distance.R")

# Distance metric
distFun= "Euclidean"

JG_knn<- function(testSet, trainSet, trainLabels, k){
  # Get the distance to k nearest neighbor of every point (knn_distance) 
  # and index of k-neighbors (neighbors)
  knn_distance <- matrix(,nrow = nrow(testSet)) # Create the array for the distance
  neighbors <- matrix(,nrow = nrow(testSet), ncol = k) # Array for index of neighbors
  for (i in seq(along=1:nrow(testSet))){
    temp_dist <- matrix(,nrow = nrow(trainSet))
    # Compute distance to each point
    for (j in seq(along=1:nrow(trainSet))){
      temp_dist[j] = JG_dist(testSet[i,], trainSet[j,], distFun)
    }
    # Take the k lowest distance
    temp_dist <- sort(temp_dist,index.return=TRUE)
    knn_distance[i] <- temp_dist$x[k]
    # Save the index of the k neighbors
    neighbors[i,] <- temp_dist$ix[1:k]
  }
  # Check the most frequent value of the neighbors
  testPredict <- matrix(,nrow = nrow(testSet))
  for (i in seq(along=1:nrow(testSet))){
    # Index of neighbors
    idx_i <- neighbors[i,]
    # Labels of these neighbors
    labels_i <- trainLabels[idx_i]
    # Get the most frequent neighbor
    unique_i <- unique(na.omit(labels_i))
    testPredict[i] <- unique_i[which.max(tabulate(match(labels_i, unique_i)))]
  }
  
  return (testPredict)
}

# Decision Tree (Works only with numeric data)
# Gini index for splitting criteria
# Dependent variable should be located in the last column (pre-process if not)
# [1]: https://machinelearningmastery.com/implement-decision-tree-algorithm-scratch-python/
# [2]: https://github.com/raxshah/Decision-Tree-R

JG_decisionTree<- function(testSet, trainSet, trainLabels){
  # Pre-proccess as data frame
  train <- cbind(trainSet,trainLabels)
  train <- as.data.frame(train)
  
  test <- as.data.frame(testSet)
  classColumNum <- ncol(train)
  
  # Gini index
  gini_index <- function(y){
    if(length(y) == 0) return(0)
    p <- table(y)/length(y)
    return (1-sum(p^2))
  }
  
  # Generate unique number to give an ID to each node of Decision Tree
  # Initialize ID
  idx <<- 3
  idGen <- function(){
    idx <<- idx + 4
    return (idx)
  }
  
  # Find the best split
  best_split <- function(train){
    classColumNum <- ncol(train)
    summaryColumnGini <- matrix(data= NA,ncol = 3,byrow = T)
    # Loop over independent variables
    for(col in 1:(ncol(train)-1)){
      # Compute mid-point vector between contiguous points
      colValue <- train[,col]
      colValue <- sort(colValue)
      colValue <- unique(colValue)
      meanPointVector <- rep(NA,length(colValue))
      
      for(i in 1:length(colValue)){
        # Add previous value if it is NA
        if(is.na(colValue[i+1]) == T){
          colValue[i+1] <- colValue[i]
        }
        meanPoint <- (colValue[i]+ colValue[i+1])/2
        meanPointVector[i] <- meanPoint
      }
      
      # Get rid off NA
      meanPointVector <- meanPointVector[!is.na(meanPointVector)]
      
      summaryGini <- matrix(data= NA,ncol = 2,byrow = T)
      
      # Loop over every raw != NA
      for(i in 1:length(meanPointVector)){
        totalRecords <- nrow(train)
        
        splitLeft <- train[train[,col] < meanPointVector[i],]
        splitRight <- train[train[,col] >= meanPointVector[i],]
        
        splitLeft.totalRecords <- nrow(splitLeft)
        splitRight.totalRecords <- nrow(splitRight)
        
        # Get Gini information
        if(splitLeft.totalRecords >0){
          splitLeft.gini <- gini_index(splitLeft[,classColumNum])
        }else{
          splitLeft.gini <- 0
        }
        
        if(splitRight.totalRecords >0){
          splitRight.gini <- gini_index(splitRight[,classColumNum])
        }else{
          splitRight.gini <- 0 
        }
        
        # Weighted mean of Gini Index
        gini_split <- as.double(splitLeft.gini * (splitLeft.totalRecords/totalRecords)) + as.double(splitRight.gini * (splitRight.totalRecords/totalRecords))
        
        # Add a little constant whether is 0
        if(splitLeft.gini == 0 | splitRight.gini == 0){
          gini_split <- gini_split + 0.01
        }
        
        summaryGini <- rbind(summaryGini,c(meanPointVector[i],gini_split))
      }
      
      # Get the splitting that yields to minimum Gini index
      summaryGini <- summaryGini[which.min(summaryGini[,2]),]
      splitValue <- summaryGini[1]
      giniValue <- summaryGini[2]
      
      summaryColumnGini <- rbind(summaryColumnGini,c(col,splitValue,giniValue))
    }
    
    # Get summary of each variable (column)
    summaryColumnGini <- summaryColumnGini[which.min(summaryColumnGini[,3]),]
    colNum <- summaryColumnGini[1]
    splitValue <- summaryColumnGini[2]
    giniValue <- summaryColumnGini[3]
    
    return(c(colNum,splitValue,giniValue))
    
  }
  
  # Global variables:
  sequ <<- 0
  nodeOwnNum <<- 0
  child0 <<- 1
  child1 <<- 2
  parent <<- 0
  
  summary_train_model <<- data.frame(NodeNum = NA, SplitColumn= NA, SplitValue=NA, ClassLabel= NA,NodeType = NA,Parent=NA,Child0= NA,Child1= NA,stringsAsFactors = F)
  
  growTree <- function(dataset,nodeOwnNum,sequ,parent,child0,child1){
    sequ <- sequ + 2
    
    dataset.classes <- nlevels(factor(dataset[,classColumNum]))
    
    # Check whether it is leaf(LN) or internal node (IN)
    # It is Leaf Node if there is only class in the subset
    if(dataset.classes == 1){ # Leaf node (LN)
      summary_train_model <<- rbind(summary_train_model,list(nodeOwnNum,NA,NA,dataset[1,classColumNum],"LN",parent,NA,NA))
    }
    else{ # Internal node (IN)
      dataset.splitSummary <- best_split(dataset)
      dataset.colNum <- dataset.splitSummary[1]
      dataset.splitValue <- dataset.splitSummary[2]
      summary_train_model <<- rbind(summary_train_model,list(nodeOwnNum,dataset.colNum,dataset.splitValue,NA,"IN",parent,child0,child1))
      
      # Split dataset with condition dataset[,dataset.colNum] < dataset.splitValue
      splitLeft <- dataset[dataset[,dataset.colNum] < dataset.splitValue,]
      splitRight <- dataset[dataset[,dataset.colNum] >= dataset.splitValue,]
      
      # Grow tree recursively
      if(!(nrow(splitLeft) == 0)){
        growTree(splitLeft,child0,sequ,nodeOwnNum,idGen(),idGen())
      }   
      
      if(!(nrow(splitRight) == 0)){
        growTree(splitRight,child1,sequ,nodeOwnNum,idGen(),idGen())
      }   
    }
  }
  
  growTree(train,nodeOwnNum,sequ,parent,child0,child1)
  summary_train_model <- summary_train_model[-1,]
  
  # Function to predict
  predict <- function(record,parent,nodenum){
    # Select the node -> nodenum
    node <- summary_train_model %>% filter(NodeNum == nodenum) %>%  select(NodeNum,SplitColumn,SplitValue,NodeType,ClassLabel,Parent,Child0,Child1) 
    
    # Check whether it is leaf(LN) or internal node (IN)
    if(node$NodeType == "LN"){
      # Return class if it is Leaf Node
      return(node$ClassLabel)
    }
    else{
      # Go through the tree if it is Interal Node
      split.column <- node$SplitColumn
      split.value <- node$SplitValue
      
      childNode <- NA
      if(record[,split.column] < split.value){
        childNode <- node$Child0
      }else{
        childNode <- node$Child1
      }
      
      parent <- node$Parent
      # Predict recursively
      predict(record,parent,childNode)
    }
  }
  
  df.predictedValue <<- data.frame(predictedValue = NA)
  for(n in 1:nrow(test)){
    pred.ClassLabel <-  predict(test[n,],0,0)
    df.predictedValue <- rbind(df.predictedValue,pred.ClassLabel)
  }
  pred <- df.predictedValue[-1,]
  
  return(pred)
}

# Goodness parameters
JG_accuracy <- function(yPred, yTrue){
  conf <- table(yPred, yTrue)
  accuracy <- sum(diag(conf))/sum(conf)
}

JG_precision <- function(yPred, yTrue){
  conf <- table(yPred, yTrue)
  prec <- matrix(,nrow = nrow(conf))
  for (i in seq(1:nrow(conf))){
    prec[i] <- conf[i,i] / sum (conf[i,])
  }
  return (prec)
}

JG_recall <- function(yPred, yTrue){
  conf <- table(yPred, yTrue)
  recall <- matrix(,nrow = nrow(conf))
  for (i in seq(1:nrow(conf))){
    recall[i] <- conf[i,i] / sum (conf[,i])
  }
  return (recall)
}

JG_F1score <- function(yPred, yTrue){
  conf <- table(yPred, yTrue)
  prec  <- matrix(,nrow = nrow(conf))
  recall <- matrix(,nrow = nrow(conf))
  F1score <- matrix(,nrow = nrow(conf))
  for (i in seq(1:nrow(conf))){
    prec[i] <- conf[i,i] / sum (conf[i,])
    recall[i] <- conf[i,i] / sum (conf[,i])
    F1score[i] <- 2 * prec[i] * recall[i] / (prec[i] + recall[i])
  }
  return (F1score)
}

# Data preparation. Split datatset
#split the data in proportion 70/30 for training and test purposes.
training_percentage = 70
sample_size <- floor((training_percentage/100) * nrow(x))
train_ind <- sample(seq_len(nrow(x)), size = sample_size)
trainSet <- x[train_ind, ]
trainLabels <- labels[train_ind]
testSet <- x[-train_ind, ]
testLabels <- labels[-train_ind]

# KNN
# Determined using Leave-one-out cross validation
k <- 5

# Prediction on test set
testPredict_knn <- JG_knn(testSet, trainSet, trainLabels, k)

# Performance
test_conf_knn <- table(testPredict_knn, testLabels)
test_conf_knn
test_accuracy_knn <- JG_accuracy(testPredict_knn,testLabels)
print("Accuracy in Test Set is:")
print(test_accuracy_knn)
test_precision_knn <- JG_precision(testPredict_knn,testLabels)
print("Precision in Test Set is:")
print(test_precision_knn)
test_recall_knn <- JG_recall(testPredict_knn,testLabels)
print("Recall in Test Set is:")
print(test_recall_knn)
test_F1score_knn <- JG_F1score(testPredict_knn,testLabels)
print("F1 Score in Test Set is:")
print(test_F1score_knn)


# Plotting
symbols <- 1*(testLabels==testPredict_knn) # Misclassification
symbols[symbols ==1] <- 20
symbols[symbols ==0] <- 4
colors <- testPredict_knn + 1
colors[testLabels!=testPredict_knn] <- 9
plot(testSet[,1],testSet[,2], col=scales::alpha(colors,0.5), pch=symbols, main ='KNN Test set prediction')




## Decision Tree
testPredict_DT <- JG_decisionTree(testSet, trainSet, trainLabels)

# Performance
test_conf_DT <- table(testPredict_DT, testLabels)
test_conf_DT
test_accuracy_DT <- JG_accuracy(testPredict_DT,testLabels)
print("Accuracy in Test Set is:")
print(test_accuracy_DT)
test_precision_DT <- JG_precision(testPredict_DT,testLabels)
print("Precision in Test Set is:")
print(test_precision_DT)
test_recall_DT <- JG_recall(testPredict_DT,testLabels)
print("Recall in Test Set is:")
print(test_recall_DT)
test_F1score_DT <- JG_F1score(testPredict_DT,testLabels)
print("F1 Score in Test Set is:")
print(test_F1score_DT)

# Plotting
symbols <- 1*(testLabels==testPredict_DT) # Misclassification
symbols[symbols ==1] <- 20
symbols[symbols ==0] <- 4
colors <- testPredict_DT + 1
colors[testLabels!=testPredict_DT] <- 9
plot(testSet[,1],testSet[,2], col=scales::alpha(colors,0.5), pch=symbols, main ='Decision Tree Test set prediction')








