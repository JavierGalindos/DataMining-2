# Ex 1: Feature Selection
# Javier Galindos
# Fisher score
rm(list=ls())

library("rstudioapi")  

# Set working directory
setwd(dirname(getActiveDocumentContext()$path)) 
getwd()  

# loading the data from file
load(file="Data/JGdata.RData")


JG_fisherScore <- function(feature, labels){
  #n_classes <- length(unique(labels))
  # Mean of each class
  mu_j <- aggregate(feature, list(labels), mean)
  # Standard deviation of each class
  sigma_j <- aggregate(feature, list(labels), sd)
  # Global mean
  mu <- mean(feature)
  # Fraction of points belonging to the same class
  p_j <- table(labels) / length(labels)
  
  # Compute Fisher score
  numerator <- p_j * ((mu_j$x - mu)^2)
  denominator <- p_j * sigma_j$x^2
  
  FisherScore <- sum(numerator) / sum(denominator)
  return (FisherScore)
}

FS <- JG_fisherScore(x[,2],labels)
FS

FS <- JG_fisherScore(x[,1],labels)


# Compute for each feature

JG_fisherScoreDataset <- function(dataset, labels){
  FS <- matrix(ncol = ncol(dataset) , nrow = 1 )
  colnames(FS) <- colnames(dataset)
  rownames(FS) <- 'Fisher score'
  for (i in 1: ncol(dataset)){
    FS[,i] <- JG_fisherScore(dataset[,i],labels)
  }
  return(FS)
}

FS_dataset <-JG_fisherScoreDataset(x,labels)
barplot(FS_dataset, main = 'Fisher score', col = "indianred")
