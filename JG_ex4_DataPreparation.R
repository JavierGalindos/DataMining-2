# Exercise 4: Data preparation
# Javier Galindos
rm(list=ls())

library("rstudioapi") 
set.seed(123)

# PCA
JG_pca <- function(dataset){
  dataset <- scale(dataset, center = FALSE, scale = TRUE)
  mu <- colMeans(dataset)
  data_centered <- matrix(, nrow= nrow(dataset), ncol = ncol(dataset))
  for (i in seq(1:ncol(dataset))){
    data_centered[,i] <- dataset[,i]- mu[i]  # center the data
  }
  C <- cov(data_centered)  # describe each axis variation
  
  eigenObject <- eigen(C)
  eigenVectors <- eigenObject$vectors
  eigenValues <- eigenObject$values
  
  return(list(components = eigenVectors, varianceExplained = eigenValues))  
}

JG_pca_project <- function(dataset, varianceExplained){
  PCA <- JG_pca(dataset)
  varianceScaled <- PCA$varianceExplained / sum(PCA$varianceExplained)
  # Obtain k components for specific variability
  k <- length(varianceScaled[cumsum(varianceScaled)<= varianceExplained])
  dataProjected <- dataset %*% PCA$components[,1:k]
  return (dataProjected)
}

# Generating dataset
# Independent variables
nCol <- 4
dataset <- matrix(rnorm(nCol* 500),ncol= nCol)
# Dependent variables
dataset=cbind(dataset,dataset[,1]^2)
dataset=cbind(dataset,dataset[,2]^2)
dataset=cbind(dataset,dataset[,4]^2)

# Correlation matrix before PCA
cor(dataset)

# PCA
PCA <- JG_pca(dataset)
varianceScaled <- PCA$varianceExplained / sum(PCA$varianceExplained)
k <- length(varianceScaled[cumsum(varianceScaled)<= 0.95])
barplot(varianceScaled, main = 'Variance explained', xlab = 'Features', ylab = 'Variance explained', col = 'indianred')

# Project data
k <- 7
dataProjected <- dataset %*% PCA$components[,1:k]

# Correlation matrix after PCA
cor(dataProjected)


# Generating dataset of highly correlated variables
nCol <- 4
dataset <- matrix(rnorm(nCol* 500),ncol= nCol)
dataset=cbind(dataset,dataset[,1]*runif(500)*0.01)
dataset=cbind(dataset,dataset[,2]*runif(500)*0.01)
dataset=cbind(dataset,dataset[,3]*runif(500)*0.01)
#dataset=cbind(dataset,dataset[,4]*runif(500)*0.01)
cor(dataset)

# PCA
PCA <- JG_pca(dataset)
varianceScaled <- PCA$varianceExplained / sum(PCA$varianceExplained)
barplot(varianceScaled, main = 'Variance explained', xlab = 'Features', ylab = 'Variance explained', col = 'indianred', ylim=c(0,0.35))

# Project data
k <- 7
dataProjected <- dataset %*% PCA$components[,1:k]
cor(dataProjected)




