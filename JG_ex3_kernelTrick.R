# Ex 3: Kernel Trick
# Javier Galindos

rm(list=ls())
set.seed(123)

library(rgl)

# Generate half-moon dataset
JG_generateHalfMoon <- function(dataPoins, sd){
  # First half-moon
  x1 <- seq(0, pi, pi/(dataPoins-1))
  y1 <- 4*sin(x1) + rnorm(dataPoins, mean = 0, sd = sd)
  z1 <- runif(dataPoins) * 10
  labels1 <- rep(1,dataPoins)
  
  # Second half-moon
  x2 <- seq(pi, 2*pi, pi/(dataPoins-1))
  y2 <- 4*sin(x2) + rnorm(dataPoins, mean = 0, sd = sd)
  x2 <- x2 - pi/2
  z2 <- runif(dataPoins) * 10
  labels2 <- rep(2,dataPoins)
  
  x <- c(x1,x2)
  y <- c(y1,y2)
  #z <- c(z1,z2)
  labels <- c(labels1,labels2)
  #dataset <- cbind(x,y,z,labels)
  dataset <- cbind(x,y,labels)
  return (dataset)
}

dataPoins <- 500
sd <- 0.6
dataset <- JG_generateHalfMoon(dataPoins, sd)

#Plot 2D
plot(dataset[,1],dataset[,2], col= scales::alpha(dataset[,3]+1,0.3), pch=20, main = 'Half moons dataset')
# Plot 3D
#plot3d(dataset[,1], dataset[,2], dataset[,3],col= scales::alpha(dataset[,4]+1,0.3), type="p") 

# Generating kernel function
z <- matrix(, nrow = nrow(dataset) )
for (i in seq(1:nrow(dataset))){
  z[i] <- 2.33 * dataset[i,1]^3 -17.5*dataset[i,1]^2 + 39.66* dataset[i,1] -26 - dataset[i,2]
}

# Plot 3D
plot3d(dataset[,1], dataset[,2], z,col= scales::alpha(dataset[,3]+1,0.3), type="p") 

planes3d(0, 0, 1, 0, col = 'blue', alpha = 0.6)

#Plot 2D
eq <- function(x){2.33 * x*x*x -17.5*x*x + 39.66* x -26}

curve(eq, from=0, to=4.8, xlab="x", ylab="y", ylim=c(-7,7),main='Kernel function to separate moons')
points(dataset[,1],dataset[,2], col= scales::alpha(dataset[,3]+1,0.3), pch=20)



