
knnCustom.predict <- function(trainSet, testSet, trainClasses, k = 1, simMeasure = 0, isKplus = FALSE, weight = NULL) {
  # Predicts the kNN classifier output
  #
  # Args:
  #   trainSet: Training set 
  #   testSet: Testing set
  #   trainClasses: Labels of the training set
  #   k: number of neighbours to obtains as voters for a class
  #   simMeasure: 0: euclidean Distance, 1: cosine Dissimilarity
  #   isKplus: if TRUE, knn+ is used
  #   weights: constant weights for voting for a given class
  #
  # Returns:
  #   The kNN model of classifier
  # Creating a model of a lazy classifier is useless and is only taking up the memory
  # So the model is not created
  # The prediction keeps its official form
  classes <- sort (unique(trainClasses))

  if(is.null(weight))
    weight <- rep(1, length(classes))

  if(length(weight)!=length(classes))
    stop ("Weight vector is of different size than the unique number of classes")
  
  closestNeighbours <- knnCustom.closestNeighbours(trainSet, testSet, simMeasure, k, isKplus)
  
  # Weighted votes Vectors
  voteLists <- t(sapply(closestNeighbours,function(x){
    singleVote(x, weight, as.numeric(trainClasses))
  }))
  
  # votes
  class <- apply(voteLists,1, function(x){
    classes[order(x, decreasing = TRUE)[1]]
  })
  
  knnCustom.predict <- class 
}

knnCustom.closestNeighbours <- function(trainSet, testSet, simMeasure = 0, k = 1, isKplus = FALSE) {
  # Computes the closestNeighbours for the vorting
  #
  # Args:
  #   trainSet: trainingsSet, to which distance is caluclated
  #   testSet: testSet, to which distance is calculated
  #   simMeasure: if 0: calculate euclidean, if 1: caluculate Cosine
  #   k: parameter of closes neigbours
  #   isKplus: parameter if the neighbours are to be k++
  #
  # Returns:
  #   The list of vectors of indices of the closes neighbours
  
  # For all the samples in a testing set and all the samples in a training
  # set, calculate distance
  
  if(!is.matrix(trainSet) || !is.matrix(testSet))
    stop ("TrainSet or TestSet are not matrices")
  else if(NCOL(trainSet)!=NCOL(testSet))
    stop ("TrainSet and TestSet have different number of arguments")
  
  if(simMeasure == 0){
    output <- list(nrow(testSet))  
    for(x in 1:nrow(testSet)){
      out <- list(nrow(trainSet))
      for(y in 1:nrow(trainSet))
        out[[y]] <- calculateEuclidean(testSet[x,], trainSet[y,])
      
      if(!isKplus)  
        output[[x]] <- order(simplify2array(out))[1:k]
      else
        output[[x]] <- orderKPlus(simplify2array(out),k)
    }
    knnCustom.dist <- output
  }
  else{
    output <- list(nrow(testSet))  
    for(x in 1:nrow(testSet)){
      out <- list(nrow(trainSet))
      for(y in 1:nrow(trainSet))
        out[[y]] <- calculateCosine(testSet[x,], trainSet[y,])
      
      if(!isKplus)  
        output[[x]] <- order(simplify2array(out))[1:k]
      else
        output[[x]] <- orderKPlus(simplify2array(out),k)
    }
    knnCustom.dist <- output
    }
    knnCustom.dist <- output
  }
}

calculateEuclidean <- function(a,b){
  # Computes the square of euclidean dissimilarity (distance) between two vectors
  #
  # Args:
  #   a: One of two vectors which are taken into consideration
  #   b: Second of the vectors
  #
  # Returns:
  #   The euclidean distance between vectors a and b (scalar)

  anew <- a[a!=b]
  bnew <- b[a!=b]

  sum<-0
  
  if(length(anew)!=0){  
    for(i in 1:length(anew))
      sum = sum + (anew[i]-bnew[i])^2
  }
  calculateEuclidean <- sum
}

calculateCosine <- function(a,b){
  # Computes the cosine dissimilarity (distance) between two vectors
  #
  # Args:
  #   a: One of two vectors which are taken into consideration
  #   b: Second of the vectors
  #
  # Returns:
  #   The cosine reverse similarity (1-cossim) between vectors a and b (scalar)
  
  if(!is.vector(a) || !is.vector(b))
    stop ("Input parameters must be one-dimensional arrays")
  else if(length(a)!=length(b))
    stop ("Input vectors are of different lengths in calculateCosine function")
  
  anew <- a[a!=b]
  bnew <- b[a!=b]

  sumAB <- 0
  sumA <- 0
  sumB <- 0
  
  if(length(anew)!=0){
  for(i in 1:length(anew)){
    sumAB <- sumAB + anew[i]*bnew[i]
    sumA <- sumA + anew[i]^2
    sumB <- sumB + bnew[i]^2
  }
  
  
    if(sumA!=0 && sumB!=0){
      calculateCosine <- (1 - sumAB/(sumA*sumB)^(1/2))
    } else{
      calculateCosine <- 1
    } } else calculateCosine <- 0

}

orderKPlus <- function(x, k) {
  # Get K++ neighbours for a sample of the testSet
  #
  # Args:
  #   x: all the trainingSet's distances
  #   k: nominal k value
  #
  # Returns:
  #   Array of K++ size containing k nearest neighbour and the equal-distanced to the 
  #   most remote one
  
  ordered <- x[order(x)]
  
  i <- k
  while(i <= length(x) && ordered[i]==ordered[k]){
    i <- i+1
  }
  
  orderKPlus <- order(x)[1:i-1]
}


singleVote <- function(x, weights, trainClasses) {
  # Counts the number of votes for each class based on weights and vector of labels of the training set
  #
  # Args:
  #   x: Array of distances of a sample from all trainig samples
  #   weights: An Array of weights for all the classes
  #   trainClasses:  Vector of labels of the training Set
  #
  # Returns:
  #   Array of votes for each class
  
  singleVote <- sapply (1:length(weights), function(y){
    sum(weights[y]*as.numeric(trainClasses[x]==y))
  } )
  
}
