
knnCustom.predict <- function(trainSet, testSet, trainClasses, k = 1, simMeasure = 0, isKplus = FALSE, weights = c(1,1)) {
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
  distanceMatrix <- knnCustom.dist(trainSet, testSet, simMeasure)
  
  # Generate the voteList of all the closestNeighbours
  voteLists <- apply(distanceMatrix, 1,function(x){
    if(isKplus) 
      orderKPlus(x, k) 
    else
      order(x)[1:k]
  })
  
  # Weighted votes Vectors
  voteLists <- sapply(voteLists,function(x){
    singleVote(x, weights, trainClasses)
  })
  
  # votes
  class <- apply(voteLists, 2, function(x){
    order(x)[-1]
  })
  
  knnCustom.predict <- class
}

knnCustom.dist <- function(trainSet, testSet, simMeasure = 0) {
  # Computes the dissimilarity between two matrices
  #
  # Args:
  #   trainSet: trainingsSet, to which distance is caluclated
  #   testSet: testSet, to which distance is calculated
  #   simMeasure: if 0: calculate euclidean, if 1: caluculate Cosine
  #
  # Returns:
  #   The matrices of a-b elements distances
  
  # For all the samples in a testing set and all the samples in a training
  # set, calculate distance
  
  if(!is.data.frame(trainSet) || !is.data.frame(testSet))
    stop ("TrainSet or TestSet are not matrices")
  else if(NCOL(trainSet)!=NCOL(testSet))
    stop ("TrainSet and TestSet have different number of arguments")
  
  knnCustom.dist <- apply(trainSet, 1, function(x){
    apply(testSet,1, function(y){
      if(simMeasure == 0)
        calculateEuclidean(x,y)
      else
        calculateCosine(x,y)
    })
  })
  
}


calculateEuclidean <- function(a,b){
  # Computes the euclidean dissimilarity (distance) between two vectors
  #
  # Args:
  #   a: One of two vectors which are taken into consideration
  #   b: Second of the vectors
  #
  # Returns:
  #   The euclidean distance between vectors a and b (scalar)
  
  if(!is.vector(a) || !is.vector(b))
    stop ("Input parameters must be one-dimensional arrays")
  else if(length(a)!=length(b))
    stop ("Input vectors are of different lengths in calculateEudclidean function")
  
  sum <- sum(mapply(function(x,y){(x-y)^2}, a, b))

  calculateEuclidean <- sum^(1/2)
  
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
    error("Input parameters must be one-dimensional arrays")
  else if(length(a)!=length(b))
    error("Input vectors are of different lengths in calculateCosine function")

  sumAB <- sum(mapply(function(x,y){x*y}, a, b))
  sumA <- sum(sapply(a, function(x){x^2}))
  sumB <- sum(sapply(b, function(x){x^2}))
  
  if(sumA!=0 && sumB!=0)
    calculateCosine <- (1 - sumAB/(sumA*sumB)^(1/2))
  else
    calculateCosine <- 1
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
  while(ordered[i]==ordered[k]){
    print(ordered[i])
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

