#' kNN+ classifier
#'
#' Predicts samples class using kNN classifier
#' @param: trainSet Training set 
#' @param: testSet Testing set
#' @param: trainClasses Labels of the training set
#' @param: k number of neighbours to obtains as voters for a class. DEFAULTs to 1.
#' @param: simMeasure 0: euclidean Distance, 1: cosine Dissimilarity. DEFAULTs to 0.
#' @param: isKplus if TRUE, knn+ is used else: knn is used. DEFAULTs to FALSE
#' @param: weights constant weights for voting for a given class. DEFAULTs to NULL
#' @export
knnCustom.predict <- function(trainSet, testSet, trainClasses, k = 1, simMeasure = 0, isKplus = FALSE, weight = NULL) {

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

#' Neighbours generator for kNN
#' 
#' Computes the closestNeighbours for the vorting
#' @param trainSet: Training set 
#' @param testSet: Testing set
#' @param simMeasure 0: euclidean Distance, 1: cosine Dissimilarity. DEFAULTs to 0.
#' @param k: number of neighbours to obtains as voters for a class. DEFAULTs to 1.
#' @param isKplus: if TRUE, knn+ is used else: knn is used. DEFAULTs to FALSE
#' @export
knnCustom.closestNeighbours <- function(trainSet, testSet, simMeasure = 0, k = 1, isKplus = FALSE) {

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
}

#' Euclidian dissimilarity
#' 
#' Calculates Euclidean distance between two arrays
#' @param a One of two vectors which are taken into consideration
#' @param b Second of the vectors
#' @export
calculateEuclidean <- function(a,b){
  # Computes the square of euclidean dissimilarity (distance) between two vectors
  #
  # Args:
  #   a: The first of the vectors
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

#' Cosine dissimilarity
#' 
#' Calculates Cosine dissimilarity
#' @param a: One of two vectors which are taken into consideration
#' @param b: Second of the vectors
#' @export
calculateCosine <- function(a,b){
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

#' kNN+ ordered array of votes
#' 
#' Calculates the ordered arrray of simple votes, if knn+ is used
#' @param x Array to be ordered
#' @param k Parameter of minimal length of the output array of indices
#' @export
orderKPlus <- function(x, k) {
  ordered <- x[order(x)]
  
  i <- k
  while(i <= length(x) && ordered[i]==ordered[k]){
    i <- i+1
  }
  
  orderKPlus <- order(x)[1:i-1]
}

#' Vote counter for kNN
#' 
#' Calculates votes for a given class for a given sample
#' @param x: Array to count votes for
#' @param weights: Weights of votes of specific classes
#' @param trainClasses: array of labels of the trainig Set
#' @export
singleVote <- function(x, weights, trainClasses) {
  singleVote <- sapply (1:length(weights), function(y){
    sum(weights[y]*as.numeric(trainClasses[x]==y))
  } )
  
}
