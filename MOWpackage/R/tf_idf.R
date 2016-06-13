#' TF-IDF funcion
#' 
#'   Creates a new tfidf Matrix from frequency matrix
#' @param frequencyMatrix: Values of the DTM for a training set
#' @param normalization: weighting type fot TF*IDF, 0: without normalization, 1: with normalization. DEFAULTs as 0
#' @export
tfidf.generate <- function(frequencyMatrix, normalization = 0){
  
  if(normalization == 0){
    tf <- frequencyMatrix
    idf <- tfidf.calculateLogIDF(frequencyMatrix)
    tfidf.generate <- t(apply(tf,1,function(x){
      x*idf
    } ))
  }
  else if(normalization == 1){
    tf <- tfidf.calculateAverageTF(frequencyMatrix)
    idf <- tfidf.calculateLogIDF(frequencyMatrix)
    tfidf.generate <- t(apply(tf,1,function(x){
      x*idf
    } ))
  }
  else
    error("This type of normalization is cannot be handled")

}

#' Calculates normalized TF for a given frequencyMatrix
#' 
#'   Creates a new TF Matrix from frequency matrix
#' @param frequencyMatrix: Values of the DTM for a training set
#' @export
tfidf.calculateAverageTF <- function(frequencyMatrix) {
  output<-apply(frequencyMatrix, 1, function(x){
    x/sum(x)
  } )

  tfidf.calculateAverageTF<-t(output)
  
}

#' Calculates IDF for a given frequencyMatrix
#' 
#'   Creates a new IDF Matrix from frequency matrix
#' @param frequencyMatrix: Values of the DTM for a training set
#' @export
tfidf.calculateLogIDF <- function(frequencyMatrix) {
  noOfDocuments <- NROW(frequencyMatrix)
  # For each term
  tfidf.calculateLogIDF<-apply(frequencyMatrix, 2, function(x){
    # Calculate the number of documents in which the term appears
    log10(noOfDocuments/sum(as.numeric(x[x>0]))) + 1
  } )
}


