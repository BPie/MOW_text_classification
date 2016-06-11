tfidf.generate <- function(frequencyMatrix, normalization = 0){
# Creates a new tfidf Matrix from frequency matrix
#
# Args:
#   frequencyMatrix: Training set 
#   normalization: weighting type fot TF*IDF
#
# Returns:
#   The tfidf matrix
  
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

frequencyMatrix <- matrix(c(1,1,1,0,0,0,1,1), byrow = TRUE, ncol=4)
frequencyMatrix

tfidf.calculateAverageTF <- function(frequencyMatrix) {
  # Calculate averaged value of term Frequency
  # TF = <term_frequency>/<no_of_terms_in_a document>
  #
  # Args:
  #   frequencyMatrix: Training set 
  #
  # Returns: 
  #   Aforementioned TF
  
  # For each document
  output<-apply(frequencyMatrix, 1, function(x){
    # Calculate the number of documents in which the term appears
    x/sum(x)
  } )

  tfidf.calculateAverageTF<-t(output)
  
}

tfidf.calculateLogIDF <- function(frequencyMatrix) {
  # Calculate a simple logarythmical form of IDF
  # IDF = log(<no of documents>/<no of documents in which a given term appears>)
  #
  # Args:
  #   frequencyMatrix: Training set 
  #
  # Returns: 
  #   Specified IDF values
  
  noOfDocuments <- NROW(frequencyMatrix)
  # For each term
  tfidf.calculateLogIDF<-apply(frequencyMatrix, 2, function(x){
    # Calculate the number of documents in which the term appears
    log10(noOfDocuments/sum(as.numeric(x[x>0])))
  } )

}


