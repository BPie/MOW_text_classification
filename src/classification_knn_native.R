library(tm) 
library(class) 
library(SnowballC) 

setwd("C:/Users/usr/Desktop/MOW_text_classification")

##########################################
# MOW Classification kNN native          #
# Date: 2015-06-08                       #
# By: Rozalia Pietrzak                   #
##########################################

# setting consts
TEST_SET_PART  <- 0.2
WORD_MIN_FREQ  <- 0.001


# Read data
mainDF <- read.csv("./datasets/sms.csv", 
               sep ="\t", 
               header = FALSE, 
               stringsAsFactors = FALSE, 
               quote = "")

colnames(mainDF) <- c("class", "text")


# Generate corpus out of the text-column and
# Clean corpus (since we will be doing
# Binary Term, Frequent Term and TF-IDF,
# it is better to clean the corpus than
# to clean each of the sets)
corpus <- Corpus(VectorSource(mainDF$text))

corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, stemDocument, language = "english")
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Create DocumentTerm Frequency Matrix, with frequent, binary and TF-IDF
frequentDTM <- DocumentTermMatrix(corpus)
#binaryDTM <- DocumentTermMatrix(corpus, control = list(weighting =
#                                                        function(x)
#                                                        weightBin(x)))
#tfidfDTM <- DocumentTermMatrix(corpus, control = list(weighting =
#                                                         function(x)
#                                                           weightTfIdf(x), normalization=FALSE))

# Transform dtm to matrix to data frame - df is easier to work with
frequentDF <- as.data.frame(data.matrix(frequentDTM))
frequentDF <- cbind(frequentDF, mainDF$class)
#binaryDF <- as.data.frame(data.matrix(binaryDTM))
#tfidfDF <- as.data.frame(data.matrix(tfidfDTM))

# For deterministic comparison, use split in half
trainSet <- frequentDF[1:(NROW(frequentDF)*4/5-1),1:(NCOL(frequentDF)-1)]
testSet <- frequentDF[(NROW(frequentDF)*4/5):(NROW(frequentDF)-1),1:(NCOL(frequentDF)-1)]
cl <- frequentDF[1:(NROW(frequentDF)*4/5-1),NCOL(frequentDF)-1]
clTest <-  frequentDF[(NROW(frequentDF)*4/5):(NROW(frequentDF)-1),NCOL(frequentDF)-1]

# finding frequent words
frequentWords  <- findFreqTerms(frequentDTM, (NCOL(frequentDF)-1)*WORD_MIN_FREQ)
trainSet <- trainSet[,frequentWords]
testSet <- testSet[,frequentWords]



# Classifier
knnpred <- knnCustom.predict(trainSet, testSet, cl, k=1)
knnpred <- knn(trainSet, testSet, cl, k = 1, l = 0, prob = FALSE)

# Confusion matrix
confusionMat <- table(knnpred, clTest)
confusionMat

