library(tm) 
library(class) 
library(SnowballC) 

setwd("C:/Users/usr/Desktop/MOW_text_classification")

##########################################
# MOW Classification kNN native          #
# Date: 2015-06-08                       #
# By: Rozalia Pietrzak                   #
##########################################
# The functions to data_prep have been cp#
# here for simplification and change of  #
# the testing set. Please see data_prepr #
# for reference                          #
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
frequentDTM1 <- frequentDTM
frequentDTM <- tfidf.generate(frequentDTM, 1)
#binaryDF <- as.data.frame(data.matrix(binaryDTM))
#tfidfDF <- as.data.frame(data.matrix(tfidfDTM))

# For deterministic comparison, use the same split as in data_prep
# With no randomization. Use the 5X1/5 solution (testing set is 1/5
# but in the next steps it is selected as a diff part of original data)
testSetCount <- ceiling(nrow(frequentDTM) * TEST_SET_PART)
trainSetCount <- nrow(frequentDTM) - testSetCount

i = 0
testSetIndexes <- c((testSetCount*i+1):(testSetCount*(i+1)+1))
trainSetIndexes <- c(1:nrow(frequentDTM))[-testSetIndexes]
# Frequent words are based on DTM only
#frequentWords  <- findFreqTerms(frequentDTM, (NCOL(frequentDTM)-1)*WORD_MIN_FREQ)
frequentDTM <- frequentDTM[,frequentWords]
labels <- as.factor(mainDF$class)

# Set the trainingSet and the TestingSet
trainSet <- as.matrix(frequentDTM[trainSetIndexes,])
testSet <- as.matrix(frequentDTM[testSetIndexes,])
trainSetLabels <- as.factor(labels[trainSetIndexes])
testSetLabels <-  as.factor(labels[testSetIndexes])
                            
# Native classifier
Sys.time()
knnpred <- knn(trainSet, testSet, trainSetLabels, k = 3, l = 0, prob = FALSE)
Sys.time()

# Confusion matrix
#confusionMat <- table(knnpred, testSetLabels)
#confusionMat
confusionMat <- table(knnpred, testSetLabels)
confusionMat

testSet
