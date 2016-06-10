library(tm)
library(SnowballC)


# setting paths
path_project <-  ".."
path_data_root  <- paste(path_project,"datasets", sep="/")
path_data_sms <- paste(path_data_root, "sms.csv", sep="/")

# setting consts
TEST_SET_PART  <- 0.2
WORD_MIN_FREQ  <- 0.001

# reading data
sms_raw <- read.table(path_data_sms
                     , sep="\t"
                     , quote=""
                     , stringsAsFactors=FALSE
                     , header=FALSE)

# setting column names
colnames(sms_raw)  <- c("type", "message")

# setting factors
sms_raw$type  <- factor(sms_raw$type)

# creating corpus
sms_corpus  <- VCorpus(VectorSource(sms_raw$message))

# tokenizaition and corpus cleaning
sms_dtm  <- DocumentTermMatrix(sms_corpus,
                               control = list(tolower=TRUE
                                              , removeNumbers=TRUE
                                              , stopwords=TRUE
                                              , removePunctuation=TRUE
                                              , stemming=TRUE))

# splitting data
set_count <- NROW(sms_dtm)
test_set_count  <- ceiling(set_count * TEST_SET_PART)
learn_set_count  <- set_count - test_set_count

test_idxs  <- 1:test_set_count
learn_idxs  <- (test_set_count+1):(set_count)

test_set  <- sms_dtm[test_idxs,]
learn_set  <- sms_dtm[learn_idxs,]

test_types <- sms_raw[test_idxs,]$type
learn_types <- sms_raw[learn_idxs,]$type

# finding frequent words
word_count  <- length(dimnames(sms_dtm)$Terms)
word_min_count  <-  ceiling(word_count * WORD_MIN_FREQ)
frequent_words  <- findFreqTerms(learn_set, word_min_count)
frequent_words_count  <- length(frequent_words)

# narrowing dtm sets
test_set  <- test_set[,frequent_words]
learn_set  <- learn_set[,frequent_words]

# convert to bag of words
conv_to_bow <- function(inp) inp <- ifelse(inp > 0, "Yes", "No");
test_set  <- apply(test_set
                   , MARGIN=2
                   , conv_to_bow)

learn_set  <- apply(learn_set
                   , MARGIN=2
                   , conv_to_bow)
