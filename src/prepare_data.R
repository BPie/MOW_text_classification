library(tm)
library(SnowballC)


# setting paths
path_project <-  ".."
path_data_root  <- paste(path_project,"datasets", sep="/")
path_data_sms <- paste(path_data_root, "sms.csv", sep="/")

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
sms_corpus  <- tm_map(sms_corpus,
                      content_transformer(tolower))
sms_corpus  <- tm_map(sms_corpus,
                      removeNumbers)
sms_corpus  <- tm_map(sms_corpus,
                      removeWords,
                      stopwords())
sms_corpus  <- tm_map(sms_corpus,
                      removePunctuation)
sms_corpus <- tm_map(sms_corpus,
                     stemDocument)
sms_corpus <- tm_map(sms_corpus,
                     stripWhitespace)

lapply(sms_corpus[1:3], as.character)

