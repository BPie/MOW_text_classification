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

# tokenizaition and corpus cleaning
sms_dtm  <- DocumentTermMatrix(sms_corpus,
                               control = list(tolower=TRUE
                                              , removeNumbers=TRUE
                                              , stopwords=TRUE
                                              , removePunctuation=TRUE
                                              , stemming=TRUE))
lapply(sms_corpus[1:3], as.character)

