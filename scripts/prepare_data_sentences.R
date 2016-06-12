library(tm)
library(SnowballC)


# setting paths
path_project <-  ".."
path_data_root  <- paste(path_project,"datasets", sep="/")
path_data_sentences <- paste(path_data_root, "sentence_corpus", sep="/")
path_stopwords  <- paste(path_data_sentences, "stopwords.txt", sep="/")
path_articles  <- paste(path_data_sentences, "labeled_articles", sep="/")

# setting const
TEST_SET_PART  <- 0.2
WORD_MIN_FREQ  <- 0.001

# getting filenames
path_all_articles <- list.files(path=path_articles, pattern="*.txt")
path_all_articles <- paste(path_articles, path_all_articles, sep="/")

# readding data
all_articles  <- lapply(path_all_articles
                        , read.table
                        , quote=""
                        , sep="\t"
                        , stringsAsFactors=FALSE
                        , header=FALSE)
all_articles  <- Reduce(function(...) merge(...
                                            , all=TRUE
                                            , sort=FALSE)
                        , all_articles)

# setting column names
colnames(all_articles)  <- c("type", "message")

# setting factors
all_articles$type  <- factor(all_articles$type)

# creating corpus
articles_corpus  <-  VCorpus(VectorSource(all_articles$message))

# loading stopwords
stopwords  <- as.list(read.table(path_stopwords
                                 , sep=" "
                                 , header=FALSE))

articles_corpus <- tm_map(articles_corpus, content_transformer(tolower))
if(!is.null(stopwords)) 
{
    articles_corpus <- tm_map(articles_corpus
                              , removeWords 
                              , words=as.character(stopwords))
} else {
    articles_corpus <- tm_map(articles_corpus, removeWords, stopwords())
}
articles_corpus <- tm_map(articles_corpus, removePunctuation)
articles_corpus <- tm_map(articles_corpus, stemDocument)
articles_corpus <- tm_map(articles_corpus, stripWhitespace)

# tokenization 
articles_dtm  <- DocumentTermMatrix(articles_corpus
                                    , control = list(tolower=FALSE
                                                     , removeNumbers=FALSE
                                                     , stopwords=FALSE
                                                     , removePunctuation=FALSE
                                                     , stemming=FALSE))


# splitting data
set_count <- NROW(articles_dtm)
test_set_count <- ceiling(set_count * TEST_SET_PART)
learn_set_count <- set_count - test_set_count

test_idxs <- 1:test_set_count
learn_idxs <- (test_set_count+1):(set_count)

test_set <- articles_dtm[test_idxs,]
learn_set <- articles_dtm[learn_idxs,]

test_types <- all_articles[test_idxs,]$type
learn_types <- all_articles[learn_idxs,]$type

# finding frequent words
word_count  <- length(dimnames(articles_dtm)$Terms)
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
