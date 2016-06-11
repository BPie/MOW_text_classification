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

path_all_articles <- list.files(path=path_articles, pattern="*.txt")
print(path_all_articles)



# reading data
#raw_data <- read.svmlight(path_data_farmads)
#data  <- raw_data$data
#types  <- raw_data$labels

## splitting data
#set_count <- NROW(data)
#test_set_count <- ceiling(set_count * TEST_SET_PART)
#learn_set_count <- set_count - test_set_count

#test_idxs  <- 1:test_set_count
#learn_idxs  <- (test_set_count+1):(set_count)

#test_set  <- data[test_idxs,]
#learn_set  <- data[learn_idxs,]

#test_types <- types[test_idxs]
#learn_types <- types[learn_idxs]


## convert to bag of words
#conv_to_bow <- function(inp) inp <- ifelse(inp > 0, "Yes", "No");
#test_set  <- apply(test_set
                   #, MARGIN=2
                   #, conv_to_bow)

#learn_set  <- apply(learn_set
                   #, MARGIN=2
                   #, conv_to_bow)
