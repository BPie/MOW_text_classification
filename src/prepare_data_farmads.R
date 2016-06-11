library(tm)
library(SnowballC)
library(RSofia)

# setting paths
path_project <-  ".."
path_data_root  <- paste(path_project,"datasets", sep="/")
path_data_farmads <- paste(path_data_root, "farm.txt", sep="/")

raw_data <- read.svmlight(path_data_farmads)
data  <- raw_data$data
labels  <- raw_data$labels
data[1:5,1:19]
NROW(data)
NCOL(data)
