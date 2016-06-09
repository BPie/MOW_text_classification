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
str(sms_raw[1:10,])



