path_project = ".."
path_data_root = paste(path_project,"datasets", sep="/")
path_data_sms = paste(path_data_root, "sms.csv", sep="/")

sms_raw = read.table(path_data_sms, sep="\t", quote="")
sms_raw[1:5,]