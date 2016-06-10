if(!exists("learn_set")) 
{
    source("prepare_data.R")
}
library("gmodels")

myNaiveBayes  <- function(data, types, smoothing=0)
{
    types_lvls  <- levels(types)
    # assumption, that we learn for the first class 
    # (and all the rest)
    target_type  <- types_lvls[1]

    types_lvls_count <- length(types_lvls)
    bow_count  <- NCOL(data) # bag of words count

    types_summary  <- table(types)
    data_len  <-  NROW(types)

    # data initialization
    priors <- data.frame(matrix(ncol=1
                               , nrow=types_lvls_count))
    row.names(priors)  <- types_lvls
    names(priors)  <- c("prior")

    likelihood <- data.frame(matrix(ncol=bow_count
                                   , nrow=types_lvls_count))
    row.names(likelihood) <- types_lvls
    names(likelihood) <- names(data)

    for(t in types_lvls)
    {
        # setting priors
        priors[t, "prior"]  <- types_summary[t]/data_len
    
        # setting approx. likelihood
        
    }
    print(priors)
}

predict.myNaiveBayes  <- function(modelObject) 
{
    
}
myNaiveBayes(learn_set, learn_types)

