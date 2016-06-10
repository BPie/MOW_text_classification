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
        temp_type_data = learn_set[learn_types==t,]
        temp_count = NROW(temp_type_data)
        likelihood[t,] <- apply(temp_type_data
                                , 2
                                , function(x)
                                {
                                  temp_v = length(which(x=="Yes"))/temp_count
                                  ifelse(temp_v>0, temp_v, smoothing)  
                                })
    }
    model = structure(list(likelihood=likelihood
                           , priors=priors)
                      , class="myNaiveBayes")
    return(model)
}

predict.myNaiveBayes  <- function(model, newdata)
{
    prob <- data.frame(matrix(ncol=NROW(newdata)
                              , nrow=NROW(model$priors)))
    
    row.names(prob) <- row.names(model$priors)
    
    for(new_sample in 1:NROW(newdata)) {
        for(t in row.names(model$priors)) {
            p = model$likelihood[t,]
            pr = model$priors[t,]
            sample = newdata[new_sample,]
            
            yes_idx = which(sample=="Yes")
            no_idx = which(sample=="No")
            
            yes_val = ifelse(length(yes_idx) > 0
                             , prod(p[yes_idx])
                             , 1)
            no_val = ifelse(length(no_idx) > 0
                            , prod(1-as.numeric(p[no_idx]))
                            , 1)
            v = yes_val*no_val
            prob[t, new_sample] <- v*model$priors[t,1]
        }    
    }
    return(rownames(prob)[apply(prob,2,which.max)])
}
# learning
mod = myNaiveBayes(learn_set
                   , learn_types
                   , 0.0001)
# predicting
pred <- predict(mod
                , test_set)

# evaluation
CrossTable(pred
           , test_types
           , prop.chisq=FALSE
           , prop.t=FALSE
           , dnn=c('predicted','actual'))


