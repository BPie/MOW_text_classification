#' Naive Bayes classificator
#'
#' This function returns model for prediction using Naive Bayes 
#' Classificator
#' @param data data.frame of data to learn the classifier in form of DataTermMatrix with "Yes" or "No"
#' @param types list of classes/types of examples given in data
#' @param smoothing Lapplace-smoothing paraneter Default to 0
#' @keywords naive bayes classification
#' @export
#' @examples
#' mod <- myNaiveBayes(learn_set, learn_types, 0.0001)
#' pred <- predict(mod, test_set)

myNaiveBayes  <- function(data, types, smoothing=0)
{
    types_lvls  <- levels(types)

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


#' Classification using Naive Bayes 
#'
#' This function returns predictions of given data types
#' @param model model of Naive Bayes returned by naiveBayes function
#' @param newdata data to be classified
#' @keywords naive bayes classification
#' @export
#' @examples
#' mod <- myNaiveBayes(learn_set, learn_types, 0.0001)
#' pred <- predict(mod, test_set)

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
