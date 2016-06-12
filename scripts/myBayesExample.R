library("superNaiveBayes")


# reading data
if(!exists("learn_set")) 
{
    source("prepare_data_sms.R")
}
library("gmodels")

# function for computing accuracy
computePredStat <- function(crossTable){
    ok = 0
    bad = 0
    for(pred in rownames(crossTable$t)){
        for(actual in colnames(crossTable$t)){
            v = crossTable$t[pred,actual]
            if(pred == actual){
                ok = ok + v
            } else {
                bad = ok + v
            }
        }
    }
    return(ok/(bad + ok))
} 

# evaluation:

# levels of smoothing parameter to test
L = c(0
      , 1e-15
      , 1e-13
      , 1e-11
      , 1e-9
      , 1e-7
      , 1e-6
      , 1e-5
      , 1e-4
      , 1e-3)

# loop initialization
res = c()
i = 1

# evaluating accuracy for each smoothing parameter
for(l in L){
	print(i)
	message("learning ...")
    mod = myNaiveBayes(learn_set, learn_types, l);
    pred <- predict(mod, test_set);
    
	message("predicting ...")
    res[[i]] <- computePredStat(CrossTable(pred
                               , test_types
                               , prop.r = F
                               , prop.c = F
                               , prop.chisq=FALSE
                               , prop.t=FALSE
                               , dnn=c('predicted','actual')));
    i = i+1
}
message("plotting ...")
plot(x=L,y=res,log="x")
