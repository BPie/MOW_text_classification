if(!exists("learn_set")) 
{
    source("prepare_data.R")
}
library("e1071")
library("gmodels")

# learning
LAPLACE_SM  <- 1 # laplace smoothing parameter
classifier_sms  <- naiveBayes(learn_set
                              , learn_types
                              , laplace = LAPLACE_SM)

# predicting
prediction_sms  <- predict(classifier_sms
                           , test_set)

# prediction evaluation
CrossTable(prediction_sms
           , test_types
           , prop.chisq=FALSE
           , prop.t=FALSE
           , dnn=c('predicted','actual'))
