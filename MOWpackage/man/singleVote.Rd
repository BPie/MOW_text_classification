% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classification_knn.R
\name{singleVote}
\alias{singleVote}
\title{Vote counter for kNN}
\usage{
singleVote(x, weights, trainClasses)
}
\arguments{
\item{x:}{Array to count votes for}

\item{weights:}{Weights of votes of specific classes}

\item{trainClasses:}{array of labels of the trainig Set}
}
\description{
Calculates votes for a given class for a given sample
}

