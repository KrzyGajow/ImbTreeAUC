% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ImbTreeAUCInter.R
\name{ExtractRules}
\alias{ExtractRules}
\title{Extract decision rules from an ImbTreeAUC Object}
\usage{
ExtractRules( tree )
}
\arguments{
\item{tree}{Fitted model object. This is assumed to be the result of some function that produces an object with the same named components as that returned by the ImbTreeAUC or ImbTreeAUCInter functions.}
}
\value{
Data frame with the extracted decision rules along with the following performance measures: Support, Confidence, Lift, Conviction, AddedValue, Cosine, Jaccard, Laplace, Leverage.
}
\seealso{
  \code{\link{ImbTreeAUC}}, \code{\link{ImbTreeAUCInter}}, \code{\link{PredictTree}}, \code{\link{PrintTree}}, \code{\link{PrintTreeInter}}, \code{\link{ExtractRules}}
}

\examples{
library("ImbTreeAUC")
data(iris)
Tree <- ImbTreeAUC(Y_name = "Species", 
                   X_names = colnames(iris)[-ncol(iris)], 
                   data = iris) 
}