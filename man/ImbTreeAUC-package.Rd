\name{ImbTreeAUC-package}
\alias{ImbTreeAUC-package}
\docType{package}
\title{
  An R package for building classification trees using Area Under the ROC Curve (AUC) on the imbalanced datasets
}
\description{
An R package for building binary and multiclass decision tree algorithms using Area Under the Receiver Operating Characteristic (ROC) Curve, to measure impurity of a node. The package provides non-standard measures to select an optimal split point for an attribute as well as the optimal attribute for splitting through the application of local, semi-global and global AUC measures. Additionally, ImbTreeAUC is able to handle imbalanced data which is a challenging issue in many practical applications. The package supports cost-sensitive learning by defining a misclassification cost matrix and weight sensitive learning. It accepts all types of attributes, including continuous, ordered and nominal. 
}

\author{
Krzysztof Gajowniczek

Maintainer: krzysztof_gajowniczek@sggw.edu.pl
}
\references{

}
\keyword{ package }
\seealso{
  \code{\link{ImbTreeAUC}}, \code{\link{ImbTreeAUCInter}}, \code{\link{PredictTree}}, \code{\link{PrintTree}}, \code{\link{PrintTreeInter}}, \code{\link{ExtractRules}}
}
\examples{
  \dontrun{
 
  }
}
