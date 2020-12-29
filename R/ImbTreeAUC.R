#' Fit a Decision Trees
#'
#' @param Y_name Name of the target variable. Character vector of one element.
#' @param X_names Attribute names used for target (Y_name) modelling. Character vector of many elements.
#' @param data Data.frame in which to interpret the parameters Yname and Xnames.
#' @param depth Set the maximum depth of any node of the final tree, with the root node counted as depth 0. 
#' Numeric vector of one element which is greater or equal to 0.
#' @param min_obs The minimum number of observations that must exist in any terminal node (leaf). 
#' Numeric vector of one element which is greater or equal to 1.
#' @param type Method used for learning. Character vector of one element with one of the: "AUCl", "AUCs", "AUCg".
#' @param levelPositive: Name of the positive class (label) used in AUC calculation, i.e. predictions being the probability of the positive event; character vector of one element.
#' @param cp Complexity parameter, i.e. any split that does not decrease the overall lack of fit by a factor of cp is not attempted. 
#' It refers to miss-classification error. If cost or weights are specified aforementioned measure takes these parameter into account.
#' Numeric vector of one element which is greater or equal to 0.
#' @param n_cores Number of cores used for parallel processing. Numeric vector of one element which is greater or equal to 1.
#' @param weights Numeric vector of cases weights. It should have as many elements as the number of observation in the data.frame passed to the data parameter.
#' @param cost Matrix of costs associated with the possible errors. The matrix should have k columns and rows, where k is the number of class levels. 
#' Rows contain true classes while columns contain predicted classes. Rows and columns names should take all possible categories (labels) of the target variable.
#' @param AUCweight: Method used for AUC weighting in multiclass classification problems; character vector of one element with one of the: "none", "bySize", "byCost".
#' @param class_th Method used for determining thresholds based on which the final class for each node is derived. 
#' If cost is specified it can take one of the following: "theoretical", "tuned", otherwise it takes "equal". 
#' Character vector of one element.
#' @param overfit Character vector of one element with one of the: "none",”leafcut”, "prune", "avoid" specifying which method overcoming overfitting should be used. 
#' ”leafcut” method is used when the full tree is built, it reduces the subtree when both siblings choose the same class label.
#' "avoid" method is incorporated during the recursive partitioning, it prohibit the split when both sibling chose the same class.
#' “prune” method employs pessimistic error pruning procedure, it should be specified along with the cf parameter.
#' @param cf Numeric vector of one element with the number in (0, 1) for the optional pessimistic-error-rate-based pruning step.
#'
#' @return
#' @export ImbTreeAUC
#' 
#' @seealso 
#' \code{\link{ImbTreeAUC}}, \code{\link{ImbTreeAUCInter}}, \code{\link{PredictTree}}, 
#' \code{\link{PrintTree}}, \code{\link{PrintTreeInter}}, \code{\link{ExtractRules}}
#' 
#' @examples
#'
#' \dontrun{
#' 
#' library("ImbTreeAUC")
#' data(iris)
#' Tree <- ImbTreeAUC(Y_name = "Species", 
#'                    X_names = colnames(iris)[-ncol(iris)], 
#'                    data = iris) 
#' PrintTree(Tree)
#' }
ImbTreeAUC <- function( Y_name, X_names, data, depth = 5, min_obs = 5, type = "AUCg", levelPositive = "1", cp = 0, n_cores = 1,
                        weights = NULL, cost = NULL, AUCweight = "none", class_th = "equal", overfit = "leafcut", cf = 0.25 ){

  # Check if all parameters are correctly specified, if no terminate the program
  Stop <- StopIfNot( Y_name, X_names, data, depth, min_obs, type, levelPositive, cp, n_cores, weights, cost, AUCweight, 
                     class_th, overfit, cf, 1, 2, T, 1, 1 )
  if( !Stop ){

    return( invisible() )

  }

  # Create probability matrix, initially all probabilities are equal
  Y_statistics <- CalcProb( data, Y_name, weights, cost )

  # Determine class labels
  Y_levels <- levels( data[,Y_name] )
  
  # Assign global probability matrix and auc
  AssignProbMatrix( data, Y_name, Y_statistics, Y_levels )
  
  # Create the root of the Tree
  Tree <- Node$new("Root")

  # Assign various initial measures
  AssignInitMeasures( Tree, data, Y_statistics, levelPositive, type, weights, AUCweight, cost, class_th )

  # If needed start cluster for parallel processing
  if( n_cores > 1 ){

    assign( "Global_Cluster", makeCluster( n_cores ), envir = .GlobalEnv )
    
  }

  # Call of the main Building function
  if( !type == "AUCg" ){
    
    # Standard recursive partitioning
    type_ <- ifelse( type == "AUCs", "AUCg", type )
    BuildTree( Tree, Y_name, X_names, data, depth, min_obs, type_, levelPositive, cp, n_cores, weights, AUCweight, cost, class_th, overfit, 1 )
    
  }else{
    
    BuildTreeGlob( Tree, Y_name, X_names, data, depth, min_obs, "AUCg", levelPositive, cp, n_cores, weights, AUCweight, cost, class_th, overfit, 1 )
    
  }

  # Determine class of each observation based on various approaches setting up thresholds
  thresholds <- AssignClass( class_th, cost )
  
  # Assign final class
  if( class_th == "tuned" ){
    
    UpdateTree( Tree )
    
  }

  # Prune tree if needed
  if( overfit == "leafcut" ){
    
    PruneTree( Tree ) 
    
  }else if( overfit == "prune" ){
    
    PessimisticErrorPruning( Tree, cf )
    
  }
    
  # Create various info for later use
  AddAttr( Tree, data, Y_levels, Y_statistics, min_obs, levelPositive, type, cp, weights, AUCweight, cost, class_th, thresholds, 
           overfit, cf, NULL, NULL, NULL, NULL, NULL )

  # Remove no more required objects
  rm(list = c("Probability_matrix", "Decision", "Global_AUC"), envir = .GlobalEnv)
  
  # If needed stop cluster for parallel processing
  if( n_cores > 1 ){

    stopCluster( Global_Cluster )

  }

  # Return Final Tree
  return( Tree )

}
