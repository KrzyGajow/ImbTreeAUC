CalcProb <- function( data, Y_name, weights, cost ){

  # Create temporary table, if weights and cost are null then then computations are based on counts not sums
  dat <- data.frame( target = data[,Y_name], weights = if( is.null(weights) ){ 1 }else{ weights } )

  if( !is.null(cost) ){
    
    # Prepare sums of each row
    costCases <- rowSums( cost )
    
    # Assign a particular cost to the corresponding class
    for( i in names(costCases) ){
      
      dat[ dat[, "target"] == i, "weights"] <- costCases[i]
      
    }
    
  }

  # Calculate numerator as a counts/sums of each class
  nom <- tapply( dat[, "weights"], list( dat[, "target"] ), sum )
  nom <- ifelse( is.na(nom), 0, nom )
  
  # Calculate denominator
  denom <- sum(nom)
  
  # Calculate probability of each class
  prob <- nom / denom
  
  # It is possible that class has no observations
  prob <- ifelse( is.finite(prob), prob, 0 )
  
  return( prob )
  
}

ChooseClass <- function( prob, Class_threshold, cost ){
  
  # Check number of classes
  k <- length( prob )
  
  if( Class_threshold == "equal" | is.null(cost) ){
    
    thresholds <- if( k == 2 ){ 0.5 }else{ rep( 1/k, k ) }
    
  }else{
    
    if( k == 2 ){
      
      # For binary case there is only one treshold
      thresholds <- cost[2,1] /( cost[2,1] + cost[1,2] )
      
    }else{
      
      # For multiclass case there are k thresholds
      thresholds <- 1 / rowSums( cost )
      thresholds <- thresholds / sum(thresholds)
      
    }
    
  }
  
  # Calculate how much each threshold is exceeded by the probability
  probs <- prob / thresholds
  
  # Take probability of the class which exceeds its threshold most (it might happen that many probabilities are greater than their thresholds)
  new_class <- names( which.max(probs) )

  return( new_class )
  
}

ClassErrorGlobal <- function( tree, child, cp ){
  
  # Calculate number of incorrectly classified observation of the entire tree (all leaves)
  leaves <- tree$root$Get( "localerror", filterFun = isLeaf )
  n_leaves <- length(leaves)
  leaves_error <- sum( leaves )
  leaves_error <- ifelse( is.na(leaves_error), 0, leaves_error)
  
  # Take number of incorrectly classified observation of the current leaf
  curr_leaf <- tree$localerror
  
  # Take number of observations (denominator)
  nobs <- tree$root$Count
  
  # Calculate misclassification rate of the entire tree
  curr_leaf_error <- leaves_error / nobs 
  
  # Calculate misclassification rate of the tree after adding new split
  new_error <- ( (leaves_error - curr_leaf) + sum(child) ) / nobs
  
  # Calculate improvement
  # improvement <- curr_leaf_error - new_error 
  cp_curr <- curr_leaf_error + 0 * n_leaves# * (tree$root$localerror / nobs) 
  cp_new <- new_error + 0 * ( n_leaves + 1 )# * (tree$root$localerror / nobs) 
  cp_root <- (tree$root$localerror / nobs)#  + cp * ( 1 ) * (tree$root$localerror / nobs) 
  improvement <- (cp_curr - cp_new) / cp_root
  
  return( improvement )
  
}

ClassErrorLocal <- function( dat ){
  
  # Calculate number of incorrectly classified observation of each child
  error <- unname( apply( dat, 1, function( x ){ sum(x) - x[ which.max(x) ] } ) )
  
  return( error )
  
}