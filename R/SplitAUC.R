SplitAUC <- function( variable, target, split_point, measure_parent, indexes, levelPositive, cp, weights, AUCweight, cost, overfit ){

  # Prepare temporary table with required variables
  matrix_all <- data.frame( variable, target, indexes )

  # Adjustment for nominal attributes, determine indexes of each child
  if( is.character(split_point) ){

    inndx <- matrix_all$variable %in% split_point

  }else{

    inndx <- matrix_all$variable <= split_point
    
  }

  # Prepare table for each child
  matrix_left <- matrix_all[ inndx,]
  matrix_right <- matrix_all[ !inndx ,]

  # Calculate counts of each child
  n_obs <- nrow(matrix_all)
  l_obs <- nrow(matrix_left)
  r_obs <- nrow(matrix_right)

  # Determine indexes of each child
  ind_left <- matrix_all[ inndx, "indexes"]
  ind_right <- matrix_all[ !inndx, "indexes"]

  # Create temporary copy of the global probability matrix
  Probability_matrix_temp <- Probability_matrix

  # Calculate probability of left child and insert them into temporary probability matrix
  probability_l <- CalcProb( matrix_left, "target", if( is.null(weights) ){ NULL }else{ weights[ind_left] }, cost )
  Probability_matrix_temp[ ind_left, -ncol(Probability_matrix_temp)] <- matrix( probability_l, nrow = l_obs, ncol = ncol(Probability_matrix_temp)-1, byrow = T)
  
  # Calculate probability of rifht child and insert them into temporary probability matrix
  probability_r <- CalcProb( matrix_right, "target", if( is.null(weights) ){ NULL }else{ weights[ind_right] }, cost )
  Probability_matrix_temp[ ind_right, -ncol(Probability_matrix_temp)] <- matrix( probability_r, nrow = r_obs, ncol = ncol(Probability_matrix_temp)-1, byrow = T)

  # Check out if both leaves chooses the same class
  if( overfit == "avoid" ){
    
    same_class_split <- AvoidSameClass( probability_l, probability_r, cost )
    
  }else{
    
    same_class_split <- 0
    
  }
  
  # Calculate number of incorrectly classified observation of each child
  if( cp > 0 ){
    
    class_error <- ClassErrorLocal( rbind( probability_r * r_obs, probability_l * l_obs) )
    # class_error <- ClassErrorLocal( rbind( probability_l * l_obs, probability_r * r_obs) )
    
  }else{
    
    class_error <- c( 0, 0 )
    
  }
  
  # Calculate AUC of this split,
  if( attr(Probability_matrix, "k") == 2 ){

    # Binary classification
    auc_measure <- TwoclassAUC( Probability_matrix_temp[, "target"], Probability_matrix_temp[, levelPositive], levelPositive, weights, cost )

  }else{
    
    # Multiclass classification
    auc_measure <- MulticlassAUC( Probability_matrix_temp[, "target"], Probability_matrix_temp[, -ncol(Probability_matrix_temp)], weights, AUCweight, cost )

  }

  # Current AUC of the entire tree
  measure_parent <- Global_AUC
  
  l_prob_peak <- sort( probability_l, T )
  l_prob_peak <- unname( l_prob_peak[1] - l_prob_peak[2] )
  
  r_prob_peak <- unname( sort( probability_r, T ) )
  r_prob_peak <- r_prob_peak[1] - r_prob_peak[2]

  return( c(gain = auc_measure - measure_parent, left_value = auc_measure, right_value = auc_measure, 
            l_class_error = class_error[2], r_class_error = class_error[1], same_class = same_class_split,
            l_prob_peak = l_prob_peak, r_prob_peak = r_prob_peak ) )

}
