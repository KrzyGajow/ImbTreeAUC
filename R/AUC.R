TwoclassAUC <- function( response, predictor, levelPositive, weights = NULL, cost = NULL ){

  # Adjustment for factor since target is represented as 1, 2.... instead of 0, 1, ...
  response <- fifelse( response == levelPositive, 1, 0 )
  # response <- response == levelPositive

  if( is.null(weights) & is.null(cost) ){

    out <- AUCcpp( predictor, response )
      
  }else if( is.null(weights) & !is.null(cost) ){
    
    # Prepare sums of each row
    costCases <- rowSums( cost )
    
    # Prepare temporary vector
    weights <- double( length(response) )
    
    # Assign a particular cost to the corresponding class
    for( i in names(costCases) ){
      
      weights[ response == i ] <- costCases[i]
      
    }
    
    out <- wAUCcpp( predictor, response, weights )
    
  }else if( !is.null(weights) ){
    
    out <- wAUCcpp( predictor, response, weights )
    
  }

  return( out  )

}

MulticlassAUC <- function( response, predictor, weights = NULL, AUCweight = F, cost = NULL ) {

  lvls <- levels(response)
  
  lvls_loc <- match(lvls, response)
  
  if( anyNA(lvls_loc) ){
    
    indicator_missing <- is.na(lvls_loc)
    
    lvls_missing <- lvls[ indicator_missing ]
    lvls_missing <- single_quote( lvls_missing )
    lvls_missing <- paste0( lvls_missing, collapse = ", " )
    
    # Proceed with non-missing levels
    lvls <- lvls[!indicator_missing]
    
  }

  # Caclulate number of non missing target levels
  k <- length(lvls)
  
  # Prepare empty vector for multiple auc
  aucs <- double( ncol( combn( k, 2 ) ) )
  
  # Prepare empty vector for number of observation involved in auc calculation
  numCases <- double( ncol( combn( k, 2 ) ) )

  i <- 1
  for( i_lvl in lvls ){

    cutpoint <- which(lvls == i_lvl)
    j_lvls <- lvls[-seq_len(cutpoint)]

    for( j_lvl in j_lvls ){
      
      # Take only observations from the investigated classes
      which_ij <- ( response %in% c(i_lvl, j_lvl)  )
      
      # Filter and create 0-1 vectors
      tar_i <- response[ which_ij ]
      # tar_i <- fifelse( tar_i == i_lvl, 1, 0 )
      tar_i <- tar_i == i_lvl
      
      tar_j <- response[ which_ij ]
      # tar_j <- fifelse( tar_j == j_lvl, 1, 0 )
      tar_j <- tar_j == j_lvl
      
      if( !is.null(weights) | !is.null(cost) ){

        if( is.null(weights) ){
          
          # Assign weights based on the cost matrix
          weights_ij <- fifelse( tar_i == 1, cost[ rownames(cost) == i_lvl, colnames(cost) == j_lvl ], cost[rownames(cost) == j_lvl, colnames(cost) == i_lvl] )
          
        }else{
          
          # Assign weights based on the filtered weights
          weights_ij <- weights[ which_ij ]
          
        }

        # Calculate weighted auc
        auc_ij <- wAUCcpp( predictor[ which_ij, i_lvl ], tar_i, weights_ij )
        auc_ji <- wAUCcpp( predictor[ which_ij, j_lvl ], tar_j, weights_ij )

        # Caclulate number of observations involved in auc computation
        numCases[i] <- sum( weights_ij )
          
      }else{

        # Calculate auc
        auc_ij <- AUCcpp( predictor[ which_ij, i_lvl ], tar_i )
        auc_ji <- AUCcpp( predictor[ which_ij, j_lvl ], tar_j )
        
        # Caclulate number of observations involved in auc computation
        numCases[i] <- length( tar_i )
          
      }

      # Calculate average auc
      auc_hat <- (auc_ij + auc_ji) / 2
      
      # Update vectors with results
      aucs[i] <- auc_hat

      i <- i + 1
      
    }
    
  }
  
  # Choose weighting method
  if( AUCweight %in% c("bySize", "byCost") ){
    
    auc <- sum(aucs * numCases) / sum(numCases)
    
  }else{
    
    auc <- 2 / ( k * (k-1) ) * sum( aucs )
    
  }

  return( auc )
  
}

wAUCcpp <- function( pred, tar, w ) {
  
  ord <- order(pred)
  return( wAUC_sorted( pred[ord], tar[ord], w[ord] ) )
  
}

AUCcpp <- function( pred, tar ) {
  
  ord <- order(pred)
  return( AUC_sorted( pred[ord], tar[ord] ) )
  
}

single_quote <- function(x) {
  
  encodeString(x, quote = "'", na.encode = FALSE)
  
}
