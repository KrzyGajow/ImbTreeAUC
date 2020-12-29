InteractiveLearningGlob <- function( split_rule, tree, Y_name, X_names, data, depth, min_obs, type, levelPositive, cp, n_cores, weights, AUCweight, 
                                     cost, class_th, overfit, cf, amb_prob, top_split, var_lev, amb_class, number, tree_path ){

  # Check if interactive learning should be performed
  stop_cond <- StopCondGlob( split_rule, amb_prob, amb_class )
  split_rule <- BestLeaf( split_rule, stop_cond )

  # Possible splits are above the threshold, no interaction required
  if( all(split_rule$stop_cond == F) ){
    
    # Take the best split
    split_rule <- split_rule[ order(-split_rule[,"value"]), ][ 1, , drop = F ]
    
    # Remove additional numbers from the row names
    rownames(split_rule) <- gsub( "\\.[[:digit:]]{1,5}$", "", rownames(split_rule) )
    attr( split_rule, "Decision" ) <- F
    
    return( split_rule )
    
  }

  # Sort in descending order splits according to the value
  split_rule_inter <- split_rule[order(-split_rule[,"value"]),]

  if( var_lev ){

    # Take best split of each variable
    split_rule_inter <- do.call( "rbind", lapply( split( split_rule_inter, gsub( "\\.[[:digit:]]{1,5}$", "", rownames(split_rule_inter) ) ), 
                                                  function(x){ x[ 1, , drop = F ] } ) )

    # Sort in descending order splits according to the value
    split_rule_inter <- split_rule_inter[order(-split_rule_inter[,"value"]),]
    
    # Take top splits
    split_rule_inter <- split_rule_inter[ 1:min( nrow(split_rule_inter), top_split ),]

  }else{
    
    # Take top splits
    split_rule_inter <- split_rule_inter[ 1:min( nrow(split_rule_inter), top_split ),]
    
  }

  cat("\n\n********** Start Interactive Learning **********\n\n")
  
  # Create output file with the initial tree
  writeLines( capture.output( PrintTreeInter( tree$root ) ), sprintf( "%s/tree%s.txt", tree_path, 0 ) )

  # Create real Global AUC and Probability matrix
  realGlobal_AUC <- Global_AUC
  realProbability_matrix <- Probability_matrix
  
  prune_vec <- c()
  for( i in 1:nrow(split_rule_inter) ){
    
    # Reset global objects
    Global_AUC <<- realGlobal_AUC
    Probability_matrix <<- realProbability_matrix
    
    # Deep clone of the current tree structure
    Tree <- Clone( tree$root )

    temp <- NodetoSplitInter( Tree, split_rule_inter[i,] )
    leave_path <- temp$leave_path

    node_name <- PrepareNamesGlob( leave_path, temp, split_rule_inter[i,], data )
    split_indexes <- node_name[[ 1 ]]
    l_name <- node_name[[ 2 ]]
    r_name <- node_name[[ 3 ]]
    
    # Update Global_AUC value
    Global_AUC <<- split_rule_inter[i, "value_left"]

    # Create left child
    CreateLeafGlobInter( leave_path, l_name, temp$split_var, split_rule_inter[i,], split_indexes, data, Y_name, weights, cost, 
                         class_th, "l_class_error", 2 * leave_path$Number, "*NOW*" )
    
    # Create right child
    CreateLeafGlobInter( leave_path, r_name, temp$split_var, split_rule_inter[i,], !split_indexes, data, Y_name, weights, cost, 
                         class_th, "r_class_error", 2 * leave_path$Number + 1, "*NOW*" )

    # Build Tree
    BuildTreeGlob( Tree, Y_name, X_names, data, depth, min_obs, type, levelPositive, cp, n_cores, weights, 
                   AUCweight, cost, class_th, overfit, number )

    # Prune tree if needed
    if( overfit == "leafcut" ){
      
      prune <- PruneTreeInter( Tree ) 
      
    }else if( overfit == "prune" ){

      prune <- PessimisticErrorPruningInter( Tree, cf )

    }else{
      
      prune <- "YES"
      
    }
    
    prune_vec <- c( prune_vec, prune )

    if( !prune == "NO" ){
      
      # Insert into the original/currnet Tree structure the created subbranches. For display purpose only
      # eval( parse( text = leaf ) )$AddChildNode( node$children[[1]] )
      # eval( parse( text = leaf ) )$AddChildNode( node$children[[2]] )
      
    }

    # Create output file with the possible tree
    writeLines( capture.output( PrintTreeInter( Tree ) ), sprintf( "%s/tree%s.txt", tree_path, i ) )

  }

  # Choose the desired tree
  repeat{
    
    # If there is only on split end
    ANSWER <- 1
    if( nrow(split_rule_inter) == 1 | all( prune_vec == "NO") ) break
    
    # Choose the desired split
    ANSWER <- as.integer( readline( sprintf( "Please choose the tree number from 1 to %s: ", nrow(split_rule_inter) ) ) )
    if( ANSWER %in% 1:nrow(split_rule_inter) ) break
    
  }
  
  if( prune_vec[ANSWER] == "NO" ){
    
    # Prepare dummy table, there is no sense to perform any split
    split_rule <- matrix( 0, 0, 12, dimnames = list( NULL, colnames(split_rule) ) )
    attr( split_rule, "Decision" ) <- F
    
    cat("******* Subtree collapsed: no new split ********")
    
    return( split_rule )
    
  }else{
    
    # Take the chosen one
    split_rule <- split_rule_inter[ANSWER,]
    
    # Remove additional numbers from the row names
    rownames(split_rule) <- gsub( "\\.[[:digit:]]{1,5}$", "", rownames(split_rule) )
    
    if( i == 1 ){
      
      attr( split_rule, "Decision" ) <- F
      cat("*********** There was only one split ***********")
      
    }else{
      
      attr( split_rule, "Decision" ) <- T
      
    }
    
    return( split_rule )
    
  }
  
}

StopCondGlob <- function( split_rule, amb_prob, amb_class ){
  
  cond <- !(amb_class == 3 & split_rule[,"l_prob_peak"] > amb_prob & split_rule[,"r_prob_peak"] > amb_prob)
  cond <- ifelse( amb_class == 2, F, cond )
  cond <- ifelse( amb_class == 1, T, cond )
  
  return( cond )
  
}

NodetoSplitInter <- function( Tree, split_rule ){
  
  # Determine path to the node
  temp <- strsplit( rownames(split_rule)[1], "/" )[[1]]
  split_var <- temp[length(temp)]
  
  # Adjustment for root
  if( length(temp) == 2 ){

    leave_path <- eval( parse(text = deparse( substitute(Tree) ) ) )
    
  }else{
    
    leave_path <- eval( parse( text = paste( deparse( substitute(Tree) ), paste0( paste0( "'", temp[-c(1, length(temp))] ), "'", collapse = "$" ), sep = "$") ) )
    
  }
  
  return( list( leave_path = leave_path, split_var = split_var ) )
  
}

BestLeaf <- function( split_rule, stop_cond ){
  
  leaves <- unlist( lapply( strsplit( rownames(split_rule), "/" ), function(x){ paste0( x[-length(x)], collapse = "/")} ) )
  leaves <- data.frame( leaves = leaves, stop_cond = stop_cond, split_rule )
  leaves <- leaves[ order(-leaves$value),]

  split_rule <- leaves[ leaves$leaves == leaves$leaves[1],-1]

  return( split_rule )
  
}
