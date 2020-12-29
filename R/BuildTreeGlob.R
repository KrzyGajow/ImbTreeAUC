BuildTreeGlob <- function( tree, Y_name, X_names, data, depth, min_obs, type, levelPositive, 
                           cp, n_cores, weights, AUCweight, cost, class_th, overfit, number ){

  # Main loop until there is no possible split
  k <- 1
  depth_max <- c()
  repeat{

    # Travers all leaves in the tree to find splits
    leaves_results <- TraversTree( tree, Y_name, X_names, data, depth, min_obs, type, levelPositive, cp, n_cores, weights, AUCweight, cost, overfit )

    # Choose only possible splits
    leaves_results <- PossibleSplitTravers( tree, leaves_results, depth, cp )

    # Check if algorithm finished
    if( nrow(leaves_results) == 0 ) break

    # Choose best split
    split_rule <- BestSplitTravers( leaves_results )

    # Choose node to split
    temp <- NodetoSplit( tree, split_rule )
    leave_path <- temp$leave_path

    # Prepare indexes of observations for each child, name is different for numerical or nominal attribute
    node_name <- PrepareNamesGlob( leave_path, temp, split_rule, data )
    split_indexes <- node_name[[ 1 ]]
    l_name <- node_name[[ 2 ]]
    r_name <- node_name[[ 3 ]]

    # Update Global_AUC value
    Global_AUC <<- split_rule[, "value_left"]

    # Create left child
    CreateLeafGlob( leave_path, l_name, temp$split_var, split_rule, split_indexes, data, Y_name, weights, cost, 
                    class_th, "l_class_error", 2 * leave_path$Number )

    # Create right child
    CreateLeafGlob( leave_path, r_name, temp$split_var, split_rule, !split_indexes, data, Y_name, weights, cost, 
                    class_th, "r_class_error", 2 * leave_path$Number + 1 )
    
    depth_max <- max( depth_max, leave_path$depth + 1 )
    # cat( sprintf( " \r Tree depth: %s, number of leaves: %s ", depth_max, length( tree$Get("pathString", filterFun = isLeaf ) ) ) )
    
    k <- k + 1
    
  }
  
  # Adjustment for the above printing
  # cat( "\n" )
  
}

TraversTree <- function( tree, Y_name, X_names, data, depth, min_obs, type, levelPositive, cp, n_cores, weights, AUCweight, cost, overfit ){

  # Create global object (temporary) with results
  assign( "AUC_Traversed_Tree", NULL, envir = .GlobalEnv )

  # Function traversing the tree and reaching all posibble leaves
  fun <- function( tree, Y_name, X_names, data, depth, min_obs, type, levelPositive, cp, n_cores, weights, AUCweight, cost, overfit ){

    # If leaf calculate best split
    if( tree$isLeaf ){

      # Check if split is possible in terms of control parameters
      if( {tree$Count / 2 > min_obs} & !all( tree$Probability %in% c(0,1) ) ){

        # Calculate various statistics of all possible best local splits, choose the best one
        split_rule <- BestSplitGlobal( data[ tree$indexes, c(X_names, Y_name) ], Y_name, tree$measure, min_obs, "AUCg", levelPositive, cp, 
                                       tree$indexes, n_cores, weights, AUCweight, cost, overfit )

        # Add depth of the tree to the results
        split_rule <- cbind( split_rule, depth = tree$depth + 1 )

        # Update object with results
        AUC_Traversed_Tree <<- rbind( AUC_Traversed_Tree, split_rule )
        
        # Assign path to the node
        rownames(AUC_Traversed_Tree)[length(rownames(AUC_Traversed_Tree))] <<- paste0(tree$pathString,"/", rownames(split_rule))

      }

      return( invisible(NULL) )

    }
    
    # Travers left child of the node
    child_left <- tree$children[[ 1 ]]
    fun( child_left, Y_name, X_names, data[, c(X_names, Y_name) ], depth, min_obs, type, levelPositive, cp, n_cores, weights, AUCweight, cost, overfit )

    # Travers right child of the node
    child_right <- tree$children[[ 2 ]]
    fun( child_right, Y_name, X_names, data[, c(X_names, Y_name) ], depth, min_obs, type, levelPositive, cp, n_cores, weights, AUCweight, cost, overfit )
  
  }

  # Run above function
  fun( tree, Y_name, X_names, data, depth, min_obs, type, levelPositive, cp, n_cores, weights, AUCweight, cost, overfit )

  # Create output
  out <- AUC_Traversed_Tree

  # Remove global object (temporary) with results
  rm("AUC_Traversed_Tree", envir = .GlobalEnv)
  
  return( out )

}

CreateLeafGlob <- function( leave_path, name, split_var, split_rule, split_indexes, data, Y_name, weights, cost, class_th, class_error, number ){

  #Create and update leaf info
  child <- leave_path$AddChild(name)

  # Assign splitting attribute
  child$feature <- split_var

  # Assign splitting value
  child$value <- split_rule[, "split"]

  # Assign measure of this node
  child$measure <- split_rule[, "value_left"]

  # Store observation indexes
  child$indexes <- leave_path$indexes[split_indexes]

  # Assign depth of the tree
  child$depth <- leave_path$depth + 1

  # Calculate number of observations
  child$Count <- length(child$indexes)

  # Assign leaf flag
  child$Leaf <- "*"

  # Probability of the node
  probability <- CalcProb( data[child$indexes,], Y_name, if( is.null(weights) ){ NULL }else{ weights[child$indexes] }, cost )

  # Assign probability to the node
  child$Probability <- probability

  # Assign class to the node
  child$Class <- ChooseClass( probability, class_th, cost )

  # Assign node number
  child$Number <- number

  # Assign local error needed for cp
  child$localerror <- split_rule[,class_error]
  
  # Update Global Probability matrix
  Probability_matrix[child$indexes, -ncol(Probability_matrix)] <<- matrix(probability, nrow = length(child$indexes), ncol = ncol(Probability_matrix)-1, byrow = T )

}

NodetoSplit <- function( tree, split_rule ){

  # Determine path to the node
  temp <- strsplit( rownames(split_rule)[1], "/" )[[1]]
  split_var <- temp[length(temp)]

  # Adjustment for root
  if( length(temp) == 2 ){

    leave_path <- eval( parse(text = deparse( substitute(tree) ) ) )

  }else{

    leave_path <- eval( parse( text = paste( deparse( substitute(tree) ), paste0( paste0( "'", temp[-c(1, length(temp))] ), "'", collapse = "$" ), sep = "$") ) )

  }

  #Remove leaf flag
  leave_path$Leaf <- ""

  return( list( leave_path = leave_path, split_var = split_var ) )

}

NodetoTake <- function( tree, split_rule ){
  
  # Determine path to the node
  rownames(split_rule) <- gsub( "\\.[[:digit:]]{1,5}$", "", rownames(split_rule) )
  temp <- strsplit( rownames(split_rule)[1], "/" )[[1]]
  
  # Adjustment for root
  if( length(temp) == 2 ){
    
    leave_path <- eval( parse(text = deparse( substitute(tree) ) ) )
    
  }else{
    
    leave_path <- eval( parse( text = paste( deparse( substitute(tree) ), paste0( paste0( "'", temp[-c(1, length(temp))] ), "'", collapse = "$" ), sep = "$") ) )
    
  }
  
  return( leave_path )
  
}

PossibleSplitTravers <- function( tree, leaves_results, depth, cp ){

  # Check which splits are possible in terms of control parameters, if none create dummy table with results
  if( is.null(leaves_results) ){

    leaves_results <- matrix(0, 0, 10)

  }else{
    
    if( cp > 0 ){
      
      no_improvement <- double( nrow(leaves_results) )

      for( i in 1:nrow(leaves_results) ){
        
        leaf <- NodetoTake( tree, leaves_results[i,] )
        no_improvement[i] <- IfImprovement( leaf, leaves_results[i,], cp )

      }

      which_possible <- !no_improvement & leaves_results[, "depth"] <= depth
      leaves_results <- leaves_results[ which_possible, , drop = F]
      
    }else{
      
      which_possible <- leaves_results[, "value"] > 0 & leaves_results[, "depth"] <= depth
      leaves_results <- leaves_results[ which_possible, , drop = F ]
      
    }

  }

  return( leaves_results )

}

BestSplitTravers <- function( leaves_results ){

  # Choose the best split
  index_best <- which( max(leaves_results[, "value"]) == leaves_results[, "value"] )

  # If there are more than one best split, choose the one producing the best balanced split
  index_best <- index_best[ which.min(leaves_results[index_best, "balance"]) ]
  split_rule <- leaves_results[ index_best, , drop = F]

  return( split_rule )

}

PrepareNamesGlob <- function( leave_path, temp, split_rule, data ){
  
  if_fac <- length( grep("\\(", split_rule[, "split"]) ) > 0
  if( if_fac ){
    
    fac_rul <- split_rule[,"split"]
    fac_rul <- eval( parse( text = paste0( "c(",paste0( paste0( "'", strsplit(substr( fac_rul, 2, nchar(fac_rul)-1 ), "," )[[1]] ), "'", collapse = "," ), ")") ) )
    split_indexes <- data[ leave_path$indexes, temp$split_var] %in% fac_rul
    l_name <- sprintf("%s = %s", temp$split_var, split_rule[, "split"])
    r_name <- sprintf("%s = %s", temp$split_var, split_rule[, "split_rest"])
    
  }else{
    
    split_indexes <- data[ leave_path$indexes, temp$split_var] <= ( eval( parse( text = split_rule[,"split"]) ) )
    l_name <- sprintf("%s <= %s", temp$split_var, split_rule[, "split"])
    r_name <- sprintf("%s >  %s", temp$split_var, split_rule[, "split"])
    
  }
  
  return( list( split_indexes = split_indexes, l_name = l_name, r_name = r_name ) )
  
}
