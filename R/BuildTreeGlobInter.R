BuildTreeGlobInter <- function( tree, Y_name, X_names, data, depth, min_obs, type, levelPositive, cp, n_cores, weights, AUCweight, cost, 
                                class_th, overfit, cf, amb_prob, top_split, var_lev, amb_class, amb_class_freq, number, tree_path ){

  # Main loop until there is no possible split
  k <- 1
  depth_max <- c()
  repeat{

    # Travers all leaves in the tree to find splits
    leaves_results <- TraversTreeInter( tree, Y_name, X_names, data, depth, min_obs, type, levelPositive, 
                                        cp, n_cores, weights, AUCweight, cost, overfit )

    # Choose only possible splits
    leaves_results <- PossibleSplitTraversInter( tree, leaves_results, depth, cp )
    
    # Check if algorithm finished
    if( nrow(leaves_results) == 0 ) break
    
    # Create real Global AUC and Probability matrix
    realGlobal_AUC <- Global_AUC
    realProbability_matrix <- Probability_matrix
    
    # Start Interactive Learning procedure if required
    if( nrow(leaves_results) !=0 ){
      
      class_prob_learn <- ClassProbLearnGlob( tree, amb_class, amb_class_freq, leaves_results, tree$root$Count, tree$root$Probability )

      split_rule <- InteractiveLearningGlob( leaves_results, tree, Y_name, X_names, data, depth, min_obs, type, levelPositive, cp, n_cores, 
                                             weights, AUCweight, cost, class_th, overfit, cf, amb_prob, top_split, var_lev, class_prob_learn, 
                                             number, tree_path )

      # If decision was made, update the decision number
      if( attr( split_rule, "Decision" ) ){
        
        Decision <<- get( "Decision", envir = .GlobalEnv ) + 1
        
      }else{
        
        Decision <- ""
        
      }
      
    }
    
    # Reset global objects
    Global_AUC <<- realGlobal_AUC
    Probability_matrix <<- realProbability_matrix
    
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
    CreateLeafGlobInter( leave_path, l_name, temp$split_var, split_rule, split_indexes, data, Y_name, weights, cost, 
                         class_th, "l_class_error", 2 * leave_path$Number, Decision )

    # Create right child
    CreateLeafGlobInter( leave_path, r_name, temp$split_var, split_rule, !split_indexes, data, Y_name, weights, cost, 
                         class_th, "r_class_error", 2 * leave_path$Number + 1, Decision )
    # print(tree)
    depth_max <- max( depth_max, leave_path$depth + 1 )
    # cat( sprintf( " \r Tree depth: %s, number of leaves: %s ", depth_max, length( tree$Get("pathString", filterFun = isLeaf ) ) ) )
    
    k <- k + 1
    
  }
  
  # Adjustment for the above printing
  # cat( "\n" )
  
}

TraversTreeInter <- function( tree, Y_name, X_names, data, depth, min_obs, type, levelPositive, cp, 
                              n_cores, weights, AUCweight, cost, overfit ){
  
  # Create global object (temporary) with results
  assign( "AUC_Traversed_Tree", NULL, envir = .GlobalEnv )
  
  # Function traversing the tree and reaching all posibble leaves
  fun <- function( tree, Y_name, X_names, data, depth, min_obs, type, levelPositive, cp, n_cores, weights, AUCweight, cost, overfit ){
    
    # If leaf calculate best split
    if( tree$isLeaf ){
      
      # Check if split is possible in terms of control parameters
      if( {tree$Count / 2 > min_obs} & !all( tree$Probability %in% c(0,1) ) ){
        
        # Calculate various statistics of all possible best local splits, choose the best one
        split_rule <- BestSplitGlobalInter( data[ tree$indexes, c(X_names, Y_name) ], Y_name, tree$measure, min_obs, "AUCg", levelPositive, cp, 
                                            tree$indexes, n_cores, weights, AUCweight, cost, overfit )
        
        # Add depth of the tree to the results
        split_rule <- cbind( split_rule, depth = tree$depth + 1 )

        # Update object with results
        AUC_Traversed_Tree <<- rbind( AUC_Traversed_Tree, split_rule )

        # Assign path to the node
        rownames(AUC_Traversed_Tree)[(nrow(AUC_Traversed_Tree)-nrow(split_rule)+1):(nrow(AUC_Traversed_Tree))] <<- paste0(tree$pathString,"/", rownames(split_rule))

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

CreateLeafGlobInter <- function( leave_path, name, split_var, split_rule, split_indexes, data, Y_name, weights, cost, class_th, class_error, number, decision ){
  
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

  # Assign decision number
  child$Decision <- decision

  # Assign local error needed for cp
  child$localerror <- split_rule[,class_error]

  # Update Global Probability matrix
  Probability_matrix[child$indexes, -ncol(Probability_matrix)] <<- matrix(probability, nrow = length(child$indexes), ncol = ncol(Probability_matrix)-1, byrow = T )

}

PossibleSplitTraversInter <- function( tree, leaves_results, depth, cp ){
  
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

ClassProbLearnGlob <- function( tree, amb_class, amb_class_freq, leaves_results, tree_count, tree_prob ){
  
  paths <- unique( unlist( lapply( strsplit( rownames(leaves_results), "/" ), function(x){ paste0(x[-length(x)], collapse = "/") } ) ) )

  results <- double( nrow( leaves_results ) )
  
  for( i in 1:length(paths) ){
    
    node <- NodetoTake( tree, leaves_results[ grep( paths[i], rownames(leaves_results) )[1] ,] )
    node_prob <- node$Count
    node_count <- node$Probability

    # If amb_class is not null check if classes of interest are above the thresholds
    if( !is.null(amb_class) ){
      
      prob_glob <- ( node_prob * node_count ) / ( tree_prob * tree_count )
      # prob_glob or node_prob
      if( any( prob_glob[amb_class] > amb_class_freq ) ){
        
        class_prob_learn <- 1
        
      }else{
        
        class_prob_learn <- 2
        
      }
      
    }else{
      
      class_prob_learn <- 3
      
    }
    
    results[ grep( paths[i], rownames(leaves_results) ) ] <- class_prob_learn
    
  }

  return( results )
  
}
