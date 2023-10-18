#' The `brute_force_knapsack` function
#'
#' @description This function solves the knapsack problem through the brute force method: It calculates the combined value for each possible combination of objects and returns the best combination that still respects the maximum weight requirement.
#'
#' @param x A dataframe containing the objects that can go in the knapsack.
#' It has two variables `v` indicating the value of the object, and `w` indicating the weight of the object.
#'
#' @param W A numeric indicating the size of the knapsack. In other words: the maximum weight that the knapsack can support.
#'
#' @param parallel FALSE or TRUE is Argument in this parameter
#'
#' @return A list of $value: the maximum knapsack value (numeric), and $elements: The rows in `x` that achieve this value, in the form of a vector.
#'
#' @source [The wikipedia article on the Knapsack problem](https://en.wikipedia.org/wiki/Knapsack_problem)
#'
#' @importFrom parallel detectCores makeCluster clusterExport parLapply stopCluster
#'
#' @export brute_force_knapsack
#'
#'



brute_force_knapsack <- function(x, W, parallel=FALSE) {
  # Check if x is a data.frame with the required structure
  
  if (!is.data.frame(x) &&  (x$v > 0) && (x$w > 0) && ( W > 0 )) {
    stop("Input 'x' must be a data.frame with 'v' and 'w' variables containing positive values.")
  }
  
  
  # Define a local function to compute the knapsack
  compute_knapsack <- function(i) {
    n <- nrow(x)
    max_value <- 0
    selected_items <- numeric(0)
    
    combination <- as.integer(intToBits(i))[1:n]
    total_value <- sum(x$v * combination)
    total_weight <- sum(x$w * combination)
    
    if (total_weight <= W && total_value > max_value) {
      max_value <- total_value
      selected_items <- which(combination == 1)
    }
    
    return(list(value = max_value, elements = selected_items,weight=total_weight))
  }
  
  n <- nrow(x)  # Number of items
  max_value <- 0
  selected_items <- numeric(0)
  
  # Generate all combinations of item indices using binary representation
  combinations <- 0:(2^n - 1)
  
  if (parallel) {
    # Parallelize using multiple cores
    num_cores <- detectCores()
    clusters <- makeCluster(num_cores)
    clusterExport(clusters, "intToBits")
    result <- parLapply(clusters, combinations, compute_knapsack)
    stopCluster(clusters)
    result <- result[sapply(result, function(x) x$weight>0 && x$weight <= W)]
    
    for (i in result) {
      if (i$value > max_value) {
        max_value <- i$value
        selected_items <- i$elements
      }
    }
  } else {
    # Non-parallel version
    for (i in combinations) {
      knapsack_result <- compute_knapsack(i)
      if (knapsack_result$value > max_value) {
        max_value <- knapsack_result$value
        selected_items <- knapsack_result$elements
      }
    }
  }
  return(list(value = round(max_value), elements = selected_items))
}

