#' The `dynamic_knapsack` function
#'
#' @description This function solves the knapsack problem using a method of dynamic programming.
#'
#' @param x A dataframe containing the objects that can go in the knapsack.
#' It has two variables `v` indicating the value of the object, and `w` indicating the weight of the object. For the dynamic solution, the weights must be positive integers.
#'
#' @param W A numeric (must be positive integer) indicating the size of the knapsack. In other words: the maximum weight that the knapsack can support.
#'
#' @return A list of $value: the maximum knapsack value (numeric), and $elements: The rows in `x` that achieve this value, in the form of a vector.
#'
#' @source [Dynamic programming algorithm from the wikipedia article on the Knapsack problem](https://en.wikipedia.org/wiki/Knapsack_problem#0-1_knapsack_problem)
#'
#' @importFrom r2r hashmap
#'
#' @export dynamic_knapsack


dynamic_knapsack <- function(x, W){

  # ---V--- CHECK INPUT ---V---
  # Check x argument
  stopifnot("argument \"x\" is not data.frame" = is.data.frame(x))
  stopifnot("not all object weights are positive integers (as required)" = all(x$w %% 1 == 0 & x$w > 0))

  # Check W argument
  stopifnot("argument \"W\" is not numeric" = is.numeric(W))
  stopifnot("maximum weight is not a positive integer (as required)" = all(W %% 1 == 0 & W > 0))
  # ---^--- CHECK INPUT ---^---

  # Construct a value hashmap where value_hashmap[[c(i,j)]] is
  # the optimal value that can be achieved when using up to the i:th object and the max weight is j.
  value_hashmap <- m(nrow(x), W, hashmap(default=-1), x)

  # Format and return output
  output <- list("value" = value_hashmap[[c(nrow(x), W)]],
                 "elements" = find_elements(nrow(x), W, value_hashmap, x))

  # Verify that these elements indeed achieve the optimal value
  #if(x$v[output$elements] %>% sum() != output$value){cat("ERROR")}
  #else{cat("Elements verified.\n")}

  return(output)
}


# Recursive function for calculating the optimal value
# value must be an empty hashmap with default value -1.
# -1 indicates that a position has not been visited before.
m <- function(i, j, value, x){
  # Check if the edges have been reached
  if(i == 0 | j <= 0){return(0)} # value[i,j] = 0

  # Check if a previously unknown position is encountered
  if(value[[c(i-1, j)]] == -1){
    value[[c(i-1, j)]] <- m(i-1, j, value, x)
  }

  # Check if object i is too big for the knapsack
  if(x$w[i] > j){
    value[[c(i,j)]] <- value[[c(i-1,j)]]
  }
  else{
    # Check if a previously unknown position is encountered
    if(value[[c(i-1, j-x$w[i])]] == -1){
      #cat("Call m again", "\n")
      value[[c(i-1, j-x$w[i])]] <- m(i-1, j-x$w[i], value, x)
    }
    # Store a new value at this position (i)
    value[[c(i,j)]] <- max(value[[c(i-1,j)]], value[[c(i-1, j-x$w[i])]]+x$v[i])
  }
  # Return final hashmap (if finished, otherwise keep going)
  if(i==nrow(x)){return(value)}
  else{return(value[[c(i,j)]])}
}

# Function that finds the elements for a given optimal value.
find_elements <- function(i,j, value, x){
  if(i == 0){
    return(c())
  }
  if(value[[c(i,j)]] > value[[c(i-1, j)]]){
    return(c(i, find_elements(i-1, j-x$w[i], value, x)))
  }
  else{return(find_elements(i-1, j, value, x))}
}

