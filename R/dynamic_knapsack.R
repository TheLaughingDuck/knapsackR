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
  stopifnot("Incorrect columns in argument \"x\": should contain columns \"v\" and \"w\"" = all(colnames(x) %in% c("v", "w")))
  stopifnot("Negative values in argument \"x\", column \"v\"" = all(x$v > 0))
  stopifnot("Negative values in argument \"x\", column \"w\"" = all(x$w > 0))
  stopifnot("Not all object weights (column \"w\") in argument \"x\" are integers (as required by dynamic_knapsack)" = all(x$w %% 1 == 0))

  # Check W argument
  stopifnot("argument \"W\" is not numeric" = is.numeric(W))
  stopifnot("maximum weight is not a positive integer (as required)" = all(W %% 1 == 0 & W > 0))
  # ---^--- CHECK INPUT ---^---

  # Construct a matrix with default zeroes, intended to hold the optimal value
  # that can be achieved when using up to the i:th object and the max weight is j.
  h_matrix <- matrix(0, nrow=nrow(x)+1, ncol=W+1, dimnames = list(c(0:nrow(x)), c(0:W)))

  # Fill in the matrix
  for (i in 1:nrow(x)){
    for (j in 1:W){
      if (x$w[i] > j){
        # Calc ...
        h_matrix[1+i, 1+j] <- h_matrix[1+i-1, 1+j]
      }
      else {
        # Calc ...
        a <- h_matrix[1+i-1, 1+j]
        b <- h_matrix[1+i-1, 1+j-x$w[i]]+x$v[i]

        h_matrix[1+i, 1+j] <- max(a, b)
      }
    }
  }

  # Format and return output
  output <- list("value" = h_matrix[1+nrow(x), 1+W],
                 "elements" = find_elements(nrow(x), W, h_matrix, x))

  return(output)
}

# Function that finds the elements for a given optimal value.
find_elements <- function(i,j, matrix, x){
  if(i == 0){
    return(c())
  }
  if(matrix[1+i, 1+j] > matrix[1+i-1, 1+j]){
    return(c(i, find_elements(i-1, j-x$w[i], matrix, x)))
  }
  else{return(find_elements(i-1, j, matrix, x))}
}

