#' The `knapsack_dynamic` function
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
#' @export knapsack_dynamic


knapsack_dynamic <- function(x, W){
  # Log start time for output
  start_time <- Sys.time()

  # ---V--- CHECK INPUT ---V---
  # Check x argument
  stopifnot("argument \"x\" is not data.frame" = is.data.frame(x))
  stopifnot("not all object weights are positive integers (as required)" = all(x$w %% 1 == 0 & x$w > 0))

  # Check W argument
  stopifnot("argument \"W\" is not numeric" = is.numeric(W))
  stopifnot("maximum weight is not a positive integer (as required)" = all(W %% 1 == 0 & W > 0))
  # ---^--- CHECK INPUT ---^---


  # Create matrix
  m <- matrix(rep(NA, ((nrow(x)+1)*(W+1))), nrow=nrow(x)+1, dimnames = list(0:nrow(x), 0:W))
  m[1,] <- rep(0, W+1)

  # Loop
  # these i and w, they represent the number of objects, and the current weight, respectively
  # Thus some adjustments for index purposes must be made.
  i <- 1 # number of objects. *not* position in m.
  w <- 0 # current weight limit
  while(i <= nrow(x)){
    while(w <= W){
      if (x$w[i] > w){
        m[i+1, w+1] <- m[i, w+1]
      }
      else {
        m[i+1, w+1] <- max(m[i, w+1], m[i, w+1-x$w[i]]+x$v[i])
      }
      #cat("Calculated m[i:", i, ", w:", current_w_limit, "] = ", m[[c(i, current_w_limit)]], "\n")
      w <- w + 1
    }
    i <- i + 1
    w <- 0
    }

  #cat("Matrix from the dynamic programming:\n")
  #print.default(m)

  # Format and return output
  output <- list("value" = m[nrow(x)+1, W+1],
                 "elements" = c(1,2,3),
                 "time" = Sys.time() - start_time)
  return(output)
}

