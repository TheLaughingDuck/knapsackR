#' The `greedy_knapsack` function
#'
#' @description This function solves the knapsack problem using a greedy heuristic approach.
#'
#' @param x A dataframe containing the objects that can go in the knapsack.
#' It has two variables `v` indicating the value of the object, and `w` indicating the weight of the object.
#'
#' @param W A numeric indicating the size of the knapsack. In other words: the maximum weight that the knapsack can support.
#'
#' @return A list of $value: the maximum knapsack value (numeric), and $elements: The rows in `x` that achieve this value, in the form of a vector.
#'
#' @source [The wikipedia article on the Knapsack problem](https://en.wikipedia.org/wiki/Knapsack_problem)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate arrange desc
#' @importFrom rlang .data
#'
#' @export greedy_knapsack


greedy_knapsack <- function(x, W){

  # ---V--- CHECK INPUT ---V---
  # Check arg1 argument
  stopifnot("argument \"x\" is not data.frame" = is.data.frame(x))

  # Check arg2 argument
  stopifnot("argument \"W\" is not numeric" = is.numeric(W))
  stopifnot("argument \"W\" is not positive" = W > 0)
  # ---^--- CHECK INPUT ---^---

  # Sort objects in x by the value per weight
  sorted_x <- x %>%
    mutate(index = 1:nrow(x)) %>%
    mutate(val_per_weight = x$v/x$w) %>%
    arrange(desc(.data$val_per_weight))

  # Create a first solution option by greedily adding objects until knapsack is full
  solution_1 <- c()
  i <- 1
  current_weight <- 0
  while(current_weight + sorted_x$w[i] <= W){
    solution_1 <- c(solution_1, i)
    current_weight <- current_weight + sorted_x$w[i]
    i <- i + 1
  }

  # Create a second solution with
  # the first element that did not fit in solution_1
  if(sorted_x$w[i] < W){solution_2 <- c(i)} else{solution_2 <- c()}

  # Calculate total value of both solutions
  sum1 <- sorted_x[solution_1,]$v %>% sum()
  sum2 <- sorted_x[solution_2,]$v %>% sum()

  # Format and return output
  output <- list()
  if(sum1 > sum2){
    output$value <- sum1
    output$elements <- sorted_x$index[solution_1]}
  else {
    output$value <- sum2
    output$elements <- sorted_x$index[solution_2]}

  return(output)
}

