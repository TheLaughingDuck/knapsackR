#' The `brute_force_knapsack` function
#'
#' @description This function solves the knapsack problem through the brute force method.
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
#' @importFrom dplyr arrange filter
#'
#' @export brute_force_knapsack


brute_force_knapsack <- function(x, W){

  # ---V--- CHECK INPUT ---V---
  # Check arg1 argument
  stopifnot("argument \"x\" is not data.frame" = is.data.frame(x))

  # Check arg2 argument
  stopifnot("argument \"W\" is not numeric" = is.numeric(W))
  # ---^--- CHECK INPUT ---^---

  # Format and return output
  output <- list("value" = 1,
                 "elements" = c(1,2,3))
  return(output)
}

