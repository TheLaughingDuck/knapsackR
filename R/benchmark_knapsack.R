#' The `benchmark_knapsack` function
#'
#' @description This function runs all three knapsack algorithms (included in this package) on some given dataframes of objects.
#'
#' @param x_list A list of dataframes containing the objects that can go in the knapsacks.
#' Each dataframe has two variables `v` indicating the value of the object, and `w` indicating the weight of the object.
#'
#' @param W_list A list of numerics indicating the size of the knapsacks. In other words: the maximum weights that the respective knapsacks can support.
#'
#' @param name_list An (optional) list of names for the datasets.
#'
#' @return A dataframe with one column each for the brute, dynamic and greedy algorithms. Elements are the optimal value achieved for a particular dataset.
#'
#' @export benchmark_knapsack


benchmark_knapsack <- function(x_list, W_list, name_list=NULL){

  # ---V--- CHECK INPUT ---V---
  # Check x argument
  stopifnot("argument \"x_list\" is not list" = is.list(x_list))
  stopifnot("element 1 in argument \"x_list\" is not data.frame" = is.data.frame(x_list[[1]]))

  # Check W argument
  stopifnot("argument \"W_list\" is not list" = is.list(W_list))
  stopifnot("element 1 in \"W_list\" is not numeric" = is.numeric(W_list[[1]]))
  # ---^--- CHECK INPUT ---^---

  # Names are optional, and will default to numbers 1,2,3,...
  if(is.null(name_list)){name_list <- 1:length(W_list)}

  # Iterate over all datasets and run the knapsack algorithms
  output_df <- data.frame(dataset=c("A"), brute=c(0), dynamic=c(0), greedy=c(0))
  for(i in 1:length(W_list)){
    # Run the algorithms on dataset i
    brute <- brute_force_knapsack(x_list[[i]], W_list[[i]])
    dynamic <- dynamic_knapsack(x_list[[i]], W_list[[i]])
    greedy <- greedy_knapsack(x_list[[i]], W_list[[i]])

    # Store the optimal knapsack solutions for dataset i in the output dataframe
    output_df[nrow(output_df)+1,] <- c(name_list[[i]], brute$value, dynamic$value, greedy$value)
  }

  # Remove default row in output_df
  output_df <- output_df[c(-1),]
  return(output_df)
}

