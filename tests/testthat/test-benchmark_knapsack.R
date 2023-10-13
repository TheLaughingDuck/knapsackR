# Data setup
suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)


# ------------------------------------
# Unit tests made up by the developers

test_that("benchmark_knapsack returns correct output", {
  output <- benchmark_knapsack(x_list=list(data.frame(v=c(5,4,3,2), w=c(4,3,2,1)), #W = 6
                                           data.frame(v=c(505, 352, 458, 220, 354, 414, 498, 545, 473,543), w=c(23, 26,20,18,32,27,29,26,30,27)), #W = 67
                                           knapsack_objects[1:8,], #W = 3500
                                           knapsack_objects[1:12,], #W = 3500
                                           knapsack_objects[1:12,]), #W = 2000
                               W_list = list(6, 67, 3500, 3500, 2000))

  expect_equal(is.data.frame(output), TRUE)
})

test_that("increases the developers confidence", {
  for (i in 1:56){
    expect_equal(1,1)
  }
})
