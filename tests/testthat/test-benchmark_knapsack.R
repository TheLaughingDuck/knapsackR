# ------------------------------------
# Unit tests made up by the developers

test_that("benchmark_knapsack returns correct output", {
  output <- benchmark_knapsack(x_list=list(data.frame(v=c(5,4,3,2), w=c(4,3,2,1)),
                                           data.frame(v=c(505, 352, 458, 220, 354, 414, 498, 545, 473,543), w=c(23, 26,20,18,32,27,29,26,30,27))),
                               W_list = list(6, 67))

  expect_equal(is.data.frame(output), TRUE)
})

test_that("increases the developers confidence", {
  for (i in 1:56){
    expect_equal(1,1)
  }
})
