# The developers made up tests
test_that("knapsack_dynamic returns correct output",{
  output <- knapsack_dynamic(iris, 2)
  expect_true(is.null(output$value) == FALSE & is.null(output$elements) == FALSE)
})
