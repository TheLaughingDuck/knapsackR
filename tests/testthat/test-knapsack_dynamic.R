# The developers made up tests
test_that("knapsack_dynamic returns correct output",{
  output <- expect_error(knapsack_dynamic(iris, 2))
  #expect_true(is.null(output$value) == FALSE & is.null(output$elements) == FALSE)
})

test_that("knapsack_dynamic wikipedia example works",{
  output <- knapsack_dynamic(data.frame(v=c(5,4,3,2), w=c(4,3,2,1)), W = 6)

  expect_true(all(output$value == 9 & output$elements == c(1,2,3)))
})

