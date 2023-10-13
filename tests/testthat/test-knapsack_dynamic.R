# Data setup
suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)


# --------------------------------------------------------------------
# Unit tests based on pre-made unit tests from the other two functions

test_that("Correct object is returned", {
  expect_silent(gk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500))
  expect_named(gk, c("value", "elements"))
})

test_that("functions rejects errounous input.", {
  expect_error(knapsack_dynamic("hej", 3500))
  expect_error(knapsack_dynamic(x = knapsack_objects[1:8,], W = -3500))
})

test_that("Function return correct results.", {
  gk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(gk$value), 16770)
  expect_true(all(round(gk$elements) %in% c(5, 8)))

  gk <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(gk$value), 16770)
  expect_true(all(round(gk$elements) %in% c(5, 8)))

  gk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))

  gk <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))

  st <- system.time(gk <- knapsack_dynamic(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] <= 0.01)

  # gk <- knapsack_dynamic(x = knapsack_objects[1:800,], W = 3500)
  # expect_equal(round(gk$value), 192647)
  #
  # gk <- knapsack_dynamic(x = knapsack_objects[1:1200,], W = 3500)
  # expect_equal(round(gk$value), 270290)
})
# --------------------------------------------------------------------

# ------------------------------------
# Unit tests made up by the developers

test_that("knapsack_dynamic returns correct output",{
  output <- expect_error(knapsack_dynamic(iris, 2))
  #expect_true(is.null(output$value) == FALSE & is.null(output$elements) == FALSE)
})

test_that("knapsack_dynamic wikipedia example works",{
  output <- knapsack_dynamic(data.frame(v=c(5,4,3,2), w=c(4,3,2,1)), W = 6)

  expect_true(all(output$value == 9 & output$elements %in% c(4,3,2)))
})
# ------------------------------------
