# Data setup
suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)


# --------------------------------------------------------------------
# Pre-made unit tests

test_that("Correct object is returned", {
  expect_silent(bfk <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500))
  expect_named(bfk, c("value", "elements"))
})

test_that("functions rejects errounous input.", {
  expect_error(brute_force_knapsack("hej", 3500))
  expect_error(brute_force_knapsack(x = knapsack_objects[1:8,], W = -3500))
})

test_that("Function return correct results.", {
  bfk <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(bfk$value), 16770)
  expect_true(all(round(bfk$elements) %in% c(5, 8)))

  bfk <- brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(bfk$value), 16770)
  expect_true(all(round(bfk$elements) %in% c(5, 8)))

  bfk <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(bfk$value), 15428)
  expect_true(all(round(bfk$elements) %in% c(3, 8)))

  bfk <- brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(bfk$value), 15428)
  expect_true(all(round(bfk$elements) %in% c(3, 8)))

  st <- system.time(bfk <- brute_force_knapsack(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] >= 0.00)
})
# --------------------------------------------------------------------

# ------------------------------------
# Unit tests made up by the developers

test_that("the achieved value matches the combined value of elements",{
  output <- brute_force_knapsack(knapsack_objects[1:16,], 1002)
  expect_equal(knapsack_objects[1:16,]$v[output$elements] %>% sum(),
               output$value)
})

test_that("the achieved solution does not exceed the weight limit",{
  output <- brute_force_knapsack(knapsack_objects[1:16,], 1002)
  expect_true(knapsack_objects[1:16,]$w[output$elements] %>% sum() <= 1002)
})

test_that("wikipedia example works",{
  output <- brute_force_knapsack(data.frame(v=c(5,4,3,2), w=c(4,3,2,1)), W = 6)

  expect_true(all(output$value == 9 & output$elements %in% c(4,3,2)))
})
# ------------------------------------
