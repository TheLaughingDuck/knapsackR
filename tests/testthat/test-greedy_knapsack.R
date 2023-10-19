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
  expect_silent(gk <- greedy_knapsack(x = knapsack_objects[1:8,], W = 3500))
  expect_named(gk, c("value", "elements"))
})

test_that("functions rejects errounous input.", {
  expect_error(greedy_knapsack("hej", 3500))
  expect_error(greedy_knapsack(x = knapsack_objects[1:8,], W = -3500))
})

test_that("Function return correct results.", {
  gk <- greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))

  gk <- greedy_knapsack(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))

  gk <- greedy_knapsack(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))

  gk <- greedy_knapsack(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))

  st <- system.time(gk <- greedy_knapsack(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] <= 0.01)

  gk <- greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
  expect_equal(round(gk$value), 192647)

  gk <- greedy_knapsack(x = knapsack_objects[1:1200,], W = 3500)
  expect_equal(round(gk$value), 270290)
})
# --------------------------------------------------------------------

# ------------------------------------
# Unit tests made up by the developers

test_that("the achieved value matches the combined value of elements",{
  output <- greedy_knapsack(knapsack_objects[1:300,], 1002)
  expect_equal(knapsack_objects[1:300,]$v[output$elements] %>% sum(),
               output$value)
})

test_that("the achieved solution does not exceed the weight limit",{
  output <- greedy_knapsack(knapsack_objects[1:300,], 1002)
  expect_true(knapsack_objects[1:300,]$w[output$elements] %>% sum() <= 1002)
})

test_that("wikipedia example works",{
  output <- greedy_knapsack(data.frame(v=c(5,4,3,2), w=c(4,3,2,1)), W = 6)

  expect_true(all(output$value == 9 & output$elements %in% c(4,3,2)))
})
# ------------------------------------

