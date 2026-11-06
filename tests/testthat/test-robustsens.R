test_that("contaminate corruots the right number of measurements", {
  set.seed(1)
  y <- contaminate(simulate_spo2(100), 0.05)
  expect_length(y, 100)
  expect_equal(sum(y == 60), 5)
})

test_that("contaminate rejects invalid proportions", {
  expect_error(contaminate(1:10, 1.5))
})

test_that("robust estimators resist outlier", {
  x <- c(rep(97, 9), 60)
  expect_lt(mean(x), 94)
  expect_equal(median(x), 97)
  expect_equal(trimmed_mean(x), 97)
  expect_equal(huber_loc(x), 97)
})

test_that("sensitivity of the mean is straight line", {
  x <- simulate_spo2(50)
  sc <- sensitivity_curve(x, mean, grid = c(60, 97, 110))
  expect_equal(sc$sensitivity, sc$x - mean(x))
})
