# within test-my_t.test.R
data <- rnorm(10, mean = 20, sd = 1)

test_that("unacceptable `alternative` throws error", {
  expect_error(my_t.test(x, alternative = "abc", mu = 0))
})


test_that("my_t.test works mathematically", {
  expect_equal(my_t.test(c(1:120), "two.sided", 60)[["test_stat"]], 0.157459164)
})

