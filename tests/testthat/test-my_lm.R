# within test-my_lm.R
sample <- mtcars

test_that("my_lm works mathematically", {
  expect_equal(my_lm(mpg ~ hp, data = sample)[1, 1], 30.09886, tolerance = 1e-05)
})

