# within test-my_knn_cv.R

test_that("see k-nearest neighbors cross validation works", {
  
  data <- my_penguins[, c("species", "bill_length_mm", "bill_depth_mm",
                              "flipper_length_mm", "body_mass_g")]
  data <- na.omit(data)
  cl = data$species
  expect_is(my_knn_cv(train = data[,2:5], cl = cl, k_nn = 1, k_cv = 5), "list")
  expect_is(my_knn_cv(train = data[,2:5], cl = cl, k_nn = 5, k_cv = 5), "list")
  
})