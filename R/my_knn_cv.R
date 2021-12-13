#' k-Nearest Neighbors Cross-Validation function
#' 
#' This function perform k-Nearest Neighbors cross-validation for input data.
#' 
#' @param train input data frame that is being used to train
#' @param cl True class value of your training data
#' @param k_nn Integer representing the number of neighbors
#' @param k_cv Integer representing the number of folds
#'  
#' @import magrittr stats dplyr tidyverse class
#' 
#' @return Numeric Lists of objects that consists: class, which is a vector of
#'   the predicted class Yhat for all observations. And cv_err, which is a
#'   numeric with the cross-validation classifications error.
#'
#'
#' @examples
#' my_penguins <- my_penguins[, c("species", "bill_length_mm", "bill_depth_mm",
#'  "flipper_length_mm", "body_mass_g")]
#' my_penguins <- na.omit(my_penguins)
#' my_knn_cv(train = my_penguins[,2:5], cl = my_penguins$species, k_nn = 1, k_cv = 5)
#' my_knn_cv(train = my_penguins[,2:5], cl = my_penguins$species, k_nn = 5, k_cv = 5)
#' 
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv){
  
  # Split data in k_cv parts, randomly
  fold <- sample(rep(1 : k_cv, length = nrow(train)))
  
  # Use data from before
  data <- data.frame("x" = train, "y" = cl, "split" = fold)
  misclass_rate <- vector(mode = "numeric", length = k_cv)
  
  
  for (i in 1 : k_cv) {
    
    data_train <- data %>% dplyr::filter(split != i)
    data_test  <-  data %>% dplyr::filter(split == i)
    
    cl_train <- as.factor(data_train$y)
    cl_test <- as.numeric(data_test$y)
    
    # Make a prediction for this data set
    predictions <- class::knn(train = data_train[,1:4], test  = data_test[,1:4], cl = cl_train, k = k_nn)
    
    # compute and record mis classification rate
    misclass_rate[i] <-  mean(data_test$y != predictions)
  }
  
  class <- class::knn(train = data[,1:4], test  = data[,1:4], cl = cl, k = k_nn)
  cv_error <- mean(misclass_rate)
  
  # store output as a list
  output <- list("class" = class, "cv_err" = cv_error)
  
  return(output)
}
