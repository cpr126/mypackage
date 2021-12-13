#' my_lm function
#' 
#' This function performs a linear model fit for the input data with a formula.
#' 
#' @param formula Formula class object to be used to determine the
#'   symbolic description of the model to be fitted.
#' @param data Input data frame that is used to perform the linear model fit.
#'  
#' @return Table with rows for each coefficient and columns for the Estimate,
#'   Std.Error, t value, and Pr(>|t|) that are calculated by perform linear model
#'   fit by using the data \code{data} and relation determined by \code{formula}.
#'
#' @import magrittr dplyr stats
#'
#' @examples 
#' my_lm(mpg~hp + wt, mtcars)
#' 
#' @export
my_lm <- function(formula, data) {
  X <- model.matrix(formula, data)
  frame <- model.frame(formula, data)
  Y <- model.response(frame)
  
  # Estimated coefficients
  beta_hat <- (solve(t(X) %*% X) %*% t(X) %*% Y)
  
  # degrees of freedom of the given data
  df = nrow(data) - ncol(X)
  
  #calculate sigma square_ hat
  sigma_sq_hat <- sum((Y - (X %*% beta_hat)) ^ 2 / df)
  
  #calculate standard error
  se <- diag(sqrt(sigma_sq_hat * solve(t(X) %*% X)))
  
  #calculate the t score
  t_score <- beta_hat / se
  
  #p value for  two sided t test
  p_val <- 2 * pt(abs(t_score), df, lower.tail = FALSE)
  
  #output
  output <- data.frame("Estimate" = beta_hat, "Std.Error" = se,
                       "t value" = t_score, "Pr(>|t|)" = p_val)
  
  return(output)
}
