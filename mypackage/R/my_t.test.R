#' t-test function
#' 
#' This function computes t_test statistics for input data.
#' 
#' @param x Numeric vector of data to be calculated
#' @param alternative Character string specifying the alternative hypothesis for
#'   the test.
#' @param mu Number indicating the null hypothesis value of the
#'   mean of the test.
#'  
#' @return Numeric Lists of elements that calculated from t-test for the input data
#'   \code{x} that consists the test_stat indicating the t value; df indicating 
#'   degrees of freedom; alternative indicating the value of the parameter alternative
#'   ; and p_val indicating the numeric p-value.
#'
#' @import stats
#' @examples 
#' data <- rnorm(10, mean = 20, sd = 1)
#' my_t.test(x = data, 'two.sided', 20)
#' my_t.test(x = data, 'less', 20)
#' 
#' @export
my_t.test <- function(x, alternative, mu) {
  if (!alternative %in% c('two.sided', 'less', 'greater')) {
    # Stop if the input 'alternative' is not valid
    stop("Please enter a valid input :)")
  }
  n <- length(x)
  test_stat <- (mean(x) - mu) / (sd(x) / sqrt(n))
  p_val <- 2 * (1 - stats::pt(test_stat, df = n - 1, lower.tail = TRUE))
  
  #change the default p_val if needed
  if (alternative == 'less') {
    p_val <- stats::pt(test_stat, df = n - 1, lower.tail = TRUE)
  } else if (alternative == 'greater') {
    p_val <- pnorm(test_stat, lower.tail = FALSE)
  }
  
  return(list("test_stat" = test_stat, "df" = n - 1, "alternative" = alternative,
              "p_val" = p_val))
}
