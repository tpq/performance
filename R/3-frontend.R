#' Performance Measures for Pairs of Variables
#'
#' The \code{performance} function allows users to analyze the performance
#'  measures for pairs of variables. It takes two inputs \code{var1} and
#'  \code{var2}, representing the two variables to be compared.
#'  Users can specify whether to use only pairwise complete observations using
#'  the \code{use_complete_obs} argument (default is \code{FALSE}).
#'  The function utilizes various sub-functions to compute the appropriate
#'  performance measures based on the variable types of
#'  \code{var1} and \code{var2}.
#'
#' @param yhat A vector representing the first variable to be compared.
#' @param y A vector representing the second variable to be compared.
#' @param use_complete_obs Logical. If \code{TRUE}, the function will use only
#'  pairwise complete observations. Default is \code{FALSE}.
#'
#' @return A data frame containing the computed performance measures for the
#'  given pair of variables.
#'
#' @examples
#' # Example usage
#' var1 <- c(1, 2, 3, NA, 5)
#' var2 <- c(3, 2, 1, 1, 0)
#'
#' # Compute performance measures for continuous and binary variables
#' result <- performance(var1, var2, use_complete_obs = TRUE)
#'
#' @export
performance <- function(yhat, y, use_complete_obs = FALSE) {
  var1 <- yhat
  var2 <- y
  var1 <- create_data_object(var1)
  var2 <- create_data_object(var2)

  # if "use_complete_obs" only use pairwise complete measurements
  # (i.e., remove observations where one or both values are NA)
  if (use_complete_obs) {
    na_exists <- var1@na_index | var2@na_index
    cat("Removing", sum(na_exists), "values because of NAs.\n")
    use1 <- var1@processed_data[!na_exists]
    use2 <- var2@processed_data[!na_exists]
  } else{
    use1 <- var1@processed_data
    use2 <- var2@processed_data
  }

  if (var1@variable_type == "continuous" &
      var2@variable_type == "continuous") {
    result <- continuous_x_continuous(use1, use2)
  } else if (var1@variable_type == "continuous" &
             var2@variable_type == "binary") {
    result <- continuous_x_binary(use1, use2)
  } else if (var1@variable_type == "continuous" &
             var2@variable_type == "category") {
    result <- continuous_x_category(use1, use2)
  } else if (var1@variable_type == "binary" &
             var2@variable_type == "continuous") {
    result <- continuous_x_binary(use2, use1)
  } else if (var1@variable_type == "binary" &
             var2@variable_type == "binary") {
    result <- binary_x_binary(use1, use2)
  } else if (var1@variable_type == "binary" &
             var2@variable_type == "category") {
    result <- category_x_category(use1, use2)
  } else if (var1@variable_type == "category" &
             var2@variable_type == "continuous") {
    result <- continuous_x_category(use2, use1)
  } else if (var1@variable_type == "category" &
             var2@variable_type == "binary") {
    result <- category_x_category(use1, use2)
  } else if (var1@variable_type == "category" &
             var2@variable_type == "category") {
    result <- category_x_category(use1, use2)
  } else {
    stop("Combination of variable types not supported.")
  }

  result
}
