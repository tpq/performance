#' DataObject Class
#'
#' A class to store data along with its processing information and
#'  variable type.
#'
#' @slot raw_data An object of class "ANY" containing the raw data.
#' @slot processed_data An object of class "ANY" containing the processed data.
#' @slot variable_type A character string indicating the variable type.
#' @slot na_index A logical vector indicating the index of missing values
#'  in the data.
#'
#' @details The `DataObject` class is designed to store data along with its
#'  processing information and variable type. The slot \code{raw_data} can hold
#'  any type of data, and \code{processed_data} stores the data after
#'  processing. The variable type is described by the \code{variable_type} slot,
#'  which can be one of "binary", "category", or "continuous". The
#'  \code{na_index} slot contains a logical vector indicating the index of
#'  missing values in the data.
#'
#' @import methods stats
#' @export
setClass(
  "DataObject",
  slots = list(
    raw_data = "ANY",
    processed_data = "ANY",
    variable_type = "character",
    na_index = "logical"
  )
)

#' Check Variable Type
#'
#' This function takes a vector \code{var} as input and determines
#'  its variable type.
#'
#' @param var A vector containing the data to be checked.
#'
#' @return A character string indicating the variable type. Possible
#'  values are "binary", "category", or "continuous".
#'
#' @details The function performs checks on non-NA values of the input
#'  vector \code{var}. If \code{var} is a character, or factor, it is
#'  considered a "category". If it contains only 0s and 1s, it is considered
#'  "binary". If it is numeric, it is considered "continuous".
#'
#' @examples
#' # Example 1: Binary variable
#' var1 <- c(0, 1, 1, 0, 1)
#' check_variable_type(var1)
#'
#' # Example 2: Categorical variable
#' var2 <- factor(c("A", "B", "A", "C", "B"))
#' check_variable_type(var2)
#'
#' # Example 3: Continuous variable
#' var3 <- c(10.5, 15.2, 12.8, 9.7, 13.4)
#' check_variable_type(var3)
#'
#' @export
check_variable_type <- function(var) {
  # only run checks on non-NA values...
  var <- var[!is.na(var)]

  if (is.logical(var)) {
    return("binary")
  } else if (is.character(var)) {
    return("category")
  } else if (is.factor(var)) {
    return("category")
  } else if (all(var %in% 0:1)) {
    return("binary")
  } else if (is.numeric(var)) {
    return("continuous")
  } else{
    stop("Provided 'var' type not supported.")
  }
}

#' Create DataObject
#'
#' Creates a new instance of the DataObject class based on the input data.
#'
#' @param var The input data, which can be a numeric vector, logical vector,
#'  or character vector.
#'
#' @return An instance of the DataObject class containing the raw data,
#'  processed data, variable type, and a logical vector indicating the index
#'  of missing values in the data.
#'
#' @details The `create_data_object` function creates a new instance of the
#'  DataObject class based on the input data. It checks the type of the data
#'  using the \code{check_variable_type} function, and then processes the data
#'  accordingly. For binary data, missing values are set to 0. For category
#'  data, missing values are set to a new category "NA". For continuous data,
#'  missing values are set to the median of the non-missing values.
#'
#' @examples
#' # Create a DataObject instance
#' data_obj <- create_data_object(c(1, 2, NA, 4, 5))
#'
#' # Access slots
#' raw_data <- data_obj@raw_data
#' processed_data <- data_obj@processed_data
#' variable_type <- data_obj@variable_type
#' na_index <- data_obj@na_index
#'
#' @export
create_data_object <- function(var) {
  obj <- methods::new("DataObject")
  type <- check_variable_type(var)
  na_index <- is.na(var)
  var_clean <- var # make a copy

  if (type == "binary") {
    var_clean <- as.numeric(var_clean)
    if (any(na_index)) {
      #message("NA is set to 0.")
      var_clean[na_index] <- 0
    }

  } else if (type == "category") {
    var_clean <- as.character(var_clean)
    if (any(na_index)) {
      #message("NA is set to its own category.")
      var_clean[na_index] <- "NA"
    }

  } else if (type == "continuous") {
    var_clean <- as.numeric(var_clean)
    if (any(na_index)) {
      #message("NA is set to median.")
      var_clean[na_index] <- stats::median(var_clean, na.rm = TRUE)
    }

  } else{
    stop("Provided 'type' not recognized.")
  }

  obj@raw_data <- var
  obj@processed_data <- var_clean
  obj@variable_type <- type
  obj@na_index <- na_index
  obj
}
