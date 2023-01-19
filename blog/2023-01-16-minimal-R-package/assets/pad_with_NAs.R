#' Add NAs to a vector
#'
#' @param x vector to which NAs will be added
#' @param n_left number of NAs to add before x
#' @param n_right number of NAs to add after x
#'
#' @return a vector containing x with the requested number of NA values before and after
#'
#' @examples
#' pad_with_NAs(c("spider", "mouse", "cat", "dog"), n_left = 1, n_right = 2)
#' pad_with_NAs(1:5, n_left = 0, n_right = 3)
#'
pad_with_NAs <- function(x, n_left, n_right){
  c(rep(NA, n_left), x, rep(NA, n_right))
}
