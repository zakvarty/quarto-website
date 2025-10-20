pad_with_NAs <- function (x, n_left, n_right) {

  stopifnot(n_left >= 0)
  stopifnot(n_right >= 0)
  stopifnot(class(x) %in% c("character", "complex", "integer",
                            "logical", "numeric", "factor"))

  c(rep(NA, n_left), x, rep(NA, n_right))
}
