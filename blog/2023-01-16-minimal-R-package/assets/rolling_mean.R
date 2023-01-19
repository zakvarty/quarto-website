#' Calculate the rolling mean of a vector
#'
#' @param x vector of values that can be interpreted numerically.
#' @param window_width integer specifying the number of values included in each mean calculation. Should be an odd, positive integer.
#' @param ... Additional arguments to pass to the mean() function call.
#'
#' @return A vector of rolling mean values of the same length as x.
#' @export
#'
#' @examples
rolling_mean <- function(x, window_width, ...){
  # -----Input Checks ----------------------------------------------------------
  # Check that x is a vector with numerical interpretation
  stopifnot(is.logical(x) | is.integer(x) | is.double(x) | is.complex(x))
  stopifnot(length(x) > 0)

  # Check window_width is an odd, positive integer
  stopifnot(length(window_width) == 1)
  stopifnot(window_width %% 1 == 0)
  stopifnot((window_width / 2) %% 1 != 0)
  stopifnot(window_width > 0)

  # ----- Function Body --------------------------------------------------------

  # number of values left and right to include in each mean
  half_width <- floor(window_width / 2)


  x_padded <- pad_with_NAs(x, n_left = half_width, n_right = half_width)
  evaluation_locations <- half_width + seq_along(x)


  output <- rep(NA, length(x))

  for (index in evaluation_locations) {
    # Extract relevant values from x_padded
    indices_in_window <- seq(index - half_width, index + half_width, by = 1)
    values_in_window <- x_padded[indices_in_window]

    # Calculate and store mean
    output[index - half_width] <- mean(values_in_window, ...)
  }

  return(output)
}

## Testing / Examples
rolling_mean(x = 1:5, window_width =  NULL)
rolling_mean(x = NULL, window_width =  3)
rolling_mean(x = "cat", window_width = 3)
rolling_mean(x = 1:5, window_width = 3)

rolling_mean(x = 1:5, window_width = 1)
rolling_mean(x = 1:5, window_width = 3)
rolling_mean(x = 1:5, window_width = 5)
rolling_mean(x = 1:5, window_width = 7)

rolling_mean(x = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE), window_width = 3)

## Development notes to self:
# Do I pick the window width or does the user?
# Is the window centred, left-aligned or right aligned?
# Should window_position be an argument or should this be three separate functions?
# Do I want to pad with NAs or return a shorter vector?
# Do I want to return a vector or a data.frame?
# What happens if the user provides an even number for window_width?
# window_halfwidth

# Will I check that the input is a vector? That is is numeric? Might want to allow non-numeric coercion like lots of other R functions.
# What requirements do I have on window_width relative to x?
# Checking odd, non-negative integer. is.integer() vs. x %% 1 = 0
# https://adv-r.hadley.nz/vectors-chap.html#testing-and-coercion
