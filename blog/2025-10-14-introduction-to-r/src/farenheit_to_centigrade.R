farenheit_to_centigrade <- function(farenheit, round_digits = 0){
  round((farenheit - 32) * 5 / 9, round_digits)
}
