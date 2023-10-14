#' Generate a random string vector
#'
#' @param n The number of items in the vector. Default is set to 500.
#' @param length the number of characters in a string. Default is set to 6.
#' @param characters A vector containing the characters to include. Default is all lowercase, all, uppercase letters and all numbers.
random_string_vector <- function(n = 500, length = 6, characters = c(letters, LETTERS, 0:9)) {
  # Generate a random vector with strings of 20 characters
  return(do.call(
    paste0,
    replicate(
      length,
      ## Random letters and numbers are drawn
      sample(
        characters,
        n,
        TRUE
      ),
      FALSE
    )
  ))
}
