#' @title Random Words
#' Function to generate random words.
#' @param n number of random words to return.
#' @param max_len,min_len maximum and minimum length of random words.
#' @param lambda positive integer value.
#' @return character vector.
#' @importFrom stats rpois
#' @importFrom purrr map_chr
#' @examples
#' random_char(10)
#' random_char(10, min_len = 3)
#' @export
random_char <- function(n,
                        max_len = 10,
                        min_len = 2,
                        lambda = 5) {
  ll <- rpois(n, lambda)
  ll <- ifelse(ll > max_len, ll %% max_len, ll)
  ll <- ifelse(ll < min_len, min_len, ll)
  purrr::map_chr(ll, function(.n) {
    paste(sample(letters, .n, TRUE), collapse = "")
  })
}
