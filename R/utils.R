#' @noRd
`%||%` <- function(a, b) {
  if(is.null(a)) b else a
}

#' @noRd
aes_modify <- function (aes1, aes2)
{
  aes <- modifyList(as.list(aes1), as.list(aes2))
  class(aes) <- "uneval"
  aes
}

#' @noRd
pretty2 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  bb <- pretty(x)
  bb[bb >= rng[1] & bb <= rng[2]]
}

#' @noRd
expand_c <- function(x) {
  stopifnot(is.numeric(x))
  rng <- range(x, na.rm = TRUE)
  if(diff(rng) == 0) {
    return(c(rng[1] - 0.05, rng[2] + 0.05))
  }
  diff <- diff(rng)
  c(rng[1] - 0.05 * diff, rng[2] + 0.05 * diff)
}
