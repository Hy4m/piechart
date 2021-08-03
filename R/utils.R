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
