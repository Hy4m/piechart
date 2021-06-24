#' @title Zoom Piechart data
#' Zoom the specific area according to the conditions.
#' @param data a piechart data.
#' @param ... any conditions.
#' @param zoom numeric, should be greater than 1.
#' @param base_line numeric, should be greater than r0 and less than r1 * zoom.
#' @return a piechart data.
#' @rdname zoom_piechart
#' @importFrom purrr map
#' @importFrom rlang eval_tidy
#' @importFrom rlang quos
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @export
zoom_piechart <- function(data,
                          ...,
                          zoom = 2,
                          base_line = NULL) {
  stopifnot(is_piechart_data(data))
  if(zoom < 1) {
    warning("'zoom' should be larger than 1.")
  }

  args <- rlang::quos(...)
  ids <- purrr::map(args, function(.arg) rlang::eval_tidy(.arg, data = data))
  n <- length(ids)
  if(n == 0) {
    ids <- rep_len(TRUE, nrow(data))
  } else {
    ids <- Reduce("&", ids, init = TRUE)
  }

  if(!any(ids) || zoom == 1) {
    return(data)
  }
  .x <- .y <- .r0 <- .r1 <- NULL
  clss <- class(data)
  nzdata <- dplyr::filter(data, !ids)
  zdata <- dplyr::filter(data, ids)
  if(is.null(base_line)) {
    zdata <- dplyr::mutate(zdata,
                           .r1 = .r1 * zoom,
                           .x  = .x * zoom,
                           .y  = .y * zoom)
  } else {
    zdata <- dplyr::mutate(zdata,
                           .r0 = ifelse(.r0 < base_line, .r0, base_line),
                           .r1 = ifelse(.r1 * zoom > base_line, .r1 * zoom, base_line),
                           .x  = .x * zoom,
                           .y  = .y * zoom)
  }

  data <- dplyr::bind_rows(nzdata, zdata)
  class(data) <- clss
  data
}

