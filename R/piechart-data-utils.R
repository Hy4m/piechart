#' Helper functions for handle piechart data
#' @title Helper Functions
#' @param x a piechart data.
#' @param .angle the angle in radians.
#' @param facing one of clockwise, binding or normal.
#' @param nice_facing logical value.
#' @param percent logical, if TRUE will convert ratio to percent.
#' @param ... extra parameters passed on to \code{dplyr::filter()}.
#' @return a data frame.
#' @rdname helper_funs
#' @importFrom dplyr filter
#' @examples
#' df <- piechart_data(mtcars, aes(value = wt))
#' get_polygon_data(df)
#' get_label_data(df)
#' @export
get_polygon_data <- function(x, ...) {
  stopifnot(is_piechart_data(x))
  .isLabel <- NULL
  dplyr::filter(.data = x, !.isLabel, ...)
}

#' @rdname helper_funs
#' @export
get_label_data <- function(x, ...) {
  stopifnot(is_piechart_data(x))
  .isLabel <- NULL
  dplyr::filter(.data = x, .isLabel, ...)
}

#' @rdname helper_funs
#' @export
calc_text_angle <- function(.angle,
                            facing = "clockwise",
                            nice_facing = TRUE) {
  facing <- match.arg(facing, c("clockwise", "binding", "downward"))
  degree <- (.angle / pi * 180) %% 360
  if(facing == "downward") {
    return(0)
  }

  if(facing == "clockwise") {
    if(isTRUE(nice_facing)) {
      degree <- ifelse(degree > 90 & degree < 270, (degree + 180) %% 360,
                       degree)
    }
  } else {
    degree <- (degree + 270) %% 360
  }
  degree
}


#' @rdname helper_funs
#' @export
filter_generate <- function(...) {
  function(data) {
    dplyr::filter(data, ...)
  }
}

#' @param .label character vector.
#' @param .ratio numeric vector.
#' @param digits integer indicating the number of decimal places.
#' @param wrap symbols.
#' @rdname helper_funs
#' @export
pie_label <- function(.label = NA,
                      .ratio = NA,
                      percent = TRUE,
                      digits = 2,
                      wrap = "(",
                      ...) {
  wrap <- match.arg(wrap, c("(", "[", "{", ""))
  if(all(is.na(.label)) && all(is.na(.ratio))) {
    return(NA)
  }
  if(isTRUE(percent)) {
    .ratio <- round(.ratio * 100, digits = digits)
    .ratio <- switch (wrap,
                      "(" = paste_with_na("(", .ratio , "%)", sep = ""),
                      "[" = paste_with_na("[", .ratio, "%]", sep = ""),
                      "{" = paste_with_na("{", .ratio, "%}", sep = ""),
                      .ratio
    )
  } else {
    .ratio <- round(.ratio, digits = digits)
    .ratio <- switch (wrap,
                      "(" = paste_with_na("(", .ratio , ")", sep = ""),
                      "[" = paste_with_na("[", .ratio, "]", sep = ""),
                      "{" = paste_with_na("{", .ratio, "}", sep = ""),
                      .ratio
    )
  }

  paste_with_na(.label, .ratio, ...)
}


#' @noRd
paste_with_na <- function(...,
                          sep = " ",
                          collapse = NULL,
                          recycle0 = FALSE) {
  ll <- list(...)
  if(length(ll) == 0) {
    return(character(0))
  }

  ll <- lapply(ll, function(.x) {
    .x <- as.character(.x)
    ifelse(is.na(.x), "", .x)
  })

  params <- c(ll, sep = sep, collapse = collapse, recycle0 = recycle0)
  do.call("paste", params)
}
