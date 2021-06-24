#' Geom function for draw piechart.
#' @title Pie Geom
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @section Aesthetics:
#' \code{geom_ring()} understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{fill}
#'       \item \code{linetype}
#'       \item \code{size}
#'    }
#' @importFrom ggplot2 geom_polygon
#' @importFrom ggplot2 ggplot_add
#' @rdname geom_pie
#' @export
geom_pie <- function(mapping = NULL,
                     data = NULL,
                     stat = "identity",
                     position = "identity",
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE,
                     ...) {
  structure(.Data = list(mapping = mapping,
                         data = data,
                         stat = stat,
                         position = position,
                         na.rm = na.rm,
                         show.legend = show.legend,
                         inherit.aes = inherit.aes,
                         ...),
            class = "geom_pie")
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.geom_pie <- function(object, plot, object_name) {
  if(!is_piechart(plot)) {
    stop("`geom_pie()` can only be added on a piechart plot.", call. = FALSE)
  }
  if(is.null(object$data)) {
    data <- plot$data
  } else if(is.function(object$data)) {
    data <- do.call(object$data, list(data = plot$data))
  } else {
    data <- object$data
  }
  stopifnot(is_piechart_data(data))
  .isLabel <- NULL
  object$data <- dplyr::filter(data, !.isLabel)
  object <- do.call(geom_polygon, object)
  ggplot_add(object, plot, object_name)
}
