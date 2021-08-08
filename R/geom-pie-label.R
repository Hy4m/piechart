#' Geom function for draw piechart label.
#' @title Pie Label Geom
#' @param ID character.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_text
#' @section Aesthetics:
#' \code{geom_pie_label()} understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \code{alpha}
#'       \item \code{angle}
#'       \item \code{colour}
#'       \item \code{family}
#'       \item \code{fill}
#'       \item \code{fontface}
#'       \item \code{group}
#'       \item \code{hjust}
#'       \item \code{lineheight}
#'       \item \code{size}
#'       \item \code{vjust}
#'    }
#' @importFrom ggplot2 geom_label
#' @importFrom ggplot2 ggplot_add
#' @importFrom grid unit
#' @rdname geom_pie_text
#' @export
geom_pie_label <- function(mapping = NULL,
                           data = NULL,
                           ...,
                           parse = FALSE,
                           nudge_x = 0,
                           nudge_y = 0,
                           label.padding = unit(0.25, "lines"),
                           label.r = unit(0.15, "lines"),
                           label.size = 0.25,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           ID = NULL) {
  structure(.Data = list(mapping = mapping,
                         data = data,
                         parse = parse,
                         nudge_x = nudge_x,
                         nudge_y = nudge_y,
                         label.padding = label.padding,
                         label.r = label.r,
                         label.size = label.size,
                         na.rm = na.rm,
                         show.legend = show.legend,
                         inherit.aes = inherit.aes,
                         ID = ID,
                         ...),
            class = "geom_pie_label")
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.geom_pie_label <- function(object, plot, object_name) {
  if(!is.null(object$data)) {
    if(!is.function(object$data) && !is_piechart_data(object$data)) {
      stop("`data` should be a piechart_data or function.", call. = FALSE)
    }
    if(is_piechart_data(object$data)) {
      .isLabel <- NULL
      data <- dplyr::filter(object$data, .isLabel)
    } else {
      data <- plot$plot_env[[plot$plot_env$last_plot]]
      data <- do.call(object$data, list(data = data))
    }
  } else {
    data <- plot$plot_env[[plot$plot_env$last_plot]]
  }

  stopifnot(is_piechart_data(data))
  object$data <- data
  object <- object[setdiff(names(object), "ID")]
  object <- do.call(geom_label, object)
  ggplot_add(object, plot, object_name)
}
