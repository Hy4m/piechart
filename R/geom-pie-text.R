#' Geom function for draw piechart text.
#' @title Pie Text Geom
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_text
#' @param facing one of clockwise, binding or normal.
#' @param nice_facing logical value.
#' @param position one of middle, top-outside, top-inside, bottom-outside,
#' bottom-inside.
#' @param ID character.
#' @section Aesthetics:
#' \code{geom_pie_text()} understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{family}
#'       \item \code{fontface}
#'       \item \code{lineheight}
#'       \item \code{size}
#'    }
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 ggplot_add
#' @rdname geom_pie_text
#' @export
geom_pie_text <- function(mapping = NULL,
                          data = NULL,
                          ...,
                          parse = FALSE,
                          nudge_x = 0,
                          nudge_y = 0,
                          check_overlap = FALSE,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          facing = "downward",
                          nice_facing = TRUE,
                          position = "middle",
                          ID = NULL) {
  structure(.Data = list(mapping = mapping,
                         data = data,
                         parse = parse,
                         nudge_x = nudge_x,
                         nudge_y = nudge_y,
                         check_overlap = check_overlap,
                         na.rm = na.rm,
                         show.legend = show.legend,
                         inherit.aes = inherit.aes,
                         facing = facing,
                         nice_facing = nice_facing,
                         position = position,
                         ID = ID,
                         ...),
            class = "geom_pie_text")
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.geom_pie_text <- function(object, plot, object_name) {
  if(!is_piechart(plot)) {
    stop("`geom_pie_text()` can only be added on a piechart plot.", call. = FALSE)
  }
  position <- object$position
  position <- match.arg(position, c("middle", "top-outside", "bottom-outside",
                                    "top-inside", "bottom-inside"))

  if(!is.null(object$data)) {
    if(!is.function(object$data) || !is_piechart_data(object$data)) {
      stop("`data` should be a piechart_data or function.", call. = FALSE)
    }
    if(is_piechart_data(object$data)) {
      data <- dplyr::filter(data, .isLabel)
    } else {
      data <- plot$plot_env[[plot$plot_env$last_plot]]
      data <- do.call(object$data, list(data = data))
    }
  } else {
    data <- plot$plot_env[[plot$plot_env$last_plot]]
  }

  stopifnot(is_piechart_data(data))
  .angle <- (data$.angle / pi * 180) %% 360

  vjust <- hjust <- 0.5
  if(position == "top-outside") {
    data$.x <- cos(data$.angle) * data$.r1 * 1.02
    data$.y <- sin(data$.angle) * data$.r1 * 1.02

    if(object$facing == "binding") {
      vjust <- 0
    }
    if(object$facing == "clockwise") {
      hjust <- ifelse(.angle > 270 | .angle < 90, 0, 1)
    }
  }

  if(position == "top-inside") {
    data$.x <- cos(data$.angle) * data$.r1 * 0.98
    data$.y <- sin(data$.angle) * data$.r1 * 0.98

    if(object$facing == "binding") {
      vjust <- 1
    }
    if(object$facing == "clockwise") {
      hjust <- ifelse(.angle > 270 | .angle < 90, 1, 0)
    }
  }

  if(position == "bottom-outside") {
    data$.x <- cos(data$.angle) * data$.r0 * 0.98
    data$.y <- sin(data$.angle) * data$.r0 * 0.98

    if(object$facing == "binding") {
      vjust <- 1
    }
    if(object$facing == "clockwise") {
      hjust <- ifelse(.angle > 270 | .angle < 90, 1, 0)
    }
  }

  if(position == "bottom-inside") {
    data$.x <- cos(data$.angle) * data$.r0 * 1.02
    data$.y <- sin(data$.angle) * data$.r0 * 1.02

    if(object$facing == "binding") {
      vjust <- 0
    }
    if(object$facing == "clockwise") {
      hjust <- ifelse(.angle >= 270 | .angle < 90, 0, 1)
    }
  }

  data$.angle <- calc_text_angle(data$.angle,
                                 facing = object$facing,
                                 nice_facing = object$nice_facing)
  data$hjust <- hjust
  data$vjust <- vjust

  object$data <- data
  object$mapping <- if(is.null(object$mapping)) {
    aes_(angle = ~.angle, hjust = ~hjust, vjust = ~vjust)
  } else {
    modifyList(object$mapping, aes_(angle = ~.angle, hjust = ~hjust, vjust = ~vjust))
  }

  object <- object[setdiff(names(object), c("facing", "nice_facing", "position", "ID"))]

  object <- do.call(geom_text, object)
  ggplot_add(object, plot, object_name)
}
