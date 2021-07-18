#' Geom function for draw arc heatmap text.
#' @title Geom Arc Heatmap Text
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_text
#' @param hjust one of left, middle or right.
#' @param facing one of clockwise, binding or normal.
#' @param nice_facing logical value.
#' @param position one of middle, top-outside, top-inside, bottom-outside,
#' bottom-inside.
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
#' @rdname geom_hp_text
#' @export
geom_rtext <- function(mapping = NULL,
                       data = NULL,
                       ...,
                       parse = FALSE,
                       nudge_x = 0,
                       nudge_y = 0,
                       check_overlap = FALSE,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE,
                       facing = "clockwise",
                       nice_facing = TRUE,
                       position = "top-outside") {
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
                         ...),
            class = "geom_rtext")
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.geom_rtext <- function(object, plot, object_name) {
  if(!is_piechart(plot)) {
    stop("`geom_rtext()` can only be added on a piechart plot.", call. = FALSE)
  }
  facing <- object$facing
  facing <- match.arg(facing, c("binding", "clockwise"))
  position <- object$position
  position <- match.arg(position, c("top-outside", "bottom-outside"))

  if(is.null(object$data) || is.function(object$data)) {
    data <- plot$data
  } else {
    data <- object$data
  }
  stopifnot(is_hp_data(data))
  .isLabel <- NULL
  data <- dplyr::filter(data, .isLabel)
  if(is.function(object$data)) {
    data <- do.call(object$data, list(data = data))
  }

  vjust <- hjust <- 0.5
  if(position == "top-outside") {
    data <- dplyr::filter(data, data$.r0 == max(data$.r0))
    .angle <- (data$.angle / pi * 180) %% 360
    data$.x <- cos(data$.angle) * data$.r1 * 1.02
    data$.y <- sin(data$.angle) * data$.r1 * 1.02

    if(facing == "binding") {
      vjust <- 1
    }
    if(facing == "clockwise") {
      hjust <- ifelse(.angle > 270 | .angle < 90, 0, 1)
    }
  }

  if(position == "bottom-outside") {
    data <- dplyr::filter(data, data$.r0 == min(data$.r0))
    .angle <- (data$.angle / pi * 180) %% 360
    data$.x <- cos(data$.angle) * data$.r0 * 0.98
    data$.y <- sin(data$.angle) * data$.r0 * 0.98

    if(facing == "binding") {
      vjust <- 0
    }
    if(facing == "clockwise") {
      hjust <- ifelse(.angle > 270 | .angle < 90, 1, 0)
    }
  }

  data$.angle <- calc_text_angle(data$.angle,
                                 facing = facing,
                                 nice_facing = object$nice_facing)
  data$hjust <- hjust
  data$vjust <- vjust

  object$data <- data
  object$mapping <- if(is.null(object$mapping)) {
    aes_(angle = ~.angle, hjust = ~hjust, vjust = ~vjust)
  } else {
    modifyList(object$mapping, aes_(angle = ~.angle, hjust = ~hjust, vjust = ~vjust))
  }

  object <- object[setdiff(names(object), c("facing", "nice_facing", "position"))]

  object <- do.call(geom_text, object)
  ggplot_add(object, plot, object_name)
}

#' @rdname geom_hp_text
#' @export
geom_ctext <- function(mapping = NULL,
                       data = NULL,
                       ...,
                       parse = FALSE,
                       nudge_x = 0,
                       nudge_y = 0,
                       check_overlap = FALSE,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = FALSE,
                       hjust = "left") {
  structure(.Data = list(mapping = mapping,
                         data = data,
                         parse = parse,
                         nudge_x = nudge_x,
                         nudge_y = nudge_y,
                         check_overlap = check_overlap,
                         na.rm = na.rm,
                         show.legend = show.legend,
                         inherit.aes = inherit.aes,
                         hjust = hjust,
                         ...),
            class = "geom_ctext")
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.geom_ctext <- function(object, plot, object_name) {
  if(!is_piechart(plot)) {
    stop("`geom_ctext()` can only be added on a piechart plot.", call. = FALSE)
  }
  hjust <- match.arg(object$hjust, c("left", "middle", "right"))

  if(is.null(object$data)) {
    data <- plot$data
  } else {
    data <- object$data
  }
  stopifnot(is_hp_data(data))
  data <- attr(data, "ctext")

  if(hjust == "left") {
    angle <- radian((data$.start + 1) %% 360)
    x <- cos(angle) * data$.r
    y <- sin(angle) * data$.r
    rot <- data$.start + 1
    hjust <- 1
  } else if(hjust == "right") {
    angle <- radian((data$.end - 1) %% 360)
    x <- cos(angle) * data$.r
    y <- sin(angle) * data$.r
    rot <- data$.start - 1
    hjust <- 0
  } else {
    angle <- radian(((data$.start + data$.end) / 2 + 180) %% 360)
    x <- cos(angle) * data$.r
    y <- sin(angle) * data$.r
    rot <- (data$.start + data$.end) / 2 + 180
    hjust <- 0.5
  }

  data <- tibble::tibble(x = x,
                         y = y,
                         label = data$.label,
                         angle = (rot - 90) %% 360,
                         hjust = hjust)
  data$angle <- ifelse(data$angle > 180, data$angle -180, data$angle)
  object$data <- data

  object$mapping <- if(is.null(object$mapping)) {
    aes_(x = ~x, y = ~y, label = ~label, angle = ~angle, hjust = ~hjust)
  } else {
    modifyList(object$mapping, aes_(x = ~x, y = ~y, label = ~label,
                                    angle = ~angle, hjust = ~hjust))
  }

  object <- object[setdiff(names(object), "hjust")]

  object <- do.call(geom_text, object)
  ggplot_add(object, plot, object_name)
}
