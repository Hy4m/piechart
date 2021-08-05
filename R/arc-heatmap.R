#' Geom function for draw arc heatmap text.
#' @title Geom Arc Heatmap Text
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_text
#' @param hjust one of left, middle or right.
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
                       inherit.aes = FALSE,
                       facing = "clockwise",
                       nice_facing = TRUE,
                       position = "top-outside",
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
            class = "geom_rtext")
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.geom_rtext <- function(object, plot, object_name) {
  if(!is_piechart(plot)) {
    stop("`geom_rtext()` can only be added on a piechart plot.", call. = FALSE)
  }
  facing <- match.arg(object$facing, c("binding", "clockwise"))
  position <- match.arg(object$position, c("top-outside", "bottom-outside"))

  if(!is.null(object$data)) {
    data <- attr(object$data, "META")
  } else {
    if(!is.null(object$ID)) {
      data <- plot$plot_env[[object$ID]]
    } else {
      data <- plot$plot_env[[plot$plot_env$last_plot]]
    }
  }

  stopifnot(inherits(data, "hp_meta"))
  r0 <- data$r0
  r1 <- data$r1
  label <- data$row_names
  n <- length(label)
  start <- seq(data$start, data$end, length.out = n + 1)[-(n + 1)]
  end <- seq(data$start, data$end, length.out = n + 1)[-1]
  angle <- ((start + end) / 2) %% 360
  vjust <- hjust <- 0.5
  if(position == "top-outside") {
    dd <- tibble::tibble(.x = cos(radian(angle)) * r1 * 1.02,
                         .y = sin(radian(angle)) * r1 * 1.02,
                         .label = label)

    if(facing == "binding") {
      vjust <- 1
    }
    if(facing == "clockwise") {
      hjust <- ifelse(angle > 270 | angle < 90, 0, 1)
    }
  }

  if(position == "bottom-outside") {
    dd <- tibble::tibble(.x = cos(radian(angle)) * r0 * 0.98,
                         .y = sin(radian(angle)) * r0 * 0.98,
                         .label = label)

    if(facing == "binding") {
      vjust <- 0
    }
    if(facing == "clockwise") {
      hjust <- ifelse(angle > 270 | angle < 90, 1, 0)
    }
  }

  dd$.angle <- calc_text_angle(radian(angle),
                               facing = facing,
                               nice_facing = object$nice_facing)
  dd$hjust <- hjust
  dd$vjust <- vjust

  object$data <- dd
  object$mapping <- if(is.null(object$mapping)) {
    aes_(x = ~.x, y = ~.y, label = ~.label, angle = ~.angle,
         hjust = ~hjust, vjust = ~vjust)
  } else {
    modifyList(object$mapping, aes_(x = ~.x, y = ~.y, angle = ~.angle,
                                    label = ~.label, hjust = ~hjust,
                                    vjust = ~vjust))
  }

  object <- object[setdiff(names(object), c("facing", "nice_facing", "position", "ID"))]

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
                       hjust = "left",
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
                         hjust = hjust,
                         ID = ID,
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

  if(!is.null(object$data)) {
    data <- attr(object$data, "META")
  } else {
    if(!is.null(object$ID)) {
      data <- plot$plot_env[[object$ID]]
    } else {
      data <- plot$plot_env[[plot$plot_env$last_plot]]
    }
  }

  stopifnot(inherits(data, "hp_meta"))
  start <- data$start
  end <- data$end
  label <- data$col_names
  n <- length(label)
  r0 <- seq(data$r0, data$r1, length.out = n + 1)[-(n + 1)]
  r1 <- seq(data$r0, data$r1, length.out = n + 1)[-1]
  r <- (r0 + r1) / 2

  if(hjust == "left") {
    angle <- radian((start + 1) %% 360)
    dd <- tibble::tibble(.x = cos(angle) * r,
                         .y = sin(angle) * r,
                         .label = label,
                         .angle = ((start + 1) %% 360) - 90,
                         .hjust = 1)
    dd$.hjust <- ifelse(dd$.angle > 180, 0, 1)
    dd$.angle <- ifelse(dd$.angle > 180, dd$.angle -180, dd$.angle)
  } else if(hjust == "right") {
    angle <- radian((data$end - 1) %% 360)
    dd <- tibble::tibble(.x = cos(angle) * r,
                         .y = sin(angle) * r,
                         .label = label,
                         .angle = ((end - 1) %% 360) - 90,
                         .hjust = 0)
    dd$.hjust <- ifelse(dd$.angle > 180, 1, 0)
    dd$.angle <- ifelse(dd$.angle > 180, dd$.angle -180, dd$.angle)
  } else {
    angle <- radian(((data$start + data$end) / 2 + 180) %% 360)
    dd <- tibble::tibble(.x = cos(angle) * r,
                         .y = sin(angle) * r,
                         .label = label,
                         .angle = (((start + end) / 2 + 180) %% 360) - 90,
                         .hjust = 0.5)
  }

  dd$.angle <- ifelse(dd$.angle > 180, dd$.angle -180, dd$.angle)

  object$data <- dd

  object$mapping <- if(is.null(object$mapping)) {
    aes_(x = ~.x, y = ~.y, label = ~.label, angle = ~.angle, hjust = ~.hjust)
  } else {
    modifyList(object$mapping, aes_(x = ~.x, y = ~.y, label = ~.label,
                                    angle = ~.angle, hjust = ~.hjust))
  }

  object <- object[setdiff(names(object), c("hjust", "ID"))]

  object <- do.call(geom_text, object)
  ggplot_add(object, plot, object_name)
}
