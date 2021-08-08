#' Geom function for draw quadline.
#' @title Quadline Geom
#' @param mapping Set of aesthetic mappings.
#' @param data a data frame.
#' @param r a numeric value between 0 to 1.
#' @param from,to specifies the start and end ID of the connection.
#' @param ... other parameters.
#' @section Aesthetics:
#' \code{geom_arclink()} understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{from}}
#'       \item \strong{\code{to}}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{size}
#'       \item \code{linetype}
#'    }
#' @rdname geom_arclink
#' @export
geom_arclink <- function(mapping = NULL,
                         data = NULL,
                         r = 0.98,
                         from_id = NULL,
                         to_id = NULL,
                         ...) {
  structure(list(data = data,
                 mapping = mapping,
                 r = r,
                 from_id = from_id,
                 to_id = to_id,
                 ...), class = "geom_arclink")
}

#' @export
ggplot_add.geom_arclink <- function(object, plot, object_name) {
  stopifnot(is_piechart(plot))
  data <- object$data
  mapping <- object$mapping
  from_id <- object$from_id
  to_id <- object$to_id

  if(!is.null(mapping) && !all(c("from", "to") %in% names(mapping))) {
    stop("mapping should contain 'from' and 'to' at least.", call. = FALSE)
  }

  if(is.null(data)) {
    data <- plot$data
  }

  if(is.null(mapping)) {
    from_var <- names(data)[1]
    to_var <- names(data)[2]
  } else {
    from_var <- rlang::as_name(mapping$from)
    to_var <- rlang::as_name(mapping$to)
  }

  if(is.null(from_id) && is.null(to_id)) {
    from_id <- to_id <- plot$plot_env$last_plot
  }
  if(is.null(from_id)) {
    from_id <- to_id
  }
  if(is.null(to_id)) {
    to_id <- from_id
  }

  from_id <- plot$plot_env[[from_id]]
  to_id <- plot$plot_env[[to_id]]
  if(is_piechart_data(from_id)) {
    angle <- from_id$.angle
    label <- from_id$.label
    r <- from_id$.r0 * (object$r %||% 0.98)
    xx <- rlang::set_names(cos(angle) * r, label)
    yy <- rlang::set_names(sin(angle) * r, label)
  } else {
    label <- from_id$row_names
    n <- length(label)
    start <- seq(from_id$start, from_id$end, length.out = n + 1)[-(n + 1)]
    end <- seq(from_id$start, from_id$end, length.out = n + 1)[-1]
    angle <- radian(((start + end) / 2) %% 360)
    r <- from_id$r0 * (object$r %||% 0.98)
    xx <- rlang::set_names(cos(angle) * r, label)
    yy <- rlang::set_names(sin(angle) * r, label)
  }

  if(is_piechart_data(to_id)) {
    angle <- to_id$.angle
    label <- to_id$.label
    r <- to_id$.r0 * (object$r %||% 0.98)
    xx2 <- rlang::set_names(cos(angle) * r, label)
    yy2 <- rlang::set_names(sin(angle) * r, label)
  } else {
    label <- to_id$row_names
    n <- length(label)
    start <- seq(to_id$start, to_id$end, length.out = n + 1)[-(n + 1)]
    end <- seq(to_id$start, to_id$end, length.out = n + 1)[-1]
    angle <- radian(((start + end) / 2) %% 360)
    r <- to_id$r0 * (object$r %||% 0.98)
    xx2 <- rlang::set_names(cos(angle) * r, label)
    yy2 <- rlang::set_names(sin(angle) * r, label)
  }

  data$.x <- xx[data[[from_var]]]
  data$.y <- yy[data[[from_var]]]
  data$.xend <- xx2[data[[to_var]]]
  data$.yend <- yy2[data[[to_var]]]
  mapping <- aes_modify(mapping[setdiff(names(mapping), c("from", "to"))],
                        aes_(x = ~.x, y = ~.y, xend = ~.xend, yend = ~.yend))
  object <- object[setdiff(names(object), c("from", "to", "r"))]
  object$data <- data
  object$mapping <- mapping
  object$inherit.aes <- FALSE
  object <- do.call(geom_quadline, object)
  ggplot_add(object, plot, object_name)
}
