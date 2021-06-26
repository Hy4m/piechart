#' @title Quadratic Bezier Curve
#' Function to draw quadratic Bezier curve.
#' @param height a numeirc value in range o to 1.
#' @inheritParams ggplot2::layer
#' @inheritParams ggforce::geom_bezier
#' @section Aesthetics:
#' \code{geom_quadline()} understands the following aesthetics (required
#' aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \strong{\code{xend}}
#'       \item \strong{\code{yend}}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{linetype}
#'       \item \code{size}
#'       \item \code{group}
#'    }
#' @importFrom ggplot2 draw_key_path
#' @importFrom ggplot2 GeomPath
#' @importFrom ggforce geom_bezier
#' @importFrom ggforce StatBezier
#' @rdname geom_quadline
#' @export
geom_quadline <- function(mapping = NULL,
                          data = NULL,
                          stat = "quadline",
                          position = "identity",
                          arrow = NULL,
                          lineend = "butt",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          height = 0.2,
                          n = 100,
                          ...) {
  suppressWarnings(
    layer(
      geom = GeomPath,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(arrow = arrow,
                    lineend = lineend,
                    height = height,
                    n = n,
                    na.rm = na.rm,
                    ...
      )
    )
  )
}


#' @rdname geom_quadline
#' @export
stat_quadline <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          height = 0.2,
                          n = 100,
                          ...) {
  layer(
    stat = StatQuadline,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(height = height,
                  n = n,
                  na.rm = na.rm,
                  ...
    )
  )
}
#' @rdname geom_quadline
#' @format NULL
#' @usage NULL
#' @export
StatQuadline <- ggproto("StatQuadline", StatBezier,
                        require_aes = c("x", "y", "xend", "yend"),
                        setup_data = function(data, params) {
                          trans_quadline(data, params$height)
                        },
                        setup_params = function(data, params) {
                          if(is.null(params$height)) {
                            params$height <- 0.8
                          }
                          params
                        },
                        extra_params = c("height", "n", "na.rm")
                        )
#' @noRd
trans_quadline <- function(data, height) {
  n <- nrow(data)
  x <- (data$x + data$xend) / 2
  y <- (data$y + data$yend) / 2

  xx <- c(data$x, x * (1 - height), data$xend)
  yy <- c(data$y, y * (1 - height), data$yend)
  group <- rep(seq_len(n), 3)
  data <- data[, setdiff(names(data), c("x", "y", "xend", "yend", "group")), drop = FALSE]
  dplyr::bind_cols(tibble::tibble(x = xx,
                                  y = yy,
                                  group = group),
                   data[group, , drop = FALSE])
}

