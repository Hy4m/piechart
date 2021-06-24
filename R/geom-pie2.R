#' @title Pie2 Geom
#' Function to draw scatter piechart.
#' @param sort_by a character vector of variable name.
#' @param decreasing logical. Should the sort order be increasing or decreasing?
#' @param start offset of starting point from 3 o'clock in radians.
#' @param steps increment of the sequence in radians.
#' @param sum_value sum of the value.
#' @param label.facing one of downward, binding or clockwise.
#' @param label.size the size of label.
#' @param label.col the color of label.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @section Aesthetics:
#' \code{geom_pie2()} understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \strong{\code{value}}
#'       \item \code{r0}
#'       \item \code{r1}
#'       \item \code{radius}
#'       \item \code{label}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{fill}
#'       \item \code{linetype}
#'       \item \code{size}
#'       \item \code{group}
#'    }
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 remove_missing
#' @importFrom ggplot2 draw_key_polygon
#' @importFrom grid grobTree
#' @importFrom grid nullGrob
#' @importFrom scales alpha
#' @rdname geom_pie2
#' @export
geom_pie2 <- function(mapping = NULL,
                      data = NULL,
                      stat = "identity",
                      position = "identity",
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      sort_by = NULL,
                      decreasing = TRUE,
                      start = 90,
                      steps = 0.001,
                      sum_value = NULL,
                      label.facing = "downward",
                      label.size = 3.88,
                      label.col = "black",
                      ...) {
  layer(
    geom = GeomPie2,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(sort_by = sort_by,
                  decreasing = decreasing,
                  start = start,
                  steps = steps,
                  sum_value = sum_value,
                  label.facing = label.facing,
                  label.size = label.size,
                  label.col = label.col,
                  na.rm = na.rm,
                  ...
                  )
  )
}

#' @rdname geom_pie2
#' @format NULL
#' @usage NULL
#' @export
GeomPie2 <- ggplot2::ggproto(
  "GeomPie2",
  ggplot2::Geom,
  required_aes = c("x", "y", "value"),

  default_aes = aes(colour = "grey35", fill = NA, radius = 5, r0 = 0.5, r1 = 1,
                    size = 0.5, linetype = 1, alpha = 1, label = NA, group = 1L,
                    sep = 0),

  handle_na = function (self, data, params) {
    ggplot2::remove_missing(data,
                            params$na.rm,
                            c(self$required_aes, self$non_missing_aes),
                            "geom_pie2")
    },

  draw_panel = function(data,
                        panel_params,
                        coord,
                        sort_by = NULL,
                        decreasing = TRUE,
                        start = 90,
                        steps = 0.001,
                        sum_value = NULL,
                        label.facing = "downward",
                        label.size = 3.88,
                        label.col = "black") {
    if(!is.null(sort_by)) {
      params <- c(data[sort_by], list(decreasing = decreasing))
      ids <- do.call(order, params)
      data <- data[ids, , drop = FALSE]
    }

    if(!is.null(sum_value)) {
      sum_value <- rep_len(sum_value, length(unique(data$group)))
    }
    data <- coord$transform(data, panel_params)
    data_list <- split(data, data$group)
    grobs <- lapply(seq_along(data_list),
                    function(.id) {
                      .data <- data_list[[.id]]
                      if(!is.null(sum_value)) {
                        sum_value <- sum_value[.id]
                      }
                      n <- nrow(.data)
                      if (n < 1) return(grid::nullGrob())
                      first_row <- .data[1, , drop = FALSE]
                      pieGrob(
                        value = .data$value,
                        label = .data$label,
                        x = first_row$x,
                        y = first_row$y,
                        r0 = .data$r0,
                        r1 = .data$r1,
                        start = start,
                        sep = .data$sep,
                        steps = steps,
                        sum_value = sum_value,
                        radius = first_row$radius,
                        label.facing = label.facing,
                        label.size = label.size * ggplot2::.pt,
                        label.col = label.col,
                        default.units = "native",
                        gp = grid::gpar(
                          col = .data$colour,
                          fill = scales::alpha(.data$fill, .data$alpha),
                          lwd = .data$size * ggplot2::.pt,
                          lty = .data$linetype
                        )
                      )
                    })
    ggplot2:::ggname("geom_pie2", do.call("grobTree", grobs))
  },

  draw_key = ggplot2::draw_key_polygon
)

