#' Geom function for draw piechart.
#' @title Pie Geom
#' @param ID character.
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
                     ID = NULL,
                     ...) {
  structure(.Data = list(mapping = mapping,
                         data = data,
                         stat = stat,
                         position = position,
                         na.rm = na.rm,
                         show.legend = show.legend,
                         inherit.aes = inherit.aes,
                         ID = ID,
                         ...),
            class = "geom_pie")
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.geom_pie <- function(object, plot, object_name) {
  if(!is_piechart(plot)) {
    stop("`geom_pie()` can only be added on a piechart plot.", call. = FALSE)
  }
  if(is.null(object$ID)) {
    object$ID <- paste0("ID", digest::digest(stats::rnorm(1, sd = 100)))
  }
  if(is.null(object$data)) {
    data <- plot$data
  } else if(is.function(object$data)) {
    data <- do.call(object$data, list(data = plot$data))
  } else if (is_piechart_data(object$data)){
    data <- object$data
  } else {
    mapping <- object$mapping[c("value", "r0", "r1", "sep", "label")]
    nm <- names(mapping)
    if(!"r0" %in% nm) {
      mapping$r0 <- object$r0 %||% 0.5
    }
    if(!"r1" %in% nm) {
      mapping$r1 <- object$r1 %||% 1
    }
    if(!"sep" %in% nm) {
      mapping$sep <- object$sep %||% 0
    }
    if(!"label" %in% nm) {
      mapping$label <- object$label %||% NA
    }

    params <- list(data = object$data,
                   mapping = mapping,
                   sort_by = object$sort_by,
                   decreasing = object$decreasing %||% TRUE,
                   start = object$start %||% 0,
                   end = object$end %||% 360,
                   steps = object$steps %||% 0.01)
    data <- do.call(piechart_data, params)

    base_mapping <- aes_(x = ~.x,
                         y = ~.y,
                         group = ~.group)
    object$mapping <- modifyList(
      object$mapping[setdiff(names(object$mapping),
                             c("value", "r0", "r1", "sep", "label"))],
      base_mapping
    )
    object <- object[setdiff(names(object), c("value", "r0", "r1", "sep", "label",
                                              "sort_by", "decreasing", "start",
                                              "end", "steps"))]
  }

  if(!is_hp_data(data)) {
    .isLabel <- NULL
    object$data <- dplyr::filter(data, !.isLabel)
    plot$plot_env[[object$ID]] <- dplyr::filter(data, .isLabel)
  } else {
    plot$plot_env[[object$ID]] <- attr(data, "META")
  }
  plot$plot_env$last_plot <- object$ID

  object <- object[setdiff(names(object), "ID")]
  object <- do.call(geom_polygon, object)
  ggplot_add(object, plot, object_name)
}
