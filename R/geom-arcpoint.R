#' Geom function for draw points on a arc canvas.
#' @title Arcpoint Geom
#' @param mapping Set of aesthetic mappings.
#' @param data a data frame.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than
#' combining with them.
#' @param jitter one of "x", "y" or "xy".
#' @param r0,r1 a positive numeric value.
#' @param start,end a numeric value between 0 to 360.
#' @param ID character.
#' @param ... other parameters.
#' @section Aesthetics:
#' \code{geom_arcpoint()} understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \code{shape}
#'       \item \code{alpha}
#'       \item \code{fill}
#'       \item \code{colour}
#'       \item \code{size}
#'       \item \code{stroke}
#'    }
#' @importFrom ggplot2 geom_point
#' @rdname geom_arcpoint
#' @export
geom_arcpoint <- function(mapping = NULL,
                          data = NULL,
                          inherit.aes = TRUE,
                          jitter = NULL,
                          r0 = 0.5,
                          r1 = 1,
                          start = 60,
                          end = 300,
                          ID = NULL,
                          ...) {
  structure(list(mapping = mapping,
                 data = data,
                 inherit.aes = inherit.aes,
                 jitter = jitter,
                 r0 = r0,
                 r1 = r1,
                 start = start,
                 end = end,
                 ID = ID,
                 ...), class = "geom_arcpoint")
}

#' @export
ggplot_add.geom_arcpoint <- function(object, plot, object_name) {
  if(is.null(object$ID)) {
    ID <- paste0("ID", digest::digest(rnorm(1, sd = 100)))
  } else {
    ID <- object$ID
  }
  data <- object$data %||% plot$data
  if(isTRUE(object$inherit.aes)) {
    mapping <- aes_modify(plot$mapping, object$mapping)
  } else {
    mapping <- object$mapping
  }
  nm <- names(mapping)
  if(!all(c("x", "y") %in% nm)) {
    stop("geom_arcpoint() needs 'x' and 'y' mapping.", call. = FALSE)
  }

  xvar <- rlang::as_name(mapping$x)
  yvar <- rlang::as_name(mapping$y)
  x <- data[[xvar]]
  y <- data[[yvar]]
  is_na <- is.na(x) & is.na(y)
  data <- data[!is_na, , drop = FALSE]
  if(nrow(data) < 1) {
    return(plot)
  }

  x <- x[!is_na]
  y <- y[!is_na]

  jitter <- object$jitter

  if(is.numeric(x)) {
    if(!is.null(jitter) && jitter %in% c("x", "xy")) {
      x <- jitter(x)
    }
    xrng <- expand_c(range(x))
    xlimits <- pretty(xrng)
  } else {
    if(!is.factor(x)) {
      x <- as.factor(x)
    }
    xlimits <- levels(x)
    if(!is.null(jitter) && jitter %in% c("x", "xy")) {
      x <- jitter(as.numeric(x))
    } else {
      x <- as.numeric(x)
    }
    xrng <- c(min(0.4, x), max(length(xlimits) + 0.6, max(x)))
  }

  if(is.numeric(y)) {
    if(!is.null(jitter) && jitter %in% c("y", "xy")) {
      y <- jitter(y)
    }
    yrng <- expand_c(range(y))
    ylimits <- pretty2(yrng)
  } else {
    if(!is.factor(y)) {
      y <- as.factor(y)
    }
    ylimits <- levels(y)
    if(!is.null(jitter) && jitter %in% c("y", "xy")) {
      y <- jitter(as.numeric(y))
    } else {
      y <- as.numeric(y)
    }
    yrng <- range(y)
  }

  start <- object$start %% 360
  end <- object$end %% 360
  if(end >= start) {
    end <- start - start - (360 - end)
  }

  rot <- scales::rescale(x, c(start, end), xrng)
  rr <- scales::rescale(y, c(object$r0, object$r1), yrng)

  data[[xvar]] <- cos(radian(rot)) * rr
  data[[yvar]] <- sin(radian(rot)) * rr

  META <- structure(list(
    r0 = object$r0,
    r1 = object$r1,
    start = start,
    end = end,
    xrange = xrng,
    yrange = yrng,
    xlimits = xlimits,
    ylimits = ylimits
  ), class = "point_meta")

  mapping <- mapping[c("x", "y", "fill", "colour", "shape",
                       "size", "alpha", "stroke")]

  object$data <- data
  object$mapping <- mapping
  object <- object[setdiff(names(object), c("jitter", "r0", "r1", "ID",
                                            "start", "end"))]
  object <- do.call(geom_point, object)
  plot$plot_env[[ID]] <- META

  ggplot_add(object, plot, object_name)
}
