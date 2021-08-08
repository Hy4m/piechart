#' Geom function for draw arcbar.
#' @title Arcbar Geom
#' @param mapping Set of aesthetic mappings.
#' @param data a data frame.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than
#' combining with them.
#' @param width a numeric value between 0 to 1.
#' @param r0,r1 a positive numeric value.
#' @param start,end a numeric value between 0 to 360.
#' @param ID character.
#' @param ... other parameters.
#' @section Aesthetics:
#' \code{geom_arcbar()} understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{label}}
#'       \item \strong{\code{value}}
#'       \item \code{alpha}
#'       \item \code{fill}
#'       \item \code{colour}
#'       \item \code{size}
#'       \item \code{linetype}
#'    }
#' @importFrom stats rnorm
#' @rdname geom_arcbar
#' @export
geom_arcbar <- function(mapping = NULL,
                        data = NULL,
                        inherit.aes = TRUE,
                        width = 0.9,
                        r0 = 0.5,
                        r1 = 1,
                        start = 60,
                        end = 300,
                        ID = NULL,
                        ...) {
  structure(list(mapping = mapping,
                 data = data,
                 inherit.aes = inherit.aes,
                 width = width,
                 r0 = r0,
                 r1 = r1,
                 start = start,
                 end = end,
                 ID = ID,
                 ...), class = "geom_arcbar")
}

#' @export
ggplot_add.geom_arcbar <- function(object, plot, object_name) {
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
  if(!all(c("value", "label") %in% nm)) {
    stop("geom_arcbar() needs 'value' and 'label' mapping.", call. = FALSE)
  }
  value <- rlang::eval_tidy(mapping$value, data)
  label <- rlang::eval_tidy(mapping$label, data)
  is_na <- is.na(value) & is.na(label)
  data <- data[!is_na, , drop = FALSE]
  if(nrow(data) < 1) {
    return(plot)
  }

  value <- value[!is_na]
  label <- label[!is_na]

  if(!is.numeric(value)) {
    stop("value should be numeirc variable.", call. = FALSE)
  }

  if(is.numeric(label)) {
    label_var <- rlang::as_name(mapping$label)
    data[[label_var]] <- as.character(data[[label_var]])
    label <- as.character(label)
  }

  rng <- range(value)
  mapping$value <- 1

  rng <- fine_range(rng)
  start <- object$start %% 360
  end <- object$end %% 360
  if(end >= start) {
    end <- start - start - (360 - end)
  }
  width <- object$width %% 1
  META <- structure(list(
    r0 = object$r0,
    r1 = object$r1,
    start = start,
    end = end,
    xrange = unique(label),
    yrange = rng
  ), class = "bar_meta")

  value <- scales::rescale(value, c(object$r0, object$r1), rng)
  if(all(rng >= 0)) {
    r0 <- 0
    r1 <- value
  } else if (all(rng <= 0)) {
    r0 <- value
    r1 <- 0
  } else {
    rr <- scales::rescale(0, c(object$r0, object$r1), rng)
    r0 <- ifelse(value >= 0, rr, value)
    r1 <- ifelse(value >= 0, value, rr)
  }
  mapping$r0 <- r0
  mapping$r1 <- r1

  n <- length(unique(label))
  ur <- (start - end) / n
  start <- start - ur / 2
  mapping$sep <- ur * (1 - width)
  data <- piechart_data(data = data,
                        mapping = mapping,
                        start = start,
                        end = end)

  .isLabel <- NULL
  META$label_data <- filter(data, .isLabel)
  data <- filter(data, !.isLabel)
  base_mapping <- aes_(x = ~.x,
                       y = ~.y,
                       group = ~.group)
  mapping <- aes_modify(base_mapping, mapping[c("fill", "colour", "linetype",
                                                "size", "alpha")])

  object$data <- data
  object$mapping <- mapping
  object$inherit.aes = FALSE
  object <- object[setdiff(names(object), c("width", "r0", "r1", "ID",
                                            "start", "end", "sep"))]
  object <- do.call(geom_pie, object)
  plot$plot_env[[ID]] <- META
  ggplot_add(object, plot, object_name)
}

fine_range <- function(x) {
  if(all(x >= 0)) {
    fine_rng <- range(pretty(x))
    return(c(0, max(fine_rng, x)))
  }
  if(all(x <= 0)) {
    fine_rng <- range(pretty(x))
    return(c(min(fine_rng, x), 0))
  }
  range(pretty(x))
}
