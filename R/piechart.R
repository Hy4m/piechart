#' Init piechart plot
#' @title Piechat Plot
#' @param data a data frame.
#' @param mapping default list of aesthetic mappings to use for plot.
#' @param xlim,ylim Limits for the x and y axes.
#' @param clip Should drawing be clipped to the extent of the plot panel?
#' @param ... extra parameters passed on to \code{piechart_data()}.
#' @return a ggplot object.
#' @rdname piechart
#' @importFrom ggplot2 aes_
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 theme_void
#' @export
piechart <- function(data = NULL,
                     mapping = aes(),
                     xlim = NULL,
                     ylim = NULL,
                     clip = "on",
                     ...
                     ){
  if(is.null(data)) {
    p <- empty_piechart(xlim = xlim, ylim = ylim, clip = clip)
    return(p)
  }

  if(!is_piechart_data(data)) {
    data <- piechart_data(data, mapping, ...)
  }

  base_mapping <- aes_(x = ~.x,
                       y = ~.y,
                       group = ~.group,
                       label = ~.label)
  mapping <- mapping[setdiff(names(mapping), c("value", "zoom", "group"))]
  mapping <- modifyList(mapping, base_mapping)

  p <- ggplot(data = data, mapping = mapping) +
    coord_fixed(xlim = xlim, ylim = ylim, clip = clip) +
    theme_piechart()
  structure(p, class = c("piechart", class(p)))
}


#' @noRd
empty_piechart <- function(xlim = NULL,
                           ylim = NULL,
                           clip = "on") {
  data <- empty_piechart_data()
  mapping <- aes_(x = ~.x,
                  y = ~.y,
                  group = ~.group,
                  label = ~.label)
  p <- ggplot(data = data, mapping = mapping) +
    coord_fixed(xlim = xlim, ylim = ylim, clip = clip) +
    theme_piechart()
  structure(p, class = c("piechart", class(p)))
}

#' @noRd
is_piechart <- function(x) inherits(x, "piechart")

