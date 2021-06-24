#' Default theme for piechart
#' @title Default Theme
#' @param ... extra parameters passed on to \code{theme()}.
#' @return a complete theme.
#' @rdname theme_piechart
#' @importFrom ggplot2 theme_gray
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 %+replace%
#' @importFrom ggplot2 element_blank
#' @export
theme_piechart <- function(...) {
  theme_gray() %+replace%
    theme(axis.title = element_blank(),
          axis.line  = element_blank(),
          axis.text  = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          ...)
}
