#' Draw Piechart
#' @description These functions use to create and draw piechart.
#' @param value non-negtive numeric vector.
#' @param label label for each sector.
#' @param x,y a scala numeric value.
#' @param r0,r1 the radius of piechart, should be between 0 and 1.
#' @param start offset of starting point from 3 o'clock in degrees.
#' @param sep gap bewteen two adjacent sectors.
#' @param steps increment of the sequence in radians.
#' @param sum_value sum of the value.
#' @param radius the size (in mm) of pie.
#' @param label.facing one of downward, binding or clockwise.
#' @param label.size the size of label.
#' @param label.col the color of label.
#' @param default.units a string indicating the default units.
#' @param gp an object of class gpar.
#' @param ... extra parameters.
#' @return a polygonGrob.
#' @rdname pieGrob
#' @importFrom grid gpar
#' @importFrom grid get.gpar
#' @importFrom grid unit
#' @importFrom grid viewport
#' @importFrom grid polygonGrob
#' @importFrom grid textGrob
#' @importFrom grid nullGrob
#' @importFrom grid grobTree
#' @importFrom grid grid.draw
#' @importFrom utils modifyList
#' @importFrom stats runif
#' @export

pieGrob <- function(value = runif(10),
                    label = NULL,
                    x = 0.5,
                    y = 0.5,
                    r0 = 0.5,
                    r1 = 1,
                    start = 90,
                    sep = 0,
                    steps = 0.001,
                    sum_value = NULL,
                    radius = 10,
                    label.facing = "downward",
                    label.size = 12,
                    label.col = "black",
                    default.units = "native",
                    gp = gpar()) {
  label.facing <- match.arg(label.facing, c("downward", "clockwise", "binding"))
  dd <- point_to_ring(value = value,
                      label = label,
                      r0    = r0,
                      r1    = r1,
                      start = start,
                      sep  = sep,
                      steps = steps,
                      sum_value = sum_value,
                      cx    = 0,
                      cy    = 0)
  sectors <- dd[!dd$.isLabel, , drop = FALSE]
  labels <- dd[dd$.isLabel, , drop = FALSE]

  rr <- max(abs(dd$.x), abs(dd$.y), na.rm = TRUE)
  lwd <- gp$lwd %||% get.gpar("lwd")$lwd %||% 0.5

  gp <- utils::modifyList(gp, gpar(lwd = lwd))
  radius <- (radius + 0.5 * max(lwd / 2.845276, na.rm = TRUE)) * 2
  vp <- viewport(x = x,
                 y = y,
                 width = unit(radius, "mm"),
                 height = unit(radius, "mm"),
                 xscale = c(-rr, rr) * 1.03,
                 yscale = c(-rr, rr) * 1.03,
                 clip = "off",
                 gp = gp)
  poly <- polygonGrob(sectors$.x, sectors$.y, id = sectors$.group,
                      default.units = default.units)

  all_na <- all(is.na(labels$.label))
  if(all_na) {
    text <- grid::nullGrob()
  } else {
    rot <- calc_text_angle(labels$.angle, facing = label.facing)

    vjust <- hjust <- 0.5
    if(label.facing %in% c("binding", "clockwise")) {
      labels$.x <- cos(labels$.angle) * labels$.r1 * 1.02
      labels$.y <- sin(labels$.angle) * labels$.r1 * 1.02
      if(label.facing == "binding") {
        vjust <- 1
      }
      if(label.facing == "clockwise") {
        angle <- degree(labels$.angle) %% 360
        hjust <- ifelse(angle > 270 | angle < 90, 0, 1)
      }
    }

    text <- textGrob(label = labels$.label,
                     x = labels$.x,
                     y = labels$.y,
                     hjust = hjust,
                     vjust = vjust,
                     rot = rot,
                     default.units = default.units,
                     gp = gpar(fontsize = label.size,
                               col = label.col))
  }
  grobTree(poly, text, gp = gp, vp = vp, cl = "pieGrob")
}

#' @rdname pieGrob
#' @export
grid.pie <- function(...) {
  grid.draw(pieGrob(...))
}

