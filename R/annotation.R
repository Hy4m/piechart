#' @title Annotation for Piechart
#' Functions to draw axis, rect and so on.
#' @param range origin range of data.
#' @param breaks the points at which tick-marks are to be drawn.
#' @param labels a character or expression vector of labels to be
#' placed at the tickpoints.
#' @param r,r0,r1 positive value.
#' @param angle,start,end the angle in radians.
#' @param dtick,dtext direction of ticks and labels.
#' @param ltick length of ticks.
#' @param lcol,lsize axis's colour and line width.
#' @param tcol,tsize label's colour and size.
#' @param facing one of clockwise, binding or normal.
#' @param nice_facing logical value.
#' @param gap gap between ticks and labels.
#' @param ... extra parameters.
#' @return a gg object.
#' @rdname annotation
#' @importFrom ggplot2 geom_path
#' @export
anno_x_axis <- function(range,
                        breaks = NULL,
                        labels = NULL,
                        r = 0.5,
                        start = 120,
                        end = 0,
                        dtick = "inside",
                        ltick = 0.02,
                        lcol = "black",
                        lsize = 0.5,
                        dtext = "inside",
                        tcol = "black",
                        tsize = 3.5,
                        facing = "binding",
                        nice_facing = TRUE,
                        gap = 0.02,
                        ...) {
  dtick <- match.arg(dtick, c("inside", "outside"))
  dtext <- match.arg(dtext, c("inside", "outside"))
  facing <- match.arg(facing, c("binding", "clockwise", "downward"))

  if(is.null(breaks)) {
    breaks <- pretty(range)
    breaks <- breaks[breaks >= range[1] & breaks <= range[2]]
  }
  if(is.null(labels)) {
    labels <- breaks
  }

  start <- start %% 360
  end <- end %% 360
  if(end >= start) {
    end <- start - start - (360 - end)
  }
  start <- radian(start)
  end <- radian(end)

  b_angle <- scales::rescale(breaks, to = c(start, end), from = range)
  tt <- seq(start, end, length.out = 500)

  if(dtick == "outside") {
    ltick <- -ltick
  }

  if(dtext == "outside") {
    gap <- -gap
  }

  tx <- cos(b_angle) * (r - ltick - gap)
  ty <- sin(b_angle) * (r - ltick - gap)
  angle <- calc_text_angle(b_angle, facing, nice_facing)

  hjust <- 0.5
  if(facing == "clockwise") {
    ba <- degree(b_angle) %% 360
    hjust <- if(dtext == "inside") {
      ifelse(ba <= 90 | ba >= 270, 1, 0)
    } else {
      ifelse(ba <= 90 | ba >= 270, 0, 1)
    }
  }
  text_data <- tibble(x = tx,
                      y = ty,
                      label = labels,
                      angle = angle,
                      hjust = hjust)
  lx <- c(cos(tt) * r, cos(rep(b_angle, each = 2)) * c(r, r - ltick))
  ly <- c(sin(tt) * r, sin(rep(b_angle, each = 2)) * c(r, r - ltick))
  ids <- c(rep("axis", 500), rep(paste0("ticks", seq_along(breaks)), each = 2))
  line_data <- tibble(x = lx,
                      y = ly,
                      ids = ids)

  text <- geom_text(mapping = aes_(x = ~x, y = ~y, label = ~label,
                                   angle = ~angle, hjust = ~hjust),
                    data = text_data,
                    colour = tcol,
                    size = tsize,
                    inherit.aes = FALSE)
  line <- geom_path(mapping = aes_(x = ~x, y = ~y, group = ~ids),
                    data = line_data,
                    colour = lcol,
                    size = lsize,
                    inherit.aes = FALSE)
  list(line, text)
}

#' @rdname annotation
#' @export
anno_y_axis <- function(range,
                        breaks = NULL,
                        labels = NULL,
                        r0 = 0.5,
                        r1 = 1,
                        angle = 90,
                        dtick = "left",
                        ltick = 1,
                        lcol = "black",
                        lsize = 0.5,
                        dtext = "left",
                        tcol = "black",
                        tsize = 3.5,
                        gap = 1,
                        ...){

  dtick <- match.arg(dtick, c("left", "right"))
  dtext <- match.arg(dtext, c("left", "right"))

  if(is.null(breaks)) {
    breaks <- pretty(range)
    breaks <- breaks[breaks >= range[1] & breaks <= range[2]]
  }
  if(is.null(labels)) {
    labels <- breaks
  }

  if(dtick == "right") {
    ltick <- -ltick
  }

  if(dtext == "right") {
    gap <- -gap
  }

  rr <- scales::rescale(breaks, to = c(r0, r1), from = range)
  tx <- cos(radian(angle + ltick + gap)) * rr
  ty <- sin(radian(angle + ltick + gap)) * rr

  hjust <- switch (dtext, left = 1, right = 0)
  text_data <- tibble(x = tx,
                      y = ty,
                      label = labels,
                      angle = (angle - 90) %% 360,
                      hjust = hjust)

  angle <- radian(angle)
  n <- ceiling(abs(1000 * radian(ltick)))
  m <- length(breaks)
  tt <- seq(angle, angle + radian(ltick), length.out = n)
  lx <- c(cos(angle) * r0, cos(angle) * r1, cos(rep(tt, m)) * rep(rr, each = n))
  ly <- c(sin(angle) * r0, sin(angle) * r1, sin(rep(tt, m)) * rep(rr, each = n))
  ids <- c(rep_len("axis", 2), rep(paste0("ticks", seq_along(breaks)), each = n))
  line_data <- tibble(x = lx,
                      y = ly,
                      ids = ids)

  text <- geom_text(mapping = aes_(x = ~x, y = ~y, label = ~label,
                                   angle = ~angle, hjust = ~hjust),
                    data = text_data,
                    colour = tcol,
                    size = tsize,
                    inherit.aes = FALSE)
  line <- geom_path(mapping = aes_(x = ~x, y = ~y, group = ~ids),
                    data = line_data,
                    colour = lcol,
                    size = lsize,
                    inherit.aes = FALSE)
  list(line, text)
}

#' @rdname annotation
#' @export
anno_rect <- function(r0 = 0.5,
                      r1 = 1,
                      start = 90,
                      end = 270,
                      ...){
  nn <- max(length(r0), length(r1), length(start), length(end))
  r0 <- rep_len(r0, nn)
  r1 <- rep_len(r1, nn)
  start <- rep_len(start, nn)
  end <- rep_len(end, nn)

  start <- start %% 360
  end <- end %% 360
  if(end >= start) {
    end <- start - start - (360 - end)
  }
  mm <- ceiling(abs(300 * radian(start - end)))

  data <- purrr::pmap_dfr(list(r0, r1, start, end, mm, seq_len(nn)),
                  function(.r0, .r1, .start, .end, .mm, .nn) {
                    if(.end >= .start) {
                      .end <- .start - .start - (360 - .end)
                    }
                    tt <- radian(seq(.start, .end, length.out = .mm))
                    tt <- c(tt, rev(tt))
                    rr <- rep(c(.r0, .r1), each = .mm)
                    tibble(x = cos(tt) * rr,
                           y = sin(tt) * rr,
                           ids = .nn)
                  })
  geom_polygon(aes_(x = ~x, y = ~y, group = ~ids),
               data = data, inherit.aes = FALSE, ...)

}
