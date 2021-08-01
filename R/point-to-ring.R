#' @importFrom tibble tibble
#' @importFrom purrr pmap_dfr
#' @noRd
point_to_ring <- function(value = runif(5),
                          label = NULL,
                          start = 0,
                          end = 360,
                          r0 = 0.5,
                          r1 = 1,
                          sep = 0,
                          steps = 0.001)
{
  if(!is.numeric(value) || length(value) == 0 || any(value < 0) ||
     any(!is.finite(value)) || !any(value > 0)) {
    stop("Data error: 'value' should be positive and finite.", call. = FALSE)
  }

  ratio <- value / sum(value, na.rm = TRUE)
  ll <- length(ratio)
  label <- label %||% names(value) %||% NA
  label <- rep_len(label, ll)

  r0 <- rep_len(r0, ll)
  r1 <- rep_len(r1, ll)
  temp <- r0
  r0 <- ifelse(r0 > r1, r1, r0)
  r1 <- ifelse(r1 < temp, temp, r1)

  sep <- rep_len(sep, ll)
  if(any(sep > ratio * 360)) {
    stop("The value of 'sep' is too large.", call. = FALSE)
  }

  start <- start %% 360
  end <- end %% 360
  if(end >= start) {
    end <- start - start - (360 - end)
  }

  start <- radian(start)
  end <- radian(end)
  sep <- radian(sep)

  width <- ratio * (abs(end - start))
  net_width <- width - sep
  s <- c(start,  start - cumsum(width)[-ll])
  e <- s - net_width
  param <- list(s, e, seq_len(ll), ratio, r0, r1)

  polygon_pos <- purrr::pmap_dfr(param, function(.start, .end, .n, .ratio, .r0, .r1) {

    if(.start > .end) {
      steps <- -steps
    }
    angles <- seq(.start, .end, by = steps)
    angles[length(angles)] <- .end

    tibble(.x       = c(cos(angles) * .r0, cos(rev(angles)) * .r1),
           .y       = c(sin(angles) * .r0, sin(rev(angles)) * .r1),
           .group   = .n,
           .ratio   = .ratio,
           .isLabel = FALSE)
  })

  .angle <- NULL
  label_pos <- tibble(.ratio   = ratio,
                      .label   = label,
                      .angle   = (s + e) / 2,
                      .r0      = r0,
                      .r1      = r1,
                      .x       = cos(.angle) * (r0 + r1) / 2,
                      .y       = sin(.angle) * (r0 + r1) / 2,
                      .group   = seq_len(ll),
                      .isLabel = TRUE)

  dplyr::bind_rows(polygon_pos, label_pos)
}

#' @noRd
degree <- function(x) x / pi * 180

#' @noRd
radian <- function(x) x / 180 * pi
