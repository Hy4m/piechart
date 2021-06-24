#' @title Coerce to a Piechart data
#' Functions to coerce an object to a piechart data if possible.
#' @param x any \code{R} object.
#' @param start offset of starting point from 3 o'clock in degree.
#' @param open size of opening angle in degree.
#' @param r0,r1 start and end radius of arc heatmap.
#' @param row_names,col_names character vector.
#' @param extra_mat named list of extra matrix data.
#' @param ... extra parameters.
#' @return a piechart data.
#' @rdname as_piechart_data
#' @examples
#' as_piechart_data(as.matrix(mtcars))
#' @export
as_piechart_data <- function(x, ...) {
  UseMethod("as_piechart_data")
}

#' @rdname  as_piechart_data
#' @method as_piechart_data matrix
#' @export
as_piechart_data.matrix <- function(x,
                                    start = 90,
                                    open  = 90,
                                    r0    = 0.5,
                                    r1    = 1,
                                    row_names = NULL,
                                    col_names = NULL,
                                    extra_mat = list(),
                                    ...)
{
  check_extra_mat_name(extra_mat)
  n <- nrow(x)
  m <- ncol(x)
  if(n < 1 || m < 1) {
    return(empty_piechart_data)
  }

  start <- start %% 360
  open <- open %% 360

  if(r0 > r1) {
    temp <- r0
    r0 <- r1
    r1 <- temp
  }

  ur <- (r1 - r0) / m
  r0 <- r0 + (seq_len(m) - 1) * ur
  r1 <- r1 - rev(seq_len(m) - 1) * ur

  sum_value <- n * open / (360 - open) + n

  rnm <- rownames(x) %||% row_names %||% paste0("Row", seq_len(n))
  cnm <- colnames(x) %||% col_names %||% paste0("Col", seq_len(m))
  ids <- expand.grid(row_names = rnm,
                     col_names = cnm)
  ids$value <- as.vector(x)
  ids$r0 <- rep(r0, each = n)
  ids$r1 <- rep(r1, each = n)
  ids$group <- rep(seq_len(m), each = n)


  ex_nm <- names(extra_mat)
  for(i in ex_nm) {
    ids[[i]] <- as.vector(as.matrix(extra_mat[[i]]))
  }

  group <- NULL
  data <- piechart_data(ids,
                        mapping = aes(value = 1, r0 = r0, r1 = r1,
                                      label = row_names, group = group),
                        start = start,
                        sum_value = sum_value,
                        ...)
  ctext <- tibble::tibble(.r = (r0 + r1) / 2,
                          .label = cnm,
                          .start = start,
                          .open = open)
  structure(.Data = data, ctext = ctext,
            class = c("hp_data", class(data)))
}

#' @rdname  as_piechart_data
#' @method as_piechart_data dist
#' @export
as_piechart_data.dist <- function(x, ...) {
  x <- as.matrix(x)
  as_piechart_data(x, ...)
}

#' @noRd
is_hp_data <- function(x) inherits(x, "hp_data")

#' @noRd
check_extra_mat_name <- function(l)
{
  n <- length(l)
  if(n > 0) {
    name <- names(l)
    if(is.null(name) || length(unique(name)) != n) {
      stop(
        "Names of extra_mat check: ",
        "The elements of 'extra_mat' must have unique name.",
        call. = FALSE)
    }

    preserved_name <- c("row_names", "col_names", "value",
                        "r0", "r1", "group")
    if(any(name %in% preserved_name)) {
      stop(
        "Names of extra_mat check: ",
        paste(name[name %in% preserved_name], collapse = ", "),
        " are preserved.",
        call. = FALSE)
    }
  }
}
