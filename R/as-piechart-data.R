#' @title Coerce to a Piechart data
#' Functions to coerce an object to a piechart data if possible.
#' @param x any \code{R} object.
#' @param start,end offset of starting and ending point from 3 o'clock in degree.
#' @param r0,r1 start and end radius of arc heatmap.
#' @param row_names,col_names character vector.
#' @param extra_mat named list of extra matrix data.
#' @param ... extra parameters.
#' @return a piechart data.
#' @importFrom magrittr %>%
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
                                    start = 0,
                                    end  = 360,
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

  if(r0 > r1) {
    temp <- r0
    r0 <- r1
    r1 <- temp
  }
  rnm <- rownames(x) %||% row_names %||% paste0("Row", seq_len(n))
  cnm <- colnames(x) %||% col_names %||% paste0("Col", seq_len(m))

  META <- list(r0 = r0,
               r1 = r1,
               start = start,
               end = end,
               row_names = rnm,
               col_names = cnm)

  ur <- (r1 - r0) / m
  r0 <- r0 + (seq_len(m) - 1) * ur
  r1 <- r1 - rev(seq_len(m) - 1) * ur

  ids <- expand.grid(row_names = rnm,
                     col_names = cnm)
  ids$value <- as.vector(x)
  ids$r0 <- rep(r0, each = n)
  ids$r1 <- rep(r1, each = n)

  ex_nm <- names(extra_mat)
  for(i in ex_nm) {
    ids[[i]] <- as.vector(as.matrix(extra_mat[[i]]))
  }

  .isLabel <- .label <- .r0 <- .r1 <- .angle <- NULL
  data <- piechart_data(ids,
                        mapping = aes_(value = ~1, r0 = ~r0, r1 = ~r1,
                                      label = ~row_names),
                        start = start,
                        end = end,
                        facet = . ~ col_names,
                        ...) %>%
    dplyr::filter(!.isLabel) %>%
    dplyr::select(-.ratio, -.isLabel, -.label, -.angle, -.r0, -.r1)
  structure(.Data = data, META = META,
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
