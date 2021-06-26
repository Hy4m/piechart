#' Init piechart data
#' @title Piechat data
#' @param data a data frame.
#' @param mapping default list of aesthetic mappings to use for plot.
#' @param facet NULL or a formula.
#' @param sort_by a character vector of variable name.
#' @param decreasing logical. Should the sort order be increasing or decreasing?
#' @param start,end offset of starting and ending point from 3 o'clock in degree.
#' @param steps increment of the sequence in radians.
#' @param ... extra parameters.
#' @return a data frame.
#' @rdname piechart_data
#' @importFrom rlang eval_tidy
#' @importFrom utils modifyList
#' @importFrom dplyr bind_cols
#' @importFrom stats complete.cases
#' @examples
#' library(ggplot2)
#' piechart_data(mtcars, aes(value = wt))
#' @export
piechart_data <- function(data,
                          mapping,
                          facet = NULL,
                          sort_by = NULL,
                          decreasing = TRUE,
                          start = 0,
                          end = 360,
                          steps = 0.01,
                          ...)
{
  if(!is_uneval(mapping)) {
    stop("Mapping should be created with `aes()` or `aes_()`.", call. = FALSE)
  }

  if(!is.null(sort_by)) {
    params <- c(unname(data[sort_by]), list(decreasing = decreasing))
    data <- data[do.call(order, params), , drop = FALSE]
  }

  base_mapping <- aes(r0 = 0.5, r1 = 1, sep = 0, label = NA)
  mapping <- modifyList(base_mapping, mapping)
  mapping <- modifyList(mapping, list(...))

  base_data <- extract_data(data, mapping[c("value", "r0", "r1", "sep",
                                            "label")])
  non_na <- complete.cases(base_data[ , c("value", "r0", "r1", "sep"),
                                      drop = FALSE])

  if(!any(non_na)) {
    return(
      dplyr::bind_cols(empty_piechart_data(), data[0, , drop = FALSE])
    )
  }

  value <- r0 <- r1 <- sep <- group <- label <- NULL
  base_data <- dplyr::rename(base_data[non_na, , drop = FALSE],
                             .value = value, .r0 = r0, .r1 = r1, .sep = sep,
                             .label = label)
  all_data <- dplyr::bind_cols(base_data, data[non_na, , drop = FALSE])
  data <- split_by_group(all_data, facet = facet)
  ex_name <- setdiff(names(all_data), c(".value", ".r0", ".r1", ".sep",
                                        ".group", ".label"))

  out <- purrr::map2_dfr(data, seq_along(data), function(.data, .id) {
    dd <- point_to_ring(value     = .data$.value,
                        label     = .data$.label,
                        start     = start,
                        end       = end,
                        r0        = .data$.r0,
                        r1        = .data$.r1,
                        sep       = .data$.sep,
                        steps     = steps)
    dd <- dplyr::bind_cols(dd, .data[dd$.group, ex_name, drop = FALSE])
    dd$.group <- paste(.id, dd$.group, sep = "--")
    dd
    })
  out$.value <- rlang::eval_tidy(mapping$value, out)
  structure(out, class = c("piechart_data", class(out)))
}

#' @noRd
extract_data <- function(data, mapping) {
  nm <- names(mapping)
  n <- nrow(data)
  if(length(nm) == 0L) return(NULL)
  dd <- purrr::map(mapping, function(.m) {
    if(is.atomic(.m)) {
      .m
    } else {
      rlang::eval_tidy(.m, data)
    }
  })
  names(dd) <- nm
  tibble::as_tibble(dd)
}

#' @noRd
is_piechart_data <- function(x) inherits(x, "piechart_data")

#' @noRd
is_uneval <- function(x) inherits(x, "uneval")

#' @noRd
empty_piechart_data <- function() {
  dd <- tibble(.x       = numeric(0),
               .y       = numeric(0),
               .group   = numeric(0),
               .ratio   = numeric(0),
               .label   = character(0),
               .angle   = numeric(0),
               .r0      = numeric(0),
               .r1      = numeric(0),
               .isLabel = logical(0))
  structure(.Data = dd, class = c("piechart_data", class(dd)))
}

#' @importFrom stats as.formula
#' @importFrom stats model.frame
#' @noRd
split_by_group <- function(data, facet = NULL) {
  if(is.null(facet)) {
    out <- list(data)
  } else {
    char <- as.character(facet)
    formula <- as.formula(paste(char[char != "."], collapse = ""))
    dd <- model.frame(formula = formula, data = data)
    out <- split(data, dd)
  }
  out
}
