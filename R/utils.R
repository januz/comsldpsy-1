#' Compute percentage
#'
#' Computes the percentage of a selected number of cases
#'   among the total number of cases.
#'
#' @param count numeric. Number of selected cases
#' @param total numeric. Total number of cases
#' @return double. Percentage rounded to one decimal
#' @export
#'
get_percent <- function(count, total) {
  round(count / total * 100, 1)
}


#' Compute number and percentage
#'
#' Computes the number and percentage of cases that fulfil
#'   a certain filter condition
#'
#' @param data tbl. A data frame to be filtered
#' @param filter character. Filter condition(s)
#' @param invert logical. Should the the filter condition(s) be inverted,
#'   Default: FALSE
#' @return numeric vector `c(number, percent)`. The number and percentage of
#'  cases fulfilling the filter condition(s)
#' @export
#'
get_n_perc_filter <- function(data, filter, invert = FALSE) {
  if (invert) {
    filter <- stringr::str_c("!(", filter, ")")
  }

  n <- data %>%
    dplyr::filter_(filter) %>%
    nrow()

  perc <- get_percent(n, nrow(data))

  c(n, perc)
}
