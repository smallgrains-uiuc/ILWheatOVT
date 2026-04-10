#' Filter table based on starred selections
#'
#' Subsets entries to only include starred varieties.
#'
#' @param df Wide format trait table.
#' @param starred Character vector of starred varieties.
#' @param show_only Logical indicating whether to show only starred varieties.
#'
#' @return Filtered data frame.
#' @export
filter_by_starred <- function(df, starred, show_only) {
  if (show_only) {
    df <- df[df$number %in% starred, , drop = FALSE]
  }
  df
}