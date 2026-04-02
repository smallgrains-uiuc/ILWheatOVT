#' Filter table based on search input
#'
#' Subsets entries based on user search input of company or variety
#'
#' @param df Wide format trait table.
#' @param search Character string for search input.
#'
#' @return Filtered data frame.
#' @export
filter_by_search <- function(df, search) {
  if (!is.null(search) && search != "") {
    df <- df[
      grepl(search, df$company, ignore.case = TRUE) |
        grepl(search, df$number, ignore.case = TRUE),
      ,
      drop = FALSE
    ]
  }
  df
}