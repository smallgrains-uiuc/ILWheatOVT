#' Set jointing stage as ordered factor
#'
#' Converts jointing stage columns to ordered factors:
#' Early (E) < Medium (M) < Late (L).
#'
#' @param df Data frame containing jointing category columns.
#'
#' @return Data frame with ordered jointing stage columns.
#' @export
jointing_level <- function(df) {
  jtd_cols <- grep("^Jointing.Category_", names(df), value = TRUE)
  
  for (col in jtd_cols) {
    values <- trimws(as.character(df[[col]]))
    df[[col]] <- factor(
      values,
      levels = c("E", "M/E", "M", "M/L", "L"),
      ordered = TRUE
    )
  }
  
  df
}
