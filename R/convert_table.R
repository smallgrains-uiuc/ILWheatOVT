#' Convert wide table values to numeric
#'
#' Rounds all numeric columns to one decimal place, except for company, number,
#' and categorical columns such as scab resistance and jointing category.
#'
#' @param df Wide format data frame produced by `prepare_table()`.
#'
#' @return Data frame with numeric columns rounded to one decimal place.
#' @export
convert_table <- function(df) {
  scab_col <- "Scab.Category_ScabNursery"
  jointing_col <- grep("Jointing.Category", names(df), value = TRUE)
  
  for (col in names(df)) {
    if (col %in% c("company", "number", scab_col, jointing_col)) next
    df[[col]] <- round(as.numeric(df[[col]]), 1)
  }
  
  df
}