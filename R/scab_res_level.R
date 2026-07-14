#' Set scab resistance as ordered factor
#'
#' Converts scab resistance rating into an ordered factor:
#' S < MS < M < MR.
#'
#' @param df Data frame containing column `Scab.Category_ScabNursery`.
#'
#' @return Data frame with ordered scab resistance column.
#' @export
scab_res_level <- function(df) {
  scab_cols <- grep("^Scab\\.Category", names(df), value = TRUE)
  
  for (col in scab_cols) {
    values <- trimws(as.character(df[[col]]))
    values[tolower(values) == "pending"] <- "pending"
    
    df[[col]] <- factor(
      values,
      levels = c("S", "MS", "M", "MR", "pending"),
      ordered = TRUE
    )
  }
  
  df
}
