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
  if ("Scab.Category_ScabNursery" %in% colnames(df)) {
    df$Scab.Category_ScabNursery <- factor(
      df$Scab.Category_ScabNursery,
      levels = c("S", "MS", "M", "MR"),
      ordered = TRUE
    )
  }
  df
}