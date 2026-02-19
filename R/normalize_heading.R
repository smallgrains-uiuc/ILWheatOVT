#' Normalize heading dates (North only)
#'
#' Rreplaces raw heading dates with the number of days
#' after the earliest North phenology entry.
#'
#' @param df Long-format trait table.
#'
#' @return Data frame with normalized `Heading.Date` values.
#' @export
normalize_heading <- function(df) {
  is_heading <- df$trait == "Heading.Date"
  df$study[is_heading] <- gsub("Urbana", "NorthPhenology", df$study[is_heading])
  dates <- as.numeric(df$value[is_heading])
  north_date <- is_heading & df$study == "NorthPhenology"
  if(any(north_date, na.rm = TRUE)) {
    df$value[north_date] <- dates[north_date[is_heading]] - min(dates[north_date[is_heading]], na.rm = TRUE)
  }
  df
}