#' Normalize maturity dates within region
#'
#' Replaces raw maturity dates with the number of days
#' after the earliest entry in the same region.
#' Also standardizes study names.
#'
#' @param df Long format trait table.
#'
#' @return Data frame with normalized `Maturity.Date` values.
#' @export
normalize_maturity <- function(df) {
  is_maturity <- df$trait == "Maturity.Date"
  
  df$study[is_maturity] <- gsub("StPeter", "SouthPhenology", df$study[is_maturity])
  df$study[is_maturity] <- gsub("Urbana", "NorthPhenology", df$study[is_maturity])
  
  dates <- as.numeric(df$value[is_maturity])
  
  south_date <- is_maturity & df$study == "SouthPhenology"
  north_date <- is_maturity & df$study == "NorthPhenology"
  
  if(any(south_date, na.rm = TRUE)) {
    df$value[south_date] <- dates[south_date[is_maturity]] - min(dates[south_date[is_maturity]], na.rm = TRUE)
  }
  if(any(north_date, na.rm = TRUE)) {
    df$value[north_date] <- dates[north_date[is_maturity]] - min(dates[north_date[is_maturity]], na.rm = TRUE)
  }
  
  df
}