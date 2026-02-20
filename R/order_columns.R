#' Order trait columns by study and trait priority
#'
#' Columns are ordered first by study, then by trait.
#' Company and number remain fixed as the first two columns.
#'
#' @param df Wide format trait table.
#' @param region Either `"South"` or `"North"`; determines study ordering.
#'
#' @return Data frame with reordered columns.
#' @export
order_columns <- function(df, region) {
  
  # Desired order
  study_order <- if (region == "South") {
    c("SouthPhenology","SouthRegionalAverage","StPeter","Addieville","Elkville",
      "ScabNursery","SBMVNursery")
  } else {
    c("NorthPhenology","NorthRegionalAverage","Urbana","Hampshire","Perry",
      "ScabNursery","SBMVNursery")
  }
  
  trait_order <- c(
    "Maturity.Date","Heading.Date","Jointing.Category",
    "Grain.Yield","Test.Weight","Plant.Height",
    "Scab.Category","Scab.Index","SBMV.Rating"
  )
  
  # Column rank
  col_priority <- function(colname) {
    parts <- strsplit(colname, "_")[[1]]
    s_rank <- match(parts[2], study_order, nomatch = length(study_order)+1)
    t_rank <- match(parts[1], trait_order, nomatch = length(trait_order)+1)
    s_rank * 10 + t_rank
  }
  
  priority_scores <- sapply(names(df)[-c(1:2)], col_priority)
  new_order <- c(names(df)[1:2], names(df)[-c(1:2)][order(priority_scores)])
  
  df[, new_order]
}