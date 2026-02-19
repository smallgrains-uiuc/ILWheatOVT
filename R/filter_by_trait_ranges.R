#' Filter table based on grain yield and maturity date ranges
#'
#' Subsets entries based on grain yield and maturity date slider ranges selected.
#'
#' @param df Wide format trait table.
#' @param region Either `"North"` or `"South"`.
#' @param y_range Numeric vector of length 2 for grain yield range.
#' @param md_range Numeric vector of length 2 for maturity date range.
#'
#' @return Filtered data frame.
#' @export
filter_by_trait_ranges <- function(df, region, y_range, md_range) {
  suf_avg <- if (region == "North") "NorthRegionalAverage" else "SouthRegionalAverage"
  suf_pheno <- if (region == "North") "NorthPhenology" else "SouthPhenology"
  
  traits_avg <- list(
    list(name = "Grain.Yield", range = y_range)
  )
  
  traits_pheno <- list(
    list(name = "Maturity.Date", range = md_range)
  )
  
  for (t in traits_avg) {
    col <- paste0(t$name, "_", suf_avg)
    if (col %in% names(df)) {
      col_data <- df[[col]]
      df <- df[col_data >= t$range[1] & col_data <= t$range[2], , drop = FALSE]
    }
  }
  
  for (t in traits_pheno) {
    col <- paste0(t$name, "_", suf_pheno)
    if (col %in% names(df)) {
      col_data <- df[[col]]
      df <- df[col_data >= t$range[1] & col_data <= t$range[2], , drop = FALSE]
    }
  }
  
  df
}