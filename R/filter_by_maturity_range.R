#' Filter table based on maturity date ranges
#'
#' Subsets entries based on maturity date slider ranges selected.
#'
#' @param df Wide format trait table.
#' @param region Either `"North"` or `"South"`.
#' @param md_range Numeric vector of length 2 for maturity date range.
#'
#' @return Filtered data frame.
#' @export
filter_by_maturity_range <- function(df, region, md_range) {
  suf_pheno <- if (region == "North") "NorthPhenology" else "SouthPhenology"
  
  col <- paste0("Maturity.Date_", suf_pheno)
  if (col %in% names(df)) {
    col_data <- df[[col]]
    df <- df[col_data >= md_range[1] & col_data <= md_range[2], , drop = FALSE]
  }
  
  df
}