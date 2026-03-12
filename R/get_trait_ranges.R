#' Get current slider ranges for grain yield and maturity date
#'
#' Computes minimum and maximum values (rounded) for regional average grain yield
#' and regional phenology maturity date.
#' Initialize Shiny slider inputs.
#'
#' @param df Wide format trait table.
#' @param region Either `"North"` or `"South"`.
#'
#' @return A list with two elements: `$Grain.Yield` and `$Maturity.Date`,
#'   each a numeric vector of length 2 (`min`, `max`).
#' @export
get_trait_ranges <- function(df, region) {
  suf_avg <- if (region == "North") "NorthRegionalAverage" else "SouthRegionalAverage"
  suf_pheno <- if (region == "North") "NorthPhenology" else "SouthPhenology"
  
  traits_avg <- "Grain.Yield"
  traits_pheno <- "Maturity.Date"
  
  output <- list()
  
  for (t in traits_avg) {
    col <- paste0(t, "_", suf_avg)
    if (col %in% names(df)) {
      col_data <- df[[col]]
      output[[t]] <- c(floor(min(col_data, na.rm = TRUE)),
                       ceiling(max(col_data, na.rm = TRUE)))
    } else {
      output[[t]] <- c(0, 0)
    }
  }
  
  for (t in traits_pheno) {
    col <- paste0(t, "_", suf_pheno)
    if (col %in% names(df)) {
      col_data <- df[[col]]
      output[[t]] <- c(floor(min(col_data, na.rm = TRUE)),
                       ceiling(max(col_data, na.rm = TRUE)))
    } else {
      output[[t]] <- c(0, 0)
    }
  }
  
  output
}