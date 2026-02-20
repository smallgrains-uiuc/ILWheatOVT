#' Get current slider ranges for maturity date
#'
#' Computes minimum and maximum values (rounded) for regional maturity date.
#' Initialize Shiny slider inputs.
#'
#' @param df Wide format trait table.
#' @param region Either `"North"` or `"South"`.
#'
#' @return A list with one element `$Maturity.Date`.
#' @export
get_maturity_range <- function(df, region) {
  suf_pheno <- if (region == "North") "NorthPhenology" else "SouthPhenology"
  
  col <- paste0("Maturity.Date_", suf_pheno)
  if (col %in% names(df)) {
    col_data <- df[[col]]
    output <- list(
      Maturity.Date = c(
        floor(min(col_data, na.rm = TRUE)),
        ceiling(max(col_data, na.rm = TRUE))
      )
    )
  } else {
    output <- list(Maturity.Date = c(0, 0))
  }
  
  output
}