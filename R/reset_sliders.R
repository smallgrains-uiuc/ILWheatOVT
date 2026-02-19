#' Reset Shiny sliders to full trait ranges
#'
#' Updates the `md_range` slider input
#' based on the current values in the filtered data.
#'
#' @param session Shiny session object.
#' @param table_data_reactive Reactive expression that returns filtered table.
#' @param region Current region selection. Either `"South"` or `"North"`.
#'
#' @return No return value; called for side effects.
#' @export
reset_sliders <- function(session, table_data_reactive, region) {
  df <- table_data_reactive()
  req(df)
  ranges <- get_trait_ranges(df, region)

  updateSliderInput(session, "md_range",
                    min = ranges$Maturity.Date[1],
                    max = ranges$Maturity.Date[2],
                    value = ranges$Maturity.Date)
}