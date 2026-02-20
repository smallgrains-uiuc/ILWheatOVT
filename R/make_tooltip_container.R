#' Build a table header container with tooltips
#'
#' Generates a two-row HTML table header grouping columns by study and adding
#' explanatory tooltips for traits.
#'
#' @param df Wide format trait table.
#'
#' @return HTML tag list representing the table header.
#' @importFrom shiny tags HTML
#' @export
make_tooltip_container <- function(df) {
  
  tooltip_rules <- list(
    MaturityHeading = list(
      pattern = "Maturity.Date|Heading.Date",
      text = "Days after the earliest variety.\n0 = earliest."
    ),
    Jointing = list(
      pattern = "Jointing.Category",
      text = "Jointing timing: Early (E), Medium (M), Late (L)."
    ),
    Scab = list(
      pattern = "Scab.Category",
      text = "FHB resistance: S, MS, M, MR."
    ),
    Index = list(
      pattern = "Index",
      text = "Disease severity index."
    ),
    Mosaic = list(
      pattern = "SBMV",
      text = "Performance under SBMV pressure."
    )
  )
  
  cols <- names(df)
  data_cols <- cols[!cols %in% c("company","number")]
  
  parsed <- do.call(rbind, strsplit(data_cols, "_"))
  trait <- parsed[,1]
  study <- parsed[,2]
  
  study_groups <- split(data_cols, study)
  
  format_study <- function(x) {
    x <- gsub("([a-z])([A-Z])", "\\1 \\2", x, perl = TRUE) # lowercase + space + word
    x <- gsub("([A-Z]+)([A-Z][a-z])", "\\1 \\2", x, perl = TRUE) # All caps + space + word
    x
  }
  format_trait <- function(x) gsub("\\.", "<br>", x)
  
  # Study header
  header_row1 <- list(
    tags$th("company", rowspan=2, style="text-align:center;"),
    tags$th("number",  rowspan=2, style="text-align:center;")
  )
  
  for (st in unique(study)) {
    n <- sum(study == st)
    study_label <- format_study(st)
    header_row1[[length(header_row1)+1]] <- tags$th(study_label, colspan=n, style="text-align:center; font-weight:700;")
  }
  
  # Trait header
  header_row2 <- lapply(seq_along(data_cols), function(i) {
    col <- data_cols[i]
    tips <- unlist(lapply(tooltip_rules, function(rule) {
      if (grepl(rule$pattern, col)) rule$text else NULL
    }))
    
    trait_label <- format_trait(trait[i])
    if (length(tips)) tags$th(HTML(trait_label), title=paste(tips,collapse="\n\n"))
    else tags$th(HTML(trait_label))
  })
  
  withTags(
    table(class="display",
          thead(
            tags$tr(header_row1),
            tags$tr(header_row2)
          )
    )
  )
}