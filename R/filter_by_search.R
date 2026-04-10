#' Filter table based on search input
#'
#' Subsets entries based on user search input of companies or varieties
#'
#' @param df Wide format trait table.
#' @param search Character vector for search input.
#'
#' @return Filtered data frame.
#' @export
filter_by_search <- function(df, search) {
  if (is.null(search) || length(search) == 0) {
    return(df)
  }
  
  if (is.character(search) && length(search) == 1) {
    terms <- trimws(unlist(strsplit(search, ",")))
  } else {
    terms <- trimws(search)
  }
  
  terms <- terms[terms != ""]
  
  if (length(terms) == 0) return(df)
  
  match_vec <- Reduce(`|`, lapply(terms, function(term) {
    grepl(term, df$company, ignore.case = TRUE) |
      grepl(term, df$number, ignore.case = TRUE)
  }))
  
  df[match_vec, , drop = FALSE]
}