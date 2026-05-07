#' Normalize target traits as percent of study average
#'
#' Replaces raw values of Grain.Yield and Test.Weight
#' with the percentage of the average within each study/site.
#' The study average is set to 100.
#'
#' @param df Long format trait table.
#'
#' @return Data frame with normalized values for target traits.
#' @export
normalize_percentage <- function(df) {
  target_traits <- c("Grain.Yield", "Test.Weight")
  north_sites <- c("NorthRegionalAverage", "Urbana", "Hampshire", "Perry")
  south_sites <- c("SouthRegionalAverage", "StPeter", "Addieville", "Elkville")
  
  site_region <- rep(NA_character_, nrow(df))
  site_region[df$study %in% north_sites] <- "North"
  site_region[df$study %in% south_sites] <- "South"
  
  is_target_site <- df$trait %in% target_traits & !is.na(site_region)
  groups <- unique(data.frame(
    trait = df$trait[is_target_site],
    region = site_region[is_target_site],
    site = df$study[is_target_site],
    stringsAsFactors = FALSE
  ))
  
  for (i in seq_len(nrow(groups))) {
    g <- groups[i, ]
    
    idx <- is_target_site &
      df$trait == g$trait &
      site_region == g$region &
      df$study == g$site
    
    vals <- as.numeric(df$value[idx])
    avg_val <- mean(vals, na.rm = TRUE)
    
    if (is.finite(avg_val) && avg_val != 0) {
      df$value[idx] <- vals / avg_val * 100
    }
  }
  
  df
}
