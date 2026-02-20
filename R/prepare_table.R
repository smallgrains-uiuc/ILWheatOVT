#' Prepare wheat variety test data in wide format
#'
#' Filters the raw dataset by region and summary type, selects studies and traits,
#' reshapes to wide format, converts numeric columns, and orders columns.
#'
#' @param raw_df Data frame containing long format data. 
#'   Defaults to `WheatOVT25`.
#'   Includes columns `company`, `number`, `study`, `trait`, `value`, `SouthEntNo`, and `NorthEntNo`.
#' @param Region Character. Either `"South"` or `"North"`.
#' @param smryType Character. Either `"Compact"` or `"Detailed"`.
#'
#' @return A data frame in wide format with trait-study combinations as columns.
#'   Numeric values are rounded to one decimal.
#' @export
prepare_table <- function(raw_df = WheatOVT25, Region = 'South', smryType = 'Compact' ){
  
  # select region
  if(Region=='South'){
    df_region <- raw_df[which(!is.na(raw_df$SouthEntNo)),]
  }
  if(Region=='North'){
    df_region <- raw_df[which(!is.na(raw_df$NorthEntNo)),]
  }
  
  #Select summary type
  if(smryType == 'Compact' & Region == 'South'){
    targetStudies <- c('SouthRegionalAverage', 'ScabNursery', 'SouthPhenology')
    targetTraits <- c('Grain.Yield','Test.Weight', 'Scab.Category', 'Maturity.Date')
  }
  if(smryType == 'Compact' & Region == 'North'){
    targetStudies <- c('NorthRegionalAverage', 'ScabNursery', 'NorthPhenology')
    targetTraits <- c('Grain.Yield','Test.Weight', 'Scab.Category', 'Maturity.Date')
  }
  if(smryType == 'Detailed' & Region == 'South'){
    targetStudies <- c('SouthRegionalAverage', 'SouthPhenology', 'StPeter', 'Addieville', 'Elkville','ScabNursery', 'SBMVNursery')
    targetTraits <- unique(df_region$trait)
  }
  if(smryType == 'Detailed' & Region == 'North'){
    targetStudies <- c('NorthRegionalAverage', 'NorthPhenology', 'Urbana', 'Hampshire', 'Perry','ScabNursery', 'SBMVNursery')
    targetTraits <- unique(df_region$trait)
  }
  
  # filter by studies of interest
  OutputData_selected_S<- df_region[df_region$study %in% targetStudies, ]
  OutputData_selected_S.T<- OutputData_selected_S[OutputData_selected_S$trait %in% targetTraits, ]
  
  # Wide format
  df_wide <- cast(company + number ~ trait + study, value ='value', data = OutputData_selected_S.T)
  df_wide <- convert_table(df_wide)
  df_wide <- order_columns(df_wide, Region)
  
  df_wide
}