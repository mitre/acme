# Supplemental Functions for Implementing Adult EHR Cleaning
# By Hannah De los Santos
# Originated on: 10/8/2020

# functions for all ----

# function to remove BIVs, based on cutoffs for the given method
# inputs:
#   subj_df: data frame with measurements of a given type
#   type: height, weight, or bmi
#   biv_df: data frame with BIV cutoffs for the given type
#   include: default F, whether or not to include the endpoints
# outputs:
#   logical, true if the given record should be removed due to being a BIV
remove_biv <- function(subj_df, type, biv_df, include = F){
  too_low <- remove_biv_low(subj_df, type, biv_df, include)
  too_high <- remove_biv_high(subj_df, type, biv_df, include)
  
  return(too_low | too_high)
}

# function to remove only the low end of BIVs, based on cutoffs for the given 
# method (for intermediate processing only)
# inputs:
#   subj_df: data frame with measurements of a given type
#   type: height, weight, or bmi
#   biv_df: data frame with BIV cutoffs for the given type
#   include: default F, whether or not to include the endpoints
# outputs:
#   logical, true if the given record should be removed due to being a BIV
remove_biv_low <- function(subj_df, type, biv_df, include = F){
  if (!include){
    too_low <- subj_df$measurement < biv_df[type, "low"]
  } else {
    too_low <- subj_df$measurement <= biv_df[type, "low"]
  }
  
  return(too_low)
}

# function to remove only the high end of BIVs, based on cutoffs for the given 
# method (for intermediate processing only)
# inputs:
#   subj_df: data frame with measurements of a given type
#   type: height, weight, or bmi
#   biv_df: data frame with BIV cutoffs for the given type
#   include: default F, whether or not to include the endpoints
# outputs:
#   logical, true if the given record should be removed due to being a BIV
remove_biv_high <- function(subj_df, type, biv_df, include = F){
  if (!include){
    too_high <- subj_df$measurement > biv_df[type, "high"]
  } else {
    too_high <- subj_df$measurement >= biv_df[type, "high"]
  }
  
  return(too_high)
}