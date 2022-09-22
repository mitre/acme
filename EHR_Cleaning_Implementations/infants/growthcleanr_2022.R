# Implementing growthcleanr (2022)
# By Hannah De los Santos
# Originated on: 9/1/22

library(growthcleanr)

# function to clean height and weight data by growthcleanr (no intermediate 
# values)
# inputs:
# df: data frame with 7 columns:
#   id: row id, must be unique
#   subjid: subject id
#   sex: sex of subject
#   age_days: age, in days
#   param: HEIGHTCM or WEIGHTKG
#   measurement: height or weight measurement
# inter_vals: boolean, return intermediate values
# outputs:
#   df, with additional columns:
#     result, which specifies whether the height measurement should be included,
#       or is implausible.
#     reason, which specifies, for implausible values, the reason for exclusion,
#       and the step at which exclusion occurred.
growthcleanr_clean_both <- function(df, inter_vals = F){
  # begin implementation ----
  
  # preallocate final designation
  df$result <- "Include"
  df$reason <- ""
  df$id <- as.character(df$id)
  rownames(df) <- df$id
  
  # 1 h/w: H/W run growthcleanr ----
  # the 1+h/w will be added in the reason
  step <- "run growthcleanr"
  
  res <- cleangrowth(
    df$subjid,
    df$param,
    df$age_days,
    df$sex,
    df$measurement
  )
  res <- as.character(res)
  
  # update output
  # align exclusion names
  not_incl <- res != "Include" 
  df$result[not_incl] <- "Implausible"
  df$reason[not_incl & df$param == "HEIGHTCM"] <- 
    paste0(res[not_incl & df$param == "HEIGHTCM"], 
           ", Step 1h, H ",
           step)
  df$reason[not_incl & df$param == "WEIGHTKG"] <- 
    paste0(res[not_incl & df$param == "WEIGHTKG"], 
           ", Step 1w, W ",
           step)
  
  return(df)
}