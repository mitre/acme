# Implementing Breland, et al. (2017)
# By Hannah De los Santos
# Originated on: 10/28/2020

# paper: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5359156/

# implement breland, et al. ----

# function to clean height and weight data by breland, et al.
# inputs:
# df: data frame with 7 columns:
#   id: row id, must be unique
#   subjid: subject id
#   sex: sex of subject
#   age_years: age, in years
#   param: HEIGHTCM or WEIGHTKG
#   measurement: height or weight measurement
# outputs:
#   df, with additional columns:
#     result, which specifies whether the height measurement should be included,
#       or is implausible.
#     reason, which specifies, for implausible values, the reason for exclusion,
#       and the step at which exclusion occurred.
breland_clean_both <- function(df){
  # method specific constants ----
  # this includes specified cutoffs, etc.
  
  # BIVs -- NOTE THESE ARE IN INCHES AND LBS
  biv_df <- data.frame(
    "low" = c(48, 75),
    "high" = c(84, 700)
  )
  rownames(biv_df) <- c("height", "weight")
  
  # begin implementation ----
  
  # preallocate final designation
  df$result <- "Include"
  df$reason <- ""
  rownames(df) <- df$id
  # go through each subject
  for (i in unique(df$subjid)){
    slog <- df$subjid == i
    
    # start with height ----
    
    h_df <- df[df$param == "HEIGHTCM" & slog,]
    
    subj_keep <- rep("Include", nrow(h_df))
    subj_reason <- rep("", nrow(h_df))
    names(subj_keep) <- names(subj_reason) <- h_df$id
    
    subj_df <- h_df
    
    # convert all heights to inches (round to nearest inch)
    subj_df$measurement <- round(subj_df$measurement/2.54)
    
    # 1h, H BIV ----
    # 1h. remove biologically implausible height records
    step <- "1h, H BIV"
    
    criteria <- remove_biv(subj_df, "height", biv_df)
    subj_keep[criteria] <- "Implausible"
    subj_reason[criteria] <- paste0("Missing, Step ",step)
    
    # add results to full dataframe
    df[names(subj_keep), "result"] <- subj_keep
    df[names(subj_reason), "reason"] <- subj_reason
    
    # then do weight ----
    
    w_df <- df[df$param == "WEIGHTKG" & slog,]
    
    subj_keep <- rep("Include", nrow(w_df))
    subj_reason <- rep("", nrow(w_df))
    names(subj_keep) <- names(subj_reason) <- w_df$id
    
    subj_df <- w_df
    
    # convert all weights to lbs (round to nearest hundreth pound)
    subj_df$measurement <- round(subj_df$measurement*2.20462, 2)
    
    # 1w, W BIV ----
    # 1w. remove biologically impossible weight records
    step <- "1w, W BIV"
    
    criteria <- remove_biv(subj_df, "weight", biv_df)
    subj_keep[criteria] <- "Implausible"
    subj_reason[criteria] <- paste0("Missing, Step ",step)
    
    subj_df <- subj_df[!criteria,]
    
    # you need at least 3 measurements to compute this
    if (nrow(subj_df) > 3){
      # 2w, W compare weight trajectory ratios ----
      # 2w. Compute ratios of weight trajectories (ratio 1: current/prior, ratio 2:
      # current/next). Compute indicator variables based on the ratios:
      # - ratio <= .67 -> indicator = -1
      # - ratio <= 1.50 -> indicator = 1
      # - else indicator = 0
      # Exclude if both ratios are -1 OR both ratios are 1.
      
      # by default, end values are included, I guess -- you can't compute both 
      # ratios
      
      # sort by age
      subj_df <- subj_df[order(subj_df$age_years),]
      
      # compute ratios
      ratio_1 <- sapply(2:(nrow(subj_df)-1), function(x){
        subj_df$measurement[x]/subj_df$measurement[x-1]
      })
      ratio_2 <- sapply(2:(nrow(subj_df)-1), function(x){
        subj_df$measurement[x]/subj_df$measurement[x+1]
      })
      
      # compute ratio indicators
      rind_1 <- rep(0, length(ratio_1))
      rind_1[ratio_1 <= .67] <- -1
      rind_1[ratio_1 >= 1.5] <- 1
      rind_2 <- rep(0, length(ratio_2))
      rind_2[ratio_2 <= .67] <- -1
      rind_2[ratio_2 >= 1.5] <- 1
      
      # criteria to remove
      criteria <- (rind_1 == -1 & rind_2 == -1) | (rind_1 == 1 & rind_2 == 1)
      subj_keep[as.character(subj_df$id[2:(nrow(subj_df)-1)])][criteria] <-
        "Implausible"
      subj_reason[as.character(subj_df$id[2:(nrow(subj_df)-1)])][criteria] <- 
        paste0("Missing, Step ",step)
    }
    
    # add results to full dataframe
    df[names(subj_keep), "result"] <- subj_keep
    df[names(subj_reason), "reason"] <- subj_reason
  }
  
  return(df)
}