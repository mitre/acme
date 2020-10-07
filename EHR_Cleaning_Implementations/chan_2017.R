# Implementing Chan, et al. (2017)
# By Hannah De los Santos
# Originated on: 10/5/2020

# paper: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5359164/

# Note: will have to update to data table upon completion for speed
# Note 2: must remove missing values before running method

# supporting functions ----

# note this is slightly different from the previous versions -- adds a variable
# for saying whether to include the cutoff in the cutoff
remove_biv <- function(subj_df, type, biv_df, include = F){
  if (!include){
    too_low <- subj_df$measurement < biv_df[type, "low"]
    too_high <- subj_df$measurement > biv_df[type, "high"]
  } else {
    too_low <- subj_df$measurement <= biv_df[type, "low"]
    too_high <- subj_df$measurement >= biv_df[type, "high"]
  }
  
  return(too_low | too_high)
}

# implement chan, et al. ----

# function to clean height and weight data by chan, et al.
# inputs:
# df: data frame with 7 columns:
#   id: row id
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
chan_clean_both <- function(df){
  # method specific constants ----
  # this includes specified cutoffs, etc.
  
  # BIVs
  biv_df <- data.frame(
    "low" = c(48*2.54, 22.7, 10),
    "high" = c(84*2.54, 340.2, 100)
  )
  rownames(biv_df) <- c("height", "weight", "bmi")
  
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
    
    # 1h, H BIV ----
    # 1h. remove biologically impossible height records
    step <- "1h, H BIV"
    
    criteria <- remove_biv(subj_df, "height", biv_df)
    subj_keep[criteria] <- "Implausible"
    
    subj_df <- subj_df[!criteria,]
    
    # 2h, H check SD away from mean ----
    # 2h. Exclude heights that were greater than 3 standard deviations from the 
    # mean.
    step <- "2h, H check SD away from mean"
    
    if (nrow(subj_df) > 0){
      # calculate mean and standard deviation
      avg_ht <- mean(subj_df$measurement)
      st_dev_ht <- sd(subj_df$measurement)
      
      # calculate exclusion criteria
      criteria <- abs(subj_df$measurement - avg_ht) > 3*st_dev_ht
      
      subj_keep[as.character(subj_df$id)][criteria] <- "Implausible"
    }
    
    # add the full calculation
    h_df$result <- subj_keep
    
    # add results to full dataframe
    df[as.character(h_df$id), "result"] <- h_df$result
    
    # move to weight ----
    
    w_df <- df[df$param == "WEIGHTKG" & slog,]
    
    subj_keep <- rep("Include", nrow(w_df))
    names(subj_keep) <- w_df$id
    
    subj_df <- w_df
    
    # 1w, W BIV ----
    # 1w. remove biologically impossible weight records
    step <- "1w, W BIV"
    
    criteria <- remove_biv(subj_df, "weight", biv_df)
    subj_keep[criteria] <- "Implausible"
    
    subj_df <- subj_df[!criteria,]
    
    # 2w, W BMI BIV ----
    # 2w. Calculate BMI based on average height, then remove BMI bivs.
    step <- "2w, W BMI BIV"
    
    if (sum(h_df$result == "Include") > 0 & nrow(subj_df) > 0){
      # calculate avg height for subject
      avg_ht <- mean(h_df$measurement[h_df$result == "Include"])
      
      # calulate bmi
      bmi_df <- data.frame(
        "measurement" = w_df$measurement/((avg_ht/100)^2)
      )
      
      criteria <- remove_biv(bmi_df, "bmi", biv_df, include = T)
      subj_keep[as.character(w_df$id[criteria])] <- "Implausible"
      
      subj_df <- subj_df[!criteria,]
    }
    
    # 3w, W check SD away from mean ----
    # 3w. Exclude weights that were greater than 3 standard deviations from the mean.
    step <- "3w, W check SD away from mean"
    
    if (nrow(subj_df) > 0){
      # calculate mean and standard deviation
      avg_wt <- mean(subj_df$measurement)
      st_dev_wt <- sd(subj_df$measurement)
      
      # calculate exclusion criteria
      criteria <- abs(subj_df$measurement - avg_wt) > 3*st_dev_wt
      
      subj_keep[as.character(subj_df$id)][criteria] <- "Implausible"
    }
    
    # add the full calculation
    w_df$result <- subj_keep
    
    # add results to full dataframe
    df[as.character(w_df$id), "result"] <- w_df$result
  }
  
  return(df)
}