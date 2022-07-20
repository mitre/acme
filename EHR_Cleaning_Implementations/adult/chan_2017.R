# Implementing Chan, et al. (2017)
# By Hannah De los Santos
# Originated on: 10/5/2020

# paper: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5359164/

# Note: will have to update to data table upon completion for speed
# Note 2: must remove missing values before running method

# implement chan, et al. ----

# function to clean height and weight data by chan, et al.
# inputs:
# df: data frame with at least 7 columns:
#   id: row id, must be unique
#   subjid: subject id
#   sex: sex of subject
#   age_years: age, in years
#   param: HEIGHTCM or WEIGHTKG
#   measurement: height or weight measurement
#   inter_vals: boolean, return intermediate values
# outputs:
#   df, with additional columns:
#     result, which specifies whether the height measurement should be included,
#       or is implausible.
#     reason, which specifies, for implausible values, the reason for exclusion,
#       and the step at which exclusion occurred.
#     intermediate value columns, if specified
chan_clean_both <- function(df, inter_vals = F){
  # method specific constants ----
  # this includes specified cutoffs, etc.
  
  # BIVs
  biv_df <- data.frame(
    "low" = c(48*2.54, 22.7, 10),
    "high" = c(84*2.54, 340.2, 100)
  )
  rownames(biv_df) <- c("height", "weight", "bmi")
  
  # intermediate value columns
  inter_cols <- c(
    "Step_1h_H_BIV_Low_Compare",
    "Step_1h_H_BIV_High_Compare",
    "Step_1h_Result",
    "Step_2h_Difference_from_Mean",
    "Step_2h_3SD",
    "Step_2h_Mean",
    "Step_2h_Result",
    "Step_1w_W_BIV_Low_Compare",
    "Step_1w_W_BIV_High_Compare",
    "Step_1w_Result",
    "Step_2w_Avg_Height",
    "Step_2w_BMI",
    "Step_2w_W_BIV_BMI_Low_Compare",
    "Step_2w_W_BIV_BMI_High_Compare",
    "Step_2w_Result",
    "Step_3w_Difference_from_Mean",
    "Step_3w_3SD",
    "Step_3w_Mean",
    "Step_3w_Result"
  )
  
  # begin implementation ----

  # preallocate final designation
  df$result <- "Include"
  df$reason <- ""
  rownames(df) <- df$id
  # if using intermediate values, preallocate values
  if (inter_vals){
    df[, inter_cols] <- NA
  }
  # go through each subject
  for (i in unique(df$subjid)){
    slog <- df$subjid == i
    
    # if using intermediate values, we want to start storing them
    # keep the ID to collate with the final dataframe
    inter_df <- df[slog, "id", drop = F]
    rownames(inter_df) <- inter_df$id
    
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
    subj_reason[criteria] <- paste0("Implausible, Step ",step)
    
    # if using intermediate values, we want to keep some
    if(inter_vals){
      inter_df[as.character(subj_df$id), "Step_1h_H_BIV_Low_Compare"] <- 
        remove_biv_low(subj_df, "height", biv_df)
      inter_df[as.character(subj_df$id), "Step_1h_H_BIV_High_Compare"] <- 
        remove_biv_high(subj_df, "height", biv_df)
      inter_df[as.character(subj_df$id), "Step_1h_Result"] <- criteria
    }
    
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
      subj_reason[as.character(subj_df$id)][criteria] <- 
        paste0("Implausible, Step ",step)
      
      # if using intermediate values, we want to keep some
      if(inter_vals){
        inter_df[as.character(subj_df$id), "Step_2h_Difference_from_Mean"] <- 
          abs(subj_df$measurement - avg_ht)
        inter_df[as.character(subj_df$id), "Step_2h_3SD"] <- 
          st_dev_ht
        inter_df[as.character(subj_df$id), "Step_2h_Mean"] <- 
          avg_ht
        inter_df[as.character(subj_df$id), "Step_2h_Result"] <- 
          criteria
      }
    }
    
    # add the full calculation
    h_df$result <- subj_keep
    h_df$reason <- subj_reason
    
    # add results to full dataframe
    df[as.character(h_df$id), "result"] <- h_df$result
    df[as.character(h_df$id), "reason"] <- h_df$reason
    
    # move to weight ----
    
    w_df <- df[df$param == "WEIGHTKG" & slog,]
    
    subj_keep <- rep("Include", nrow(w_df))
    subj_reason <- rep("", nrow(w_df))
    names(subj_keep) <- names(subj_reason) <- w_df$id
    
    subj_df <- w_df
    
    # 1w, W BIV ----
    # 1w. remove biologically impossible weight records
    step <- "1w, W BIV"
    
    criteria <- remove_biv(subj_df, "weight", biv_df)
    subj_keep[criteria] <- "Implausible"
    subj_reason[criteria] <- paste0("Implausible, Step ",step)
    
    # if using intermediate values, we want to keep some
    if(inter_vals){
      inter_df[as.character(subj_df$id), "Step_1w_W_BIV_Low_Compare"] <- 
        remove_biv_low(subj_df, "weight", biv_df)
      inter_df[as.character(subj_df$id), "Step_1w_W_BIV_High_Compare"] <- 
        remove_biv_high(subj_df, "weight", biv_df)
      inter_df[as.character(subj_df$id), "Step_1w_Result"] <- criteria
    }
    
    subj_df <- subj_df[!criteria,]
    
    # 2w, W BMI BIV ----
    # 2w. Calculate BMI based on average height, then remove BMI bivs.
    step <- "2w, W BMI BIV"
    
    if (sum(h_df$result == "Include") > 0 & nrow(subj_df) > 0){
      # calculate avg height for subject
      avg_ht <- mean(h_df$measurement[h_df$result == "Include"])
      
      # calulate bmi
      bmi_df <- data.frame(
        "measurement" = subj_df$measurement/((avg_ht/100)^2)
      )
      
      criteria <- remove_biv(bmi_df, "bmi", biv_df, include = T)
      subj_keep[as.character(subj_df$id[criteria])] <- "Implausible"
      subj_reason[as.character(subj_df$id[criteria])] <- 
        paste0("Implausible, Step ",step)
      
      # if using intermediate values, we want to keep some
      if(inter_vals){
        inter_df[as.character(subj_df$id), "Step_2w_Avg_Height"] <- 
          avg_ht
        inter_df[as.character(subj_df$id), "Step_2w_BMI"] <- 
          bmi_df$measurement
        inter_df[as.character(subj_df$id), "Step_2w_W_BIV_BMI_Low_Compare"] <- 
          remove_biv_low(bmi_df, "bmi", biv_df, include = T)
        inter_df[as.character(subj_df$id), "Step_2w_W_BIV_BMI_High_Compare"] <- 
          remove_biv_high(bmi_df, "bmi", biv_df, include = T)
        inter_df[as.character(subj_df$id), "Step_2w_Result"] <- criteria
      }
      
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
      subj_reason[as.character(subj_df$id)][criteria] <- 
        paste0("Implausible, Step ",step)
      
      # if using intermediate values, we want to keep some
      if(inter_vals){
        inter_df[as.character(subj_df$id), "Step_3w_Difference_from_Mean"] <- 
          abs(subj_df$measurement - avg_wt)
        inter_df[as.character(subj_df$id), "Step_3w_3SD"] <- 
          st_dev_wt
        inter_df[as.character(subj_df$id), "Step_3w_Mean"] <- 
          avg_wt
        inter_df[as.character(subj_df$id), "Step_3w_Result"] <- 
          criteria
      }
    }
    
    # add the full calculation
    w_df$result <- subj_keep
    w_df$reason <- subj_reason
    
    # add results to full dataframe
    df[as.character(w_df$id), "result"] <- w_df$result
    df[as.character(w_df$id), "reason"] <- w_df$reason
    
    # if we're using intermediate values, we want to save them
    if (inter_vals){
      df[as.character(inter_df$id), colnames(inter_df)[-1]] <- inter_df[,-1]
    }
  }
  
  return(df)
}