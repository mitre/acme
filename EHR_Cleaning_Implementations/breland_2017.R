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
# inter_vals: boolean, return intermediate values
# outputs:
#   df, with additional columns:
#     result, which specifies whether the height measurement should be included,
#       or is implausible.
#     reason, which specifies, for implausible values, the reason for exclusion,
#       and the step at which exclusion occurred.
#     intermediate value columns, if specified
breland_clean_both <- function(df, inter_vals = F){
  # method specific constants ----
  # this includes specified cutoffs, etc.
  
  # BIVs -- NOTE THESE ARE IN INCHES AND LBS
  biv_df <- data.frame(
    "low" = c(48, 75),
    "high" = c(84, 700)
  )
  rownames(biv_df) <- c("height", "weight")
  
  # intermediate value columns
  inter_cols <- c(
    "Step_Preprocessing_H_Inches_Rounded",
    "Step_Preprocessing_W_Pounds_Rounded",
    "Step_Preprocessing_Result",
    "Step_1h_H_Inches_Rounded",
    "Step_1h_H_BIV_Low_Compare",
    "Step_1h_H_BIV_High_Compare",
    "Step_1h_Result",
    "Step_1w_W_Pounds_Rounded",
    "Step_1w_W_BIV_Low_Compare",
    "Step_1w_W_BIV_High_Compare",
    "Step_1w_Result",
    "Step_2w_W_Pounds_Rounded",
    "Step_2w_Ratio_1",
    "Step_2w_Ratio_1_Indicator",
    "Step_2w_Ratio_2",
    "Step_2w_Ratio_2_Indicator",
    "Step_2w_Result"
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
    
    # convert all heights to inches (round to nearest inch)
    subj_df$measurement <- round(subj_df$measurement/2.54)
    
    # if using intermediate values, we want to keep some
    if (inter_vals){
      inter_df[as.character(subj_df$id), "Step_Preprocessing_H_Inches_Rounded"] <-
        subj_df$measurement
      # no result yet, so just make all these include
      inter_df[as.character(subj_df$id), "Step_Preprocessing_Result"] <-  F
    }

    # 1h, H BIV ----
    # 1h. remove biologically implausible height records
    step <- "1h, H BIV"
    
    criteria <- remove_biv(subj_df, "height", biv_df)
    subj_keep[criteria] <- "Implausible"
    subj_reason[criteria] <- paste0("Missing, Step ",step)
    
    # if using intermediate values, we want to keep some
    if(inter_vals){
      inter_df[as.character(subj_df$id), "Step_1h_H_Inches_Rounded"] <-
        subj_df$measurement
      inter_df[as.character(subj_df$id), "Step_1h_H_BIV_Low_Compare"] <- 
        remove_biv_low(subj_df, "height", biv_df)
      inter_df[as.character(subj_df$id), "Step_1h_H_BIV_High_Compare"] <- 
        remove_biv_high(subj_df, "height", biv_df)
      inter_df[as.character(subj_df$id), "Step_1h_Result"] <- criteria
    }
    
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
    
    # if using intermediate values, we want to keep some
    if (inter_vals){
      inter_df[as.character(subj_df$id), "Step_Preprocessing_W_Pounds_Rounded"] <-
        subj_df$measurement
      # no result yet, so just make all these include
      inter_df[as.character(subj_df$id), "Step_Preprocessing_Result"] <- F
    }
    
    # 1w, W BIV ----
    # 1w. remove biologically impossible weight records
    step <- "1w, W BIV"
    
    criteria <- remove_biv(subj_df, "weight", biv_df)
    subj_keep[criteria] <- "Implausible"
    subj_reason[criteria] <- paste0("Missing, Step ",step)
    
    # if using intermediate values, we want to keep some
    if(inter_vals){
      inter_df[as.character(subj_df$id), "Step_1w_W_Pounds_Rounded"] <-
        subj_df$measurement
      inter_df[as.character(subj_df$id), "Step_1w_W_BIV_Low_Compare"] <- 
        remove_biv_low(subj_df, "weight", biv_df)
      inter_df[as.character(subj_df$id), "Step_1w_W_BIV_High_Compare"] <- 
        remove_biv_high(subj_df, "weight", biv_df)
      inter_df[as.character(subj_df$id), "Step_1w_Result"] <- criteria
    }
    
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
      
      # if using intermediate values, we want to keep some
      if(inter_vals){
        inter_df[as.character(subj_df$id), "Step_2w_W_Pounds_Rounded"] <-
          subj_df$measurement
        inter_df[as.character(subj_df$id)[2:(nrow(subj_df)-1)],
                 "Step_2w_Ratio_1"] <- ratio_1
        inter_df[as.character(subj_df$id)[2:(nrow(subj_df)-1)],
                 "Step_2w_Ratio_1_Indicator"] <- rind_1
        inter_df[as.character(subj_df$id)[2:(nrow(subj_df)-1)],
                 "Step_2w_Ratio_2"] <- ratio_2
        inter_df[as.character(subj_df$id)[2:(nrow(subj_df)-1)],
                 "Step_2w_Ratio_2_Indicator"] <- rind_2
        # end values are automatically included
        inter_df[as.character(subj_df$id), "Step_2w_Result"] <- 
          c(F, criteria, F)
      }
    }
    
    # add results to full dataframe
    df[names(subj_keep), "result"] <- subj_keep
    df[names(subj_reason), "reason"] <- subj_reason
    
    # if we're using intermediate values, we want to save them
    if (inter_vals){
      df[as.character(inter_df$id), colnames(inter_df)[-1]] <- inter_df[,-1]
    }
  }
  
  return(df)
}