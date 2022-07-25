# Implementing Cheng, et al. (2016)
# By Hannah De los Santos
# Originated on: 10/1/2020

# paper: https://onlinelibrary.wiley.com/doi/full/10.1002/oby.21612

# Note: will have to update to data table upon completion for speed
# Note 2: must remove missing values before running method
# Note 3: how to deal with duplicated recordings?

# implement cheng, et al. ----

# function to clean height and weight data by cheng, et al.
# inputs:
# df: data frame with 7 columns:
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
cheng_clean_both <- function(df, inter_vals = F){
  # method specific constants ----
  # this includes specified cutoffs, etc.
  
  # BIVs
  biv_df <- data.frame(
    "low" = c(111.8, 24.9, 12),
    "high" = c(228.6, 453.6, 70)
  )
  rownames(biv_df) <- c("height", "weight", "bmi")
  
  # intermediate value columns
  inter_cols <- c(
    "Step_1h_H_BIV_Low_Compare",
    "Step_1h_H_BIV_High_Compare",
    "Step_1h_Result",
    "Step_2h_Difference_from_Mean",
    "Step_2h_SD",
    "Step_2h_.025_of_Mean",
    "Step_2h_Result",
    "Step_1w_W_BIV_Low_Compare",
    "Step_1w_W_BIV_High_Compare",
    "Step_1w_Result",
    "Step_2w_Range",
    "Step_2w_Difference_from_Mean",
    "Step_2w_SD",
    "Step_2w_.2_of_Mean",
    "Step_2w_Result",
    "Step_3_BMI",
    "Step_3_BMI_BIV_Low_Compare",
    "Step_3_BMI_BIV_High_Compare",
    "Step_3_Result"
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
    
    if (nrow(subj_df) > 1){
      # 2h, H compare difference from average to SD ----
      # 2h. Exclude height if a) absolute difference between that height and average
      # height > standard deviation (SD) AND b) SD > 2.5% of average height.
      step <- "2h, H compare difference from average to SD"
      
      # calculate standard deviation of height for a given subject
      st_dev <- sd(subj_df$measurement)
      avg <- mean(subj_df$measurement)
      
      # if the standard deviation is > than 2.5%, we can say something about height
      excl_ht <- rep(F, nrow(subj_df))
      if (st_dev/avg > .025){
        # criteria (a)
        excl_ht <- sapply(subj_df$measurement, function(x){abs(x-avg) > st_dev})
        
        subj_keep[as.character(subj_df$id[excl_ht])] <- "Implausible"
        subj_reason[as.character(subj_df$id[excl_ht])] <- 
          paste0("Implausible, Step ",step)
      }
      
      # if using intermediate values, we want to keep some
      if(inter_vals){
        inter_df[as.character(subj_df$id), "Step_2h_Difference_from_Mean"] <- 
          abs(subj_df$measurement - avg)
        inter_df[as.character(subj_df$id), "Step_2h_SD"] <- 
          st_dev
        inter_df[as.character(subj_df$id), "Step_2h_.025_of_Mean"] <- 
          .025*avg
        inter_df[as.character(subj_df$id), "Step_2h_Result"] <- excl_ht
      }
    }
    
    h_df$result <- subj_keep
    h_df$reason <- subj_reason
    
    # add results to full dataframe
    df[as.character(h_df$id), "result"] <- h_df$result
    df[as.character(h_df$id), "reason"] <- h_df$reason
    
    # then do weight ----
    
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
    
    if (nrow(subj_df) > 1){
      # 2w, W compare difference from average to range or SD ----
      # 2w. weight inaccurate if:
      # a) the range was > 22.7 kg AND absolute difference between recorded weight 
      # and avg weight was > 70% of range
      # OR
      # b) SD was >20% of the average weight AND absolute difference between that 
      # weight and average weight > the SD
      step <- "2w, W compare difference from average to range or SD"
      
      avg_w <- mean(subj_df$measurement)
      
      # first calculate criteria a)
      w_range <- abs(max(subj_df$measurement) - min(subj_df$measurement))
      # if range is > 22.7, we can possibly say something about weight
      criteria_a <- 
        if (w_range > 22.7){
          # weight difference is more than 70% of range
          sapply(subj_df$measurement, function(x){
            abs(x - avg_w) > .7*w_range
          })
        } else {
          rep(F, nrow(subj_df))
        }
      
      # criteria b)
      st_dev <- sd(subj_df$measurement)
      # if sd was >20% of avg weight, we can possibly say something about weight
      criteria_b <- 
        if (st_dev/avg_w > .2){
          # weight difference is > SD
          sapply(subj_df$measurement, function(x){
            abs(x - avg_w) > st_dev
          })
        } else {
          rep(F, nrow(subj_df))
        }
      
      subj_keep[as.character(subj_df$id[criteria_a | criteria_b])] <- "Implausible"
      subj_reason[as.character(subj_df$id[criteria_a | criteria_b])] <- 
        paste0("Implausible, Step ",step)
      
      # if using intermediate values, we want to keep some
      if(inter_vals){
        inter_df[as.character(subj_df$id), "Step_2w_Range"] <- 
          w_range
        inter_df[as.character(subj_df$id), "Step_2w_Difference_from_Mean"] <- 
          abs(subj_df$measurement - avg_w)
        inter_df[as.character(subj_df$id), "Step_2w_SD"] <- 
          st_dev
        inter_df[as.character(subj_df$id), "Step_2w_.2_of_Mean"] <- 
          .2*avg_w
        inter_df[as.character(subj_df$id), "Step_2w_Result"] <- criteria_a | criteria_b
      }
    }
    
    w_df$result <- subj_keep
    w_df$reason <- subj_reason
    
    # add results to full dataframe
    df[as.character(w_df$id), "result"] <- w_df$result
    df[as.character(w_df$id), "reason"] <- w_df$reason
    
    # 3, BMI BIV ----
    # 3. If BMI for a given set of height/weights is < 12 or > 70, deem implausible
    step <- "3, BMI BIV"
    
    # if intermediate values, remove the appended columns
    if (inter_vals){
      h_df <- h_df[, !colnames(h_df) %in% inter_cols]
      w_df <- w_df[, !colnames(w_df) %in% inter_cols]
    }
    
    # possible removal of height/weights by bmi
    # x = height, y = weight
    comb_df <- comb_df_orig <- merge(h_df, w_df, by = "age_years", all = T)
    # remove ones that don't match
    comb_df <- comb_df[complete.cases(comb_df),]
    # also remove ones that are not plausible
    comb_df <- comb_df[comb_df$result.x == "Include" & comb_df$result.y == "Include",]
    
    if (nrow(comb_df) > 0){
      # calculate bmi
      comb_df$measurement <- comb_df$measurement.y/((comb_df$measurement.x/100)^2)
      
      bmi_biv <- remove_biv(comb_df, "bmi", biv_df)
      
      # add result to interim df
      comb_df$tot_res <- "Include"
      comb_df$tot_res[bmi_biv] <- "Implausible"
      comb_df$tot_reason <- ""
      comb_df$tot_reason[bmi_biv] <- paste0("Implausible, Step ",step)
      
      # add result to full dataframe
      df[as.character(comb_df$id.y), "result"] <- 
        df[as.character(comb_df$id.x), "result"] <- comb_df$tot_res
      df[as.character(comb_df$id.y), "reason"] <- 
        df[as.character(comb_df$id.x), "reason"] <- comb_df$tot_reason
      
      if(inter_vals){
        inter_df[as.character(comb_df$id.y), "Step_3_BMI"] <- 
          inter_df[as.character(comb_df$id.x), "Step_3_BMI"] <- 
          comb_df$measurement
        inter_df[as.character(comb_df$id.y), "Step_3_BMI_BIV_Low_Compare"] <- 
          inter_df[as.character(comb_df$id.x), "Step_3_BMI_BIV_Low_Compare"] <- 
          remove_biv_low(comb_df, "bmi", biv_df)
        inter_df[as.character(comb_df$id.y), "Step_3_BMI_BIV_High_Compare"] <- 
          inter_df[as.character(comb_df$id.x), "Step_3_BMI_BIV_High_Compare"] <- 
          remove_biv_high(comb_df, "bmi", biv_df)
        inter_df[as.character(comb_df$id.y), "Step_3_Result"] <- 
          inter_df[as.character(comb_df$id.x), "Step_3_Result"] <- 
          bmi_biv
      }
    }
    
    if (inter_vals){
      # we also want to add a "not calculated" for ones that didn't have a 
      # corresponding height/weight
      comb_df_orig <- comb_df_orig[
        !(comb_df_orig$result.x == "Include" & comb_df_orig$result.y == "Include") |
          !complete.cases(comb_df_orig),]
      # remove any that were both already implausible
      comb_df_orig <- comb_df_orig[
        (comb_df_orig$result.x != "Implausible" | 
           comb_df_orig$result.y != "Implausible") %in% T,
      ]
      ind <- as.character(c(
        comb_df_orig$id.y[comb_df_orig$result.y == "Include"], 
        comb_df_orig$id.x[comb_df_orig$result.x == "Include"]
      ))
      ind <- ind[!is.na(ind)]
      inter_df[ind, "Step_3_Result"] <- "Not Calculated"
    }
    
    # if we're using intermediate values, we want to save them
    if (inter_vals){
      df[as.character(inter_df$id), colnames(inter_df)[-1]] <- inter_df[,-1]
    }
  }
  
  return(df)
}
