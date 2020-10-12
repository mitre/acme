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
# outputs:
#   df, with additional columns:
#     result, which specifies whether the height measurement should be included,
#       or is implausible.
#     reason, which specifies, for implausible values, the reason for exclusion,
#       and the step at which exclusion occurred.
cheng_clean_both <- function(df){
  # method specific constants ----
  # this includes specified cutoffs, etc.
  
  # BIVs
  biv_df <- data.frame(
    "low" = c(111.8, 24.9, 12),
    "high" = c(228.6, 453.6, 70)
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
    subj_reason[criteria] <- paste0("Implausible, Step ",step)
    
    subj_df <- subj_df[!criteria,]
    
    if (nrow(subj_df) > 0){
      # 2h, H compare difference from average to SD ----
      # 2h. Exclude height if a) absolute difference between that height and average
      # height > standard deviation (SD) AND b) SD > 2.5% of average height.
      step <- "2h, H compare difference from average to SD"
      
      # calculate standard deviation of height for a given subject
      st_dev <- sd(subj_df$measurement)
      avg <- mean(subj_df$measurement)
      
      # if the standard deviation is > than 2.5%, we can say something about height
      if (st_dev/avg > .025){
        # criteria (a)
        excl_ht <- sapply(subj_df$measurement, function(x){abs(x-avg) > st_dev})
        
        subj_keep[as.character(subj_df$id[excl_ht])] <- "Implausible"
        subj_reason[as.character(subj_df$id[excl_ht])] <- 
          paste0("Implausible, Step ",step)
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
    
    subj_df <- subj_df[!criteria,]
    
    if (nrow(subj_df) > 0){
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
    }
    
    w_df$result <- subj_keep
    w_df$reason <- subj_reason
    
    # add results to full dataframe
    df[as.character(w_df$id), "result"] <- w_df$result
    df[as.character(w_df$id), "reason"] <- w_df$reason
    
    # 3, BMI BIV ----
    # 3. If BMI for a given set of height/weights is < 12 or > 70, deem implausible
    step <- "3, BMI BIV"
    
    # possible removal of height/weights by bmi
    # x = height, y = weight
    comb_df <- merge(h_df, w_df, by = "age_years", all = T)
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
    }
  }
  
  return(df)
}
