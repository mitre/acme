# Implementing Daymont, et al. for Infants (growthcleanr-naive)
# By Hannah De los Santos
# Originated on: 9/22/22

# paper: https://academic.oup.com/jamia/article/24/6/1080/3767271

# This is more of a speculative implementation.
# Note: this could be more clean/optimized.

# supporting functions for this implementation only ----

# Function to remove data based on exponentially-weighted moving average 
# (Daymont, et al.).
# inputs:
# subj_df: subject data frame, which has age in days and z-score
# ewma_cutoff: EWMA past which considered invalid (center value). left and right
#   are .5 less.
# outputs:
#  logical indicating whether to exclude a record
remove_ewma <- function(subj_df, ewma_cutoff = 3, 
                        inter_vals = F, inter_df = data.frame(),
                        type = "h"){
  orig_subj_df <- subj_df
  
  # all three need to be beyond a cutoff for exclusion
  # exclude the most extreme, then recalculate again and again
  rem_ids <- c()
  change <- T
  iter <- 1
  while (change){
    # calculate ewma
    ewma_res <- growthcleanr::ewma(subj_df$age_days, subj_df$z,
                                   ewma.exp = -1.5)
    
    dewma <- abs(ewma_res-subj_df$z)
    colnames(dewma) <- paste0("d",colnames(ewma_res))
    
    criteria_new <- 
      (dewma$dewma.all > ewma_cutoff & 
         dewma$dewma.before > ewma_cutoff - .5 & 
         dewma$dewma.after > ewma_cutoff - .5 & 
         abs(subj_df$z) > ewma_cutoff)
    
    
    # if using intermediate values, we want to keep some
    if (inter_vals){
      inter_df[as.character(subj_df$id), 
               paste0("Step_1", type, "_Iter_", iter, "_Delta_EWMA_All")] <-
        dewma$dewma.all
      inter_df[as.character(subj_df$id), 
               paste0("Step_1", type, "_Iter_", iter, "_Delta_EWMA_Before")] <-
        dewma$dewma.before
      inter_df[as.character(subj_df$id), 
               paste0("Step_1", type, "_Iter_", iter, "_Delta_EWMA_After")] <-
        dewma$dewma.after
    }
    
    if (all(!criteria_new)){
      # if none of them are to be removed
      change <- F
      # if using intermediate values, we want to keep some
      if (inter_vals){
        inter_df[as.character(subj_df$id), 
                 paste0("Step_1", type, "_Iter_", iter, "_Result")] <-
          criteria_new
      }
    } else {
      # figure out the most extreme value and remove it and rerun
      to_rem <- which.max(abs(dewma$dewma.all)[criteria_new])
      # if using intermediate values, we want to keep some
      if (inter_vals){
        inter_df[as.character(subj_df$id), 
                 paste0("Step_1", type, "_Iter_", iter, "_Result")] <-
          F
        # update only the most extreme value to be removed
        inter_df[as.character(subj_df[criteria_new,][to_rem, "id"]), 
                 paste0("Step_1", type, "_Iter_", iter, "_Result")] <-
          T
      }
      
      # keep the ids that failed and remove
      rem_ids[length(rem_ids)+1] <- subj_df[criteria_new,][to_rem, "id"]
      subj_df <- subj_df[subj_df$id != rem_ids[length(rem_ids)],]
      # update iteration
      iter <- iter + 1
    }
  }
  
  # form results into a logical vector
  criteria <- rep(F, nrow(orig_subj_df))
  criteria[orig_subj_df$id %in% rem_ids] <- T
  
  return(list(
    "criteria" = criteria,
    "inter_df" = inter_df
  ))
}

# implement growthcleanr-naive ----

# function to clean height and weight data by Daymont, et al.
# inputs:
# df: data frame with 7 columns:
#   id: row id, must be unique
#   subjid: subject id
#   sex: sex of subject
#   age_days: age in days
#   param: HEIGHTCM or WEIGHTKG
#   measurement: height or weight measurement
# outputs:
#   df, with additional columns:
#     result, which specifies whether the height measurement should be included,
#       or is implausible.
#     reason, which specifies, for implausible values, the reason for exclusion,
#       and the step at which exclusion occurred.
growthcleanr_naive_clean_both <- function(df, inter_vals = F){
  
  # intermediate value columns -- only ones that are common among all
  inter_cols <- c(
    "Step_1h_Age_days",
    "Step_1h_Z-Score",
    "Step_1h_Result",
    "Step_1w_Age_days",
    "Step_1w_Z-Score",
    "Step_1w_Result"
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
  
  # in order not to add additional columns to output (unless called for),
  # we create a dataframe to use for computation
  all_df <- df
  
  # Create standardized measurement.
  all_df$z <- get_standardized_infant_scores(
    all_df$param, all_df$age_days, all_df$sex, all_df$measurement
  )
  
  # go through each subject
  for (i in unique(all_df$subjid)){
    slog <- all_df$subjid == i
    
    # if using intermediate values, we want to start storing them
    # keep the ID to collate with the final dataframe
    inter_df <- all_df[slog, "id", drop = F]
    rownames(inter_df) <- inter_df$id
    
    # start with height ----
    h_df <- all_df[all_df$param == "HEIGHTCM" & slog,]
    
    subj_keep <- rep("Include", nrow(h_df))
    subj_reason <- rep("", nrow(h_df))
    names(subj_keep) <- names(subj_reason) <- h_df$id
    
    subj_df <- h_df
    
    # 1h, H calculate ewma ----
    # 1h. Exclude extreme errors by calculating the exponentially weighted 
    # moving average and removing by a specified cutoff. If record(s) is/are 
    # found to be extreme, remove the most extreme one and recalculate. Repeat
    # until this no more values are found to be extreme.
    step <- "1h, H calculate ewma"
    
    # sort by age
    subj_df <- subj_df[order(subj_df$age_days),]
    
    # calculate z-scores (based on the single person)
    if (nrow(subj_df) >= 3 & sd(subj_df$measurement) > 0){
      subj_df$z <- (subj_df$measurement - mean(subj_df$measurement))/
        sd(subj_df$measurement)
      
      # if using intermediate values, we want to keep some
      if (inter_vals){
        inter_df[as.character(subj_df$id), "Step_1h_Z-Score"] <-
          subj_df$z
      }
      
      # calculate the criteria to remove the ewma
      criteria_list <- remove_ewma(subj_df, 
                                   inter_vals = inter_vals, inter_df = inter_df,
                                   type = "h")
      criteria <- criteria_list$criteria
      inter_df <- criteria_list$inter_df
      
      subj_keep[criteria] <- "Implausible"
      subj_reason[criteria] <- paste0("Implausible, Step ",step)
      
      # if using intermediate values, we want to keep some
      if (inter_vals){
        inter_df[as.character(subj_df$id), "Step_1h_Result"] <-
          criteria
      }
    }
    
    # add results to full dataframe
    all_df[names(subj_keep), "result"] <- subj_keep
    all_df[names(subj_reason), "reason"] <- subj_reason
    
    # then do weight ----
    
    w_df <- all_df[all_df$param == "WEIGHTKG" & slog,]
    
    subj_keep <- rep("Include", nrow(w_df))
    subj_reason <- rep("", nrow(w_df))
    names(subj_keep) <- names(subj_reason) <- w_df$id
    
    subj_df <- w_df
    
    # 1w, W calculate ewma ----
    # 1w. Exclude extreme errors by calculating the exponentially weighted 
    # moving average and removing by a specified cutoff. If record(s) is/are 
    # found to be extreme, remove the most extreme one and recalculate. Repeat
    # until this no more values are found to be extreme.
    step <- "1w, W calculate ewma"
    
    # convert age years to days
    subj_df$age_days <- subj_df$age_years*365.2425
    
    # if using intermediate values, we want to keep some
    if (inter_vals){
      inter_df[as.character(subj_df$id), "Step_1w_Age_days"] <-
        subj_df$age_days
    }
    
    # sort by age
    subj_df <- subj_df[order(subj_df$age_days),]
    
    # calculate z-scores (based on the single person)
    if (nrow(subj_df) >= 3 & sd(subj_df$measurement) > 0){
      subj_df$z <- (subj_df$measurement - mean(subj_df$measurement))/
        sd(subj_df$measurement)
      
      # if using intermediate values, we want to keep some
      if (inter_vals){
        inter_df[as.character(subj_df$id), "Step_1w_Z-Score"] <-
          subj_df$z
      }
      
      # calculate the criteria to remove the ewma
      criteria_list <- remove_ewma(subj_df, 
                                   inter_vals = inter_vals, inter_df = inter_df,
                                   type = "w")
      criteria <- criteria_list$criteria
      inter_df <- criteria_list$inter_df
      
      subj_keep[criteria] <- "Implausible"
      subj_reason[criteria] <- paste0("Implausible, Step ",step)
      
      # if using intermediate values, we want to keep some
      if (inter_vals){
        inter_df[as.character(subj_df$id), "Step_1w_Result"] <-
          criteria
      }
    }
    
    # add results to full dataframe
    all_df[names(subj_keep), "result"] <- subj_keep
    all_df[names(subj_reason), "reason"] <- subj_reason
    
    # if we're using intermediate values, we want to save them
    if (inter_vals){
      df[as.character(inter_df$id), colnames(inter_df)[-1]] <- inter_df[,-1]
    }
  }
  
  # add results to full dataframe
  df[all_df$id, "result"] <- all_df$result
  df[all_df$id, "reason"] <- all_df$reason
  
  return(df)
}
