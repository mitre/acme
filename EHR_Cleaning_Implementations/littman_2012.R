# Implementing Littman, et al. (2012)
# By Hannah De los Santos
# Originated on: 10/21/2020

# paper: https://www.cdc.gov/pcd/issues/2012/11_0267.htm

# supporting functions for this implementation only ----

# function to calculate the criteria to remove measurement based on difference
# of record from mean compared to SD, provided that the SD is large enough
remove_diff_from_sd <- function(subj_df, max_sd_of_mean){
  avg <- mean(subj_df$measurement)
  st_dev <- sd(subj_df$measurement)
  
  # if the SD is greater than specified fraction of mean, we can evaluate record
  if (st_dev/avg > max_sd_of_mean){
    # criteria to exclude the record
    criteria <- sapply(subj_df$measurement, function(x){
      abs(x-avg) > st_dev
    })
  } else {
    criteria <- rep(F, nrow(subj_df))
  }
  
  return(criteria)
}

# implement littman, et al. ----

# function to clean height and weight data by littman, et al.
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
littman_clean_both <- function(df){
  # method specific constants ----
  # this includes specified cutoffs, etc.
  
  # BIVs
  biv_df <- data.frame(
    "low" = c(49*2.54, 75*.453592, 0),
    "high" = c(94*2.54, 600*.453592, 80)
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
    
    # start by removing BIVs ----
    
    # get weight and height data
    h_df <- df[df$param == "HEIGHTCM" & slog,]
    w_df <- df[df$param == "WEIGHTKG" & slog,]
    
    h_subj_keep <- rep("Include", nrow(h_df))
    h_subj_reason <- rep("", nrow(h_df))
    names(h_subj_keep) <- names(h_subj_reason) <- h_df$id
    
    h_subj_df <- h_df
    rownames(h_subj_df) <- h_subj_df$id
    
    w_subj_keep <- rep("Include", nrow(w_df))
    w_subj_reason <- rep("", nrow(w_df))
    names(w_subj_keep) <- names(w_subj_reason) <- w_df$id
    
    w_subj_df <- w_df
    rownames(w_subj_df) <- w_subj_df$id
    
    # 1h, H BIV ----
    # 1h. remove biologically impossible height records
    step <- "1h, H BIV"
    
    criteria <- remove_biv(h_subj_df, "height", biv_df)
    h_subj_keep[criteria] <- "Implausible"
    h_subj_reason[criteria] <- paste0("Erroneous, Step ",step)
    
    h_subj_df <- h_subj_df[!criteria,]
    
    # 1w, W BIV ----
    # 1w. remove biologically impossible weight records based on 2 possibilities:
    # a) [weight <75 lbs  or >600 lbs] OR
    # b) [weight change per week > 2 lbs and > 50 lbs overall] OR [weight change > 
    # 100 lbs, no matter the rate]
    step <- "1w, W BIV" # NOTE: reason is recorded differently below
    
    # criteria a) traditional weight biv removal
    criteria_a <- remove_biv(w_subj_df, "weight", biv_df)
    
    # criteria b) rate change biv removal
    # preallocate
    criteria_b <- rep(F, nrow(w_subj_df))
    
    # need at least 2 values to compute rate change
    if (nrow(w_subj_df) >= 2){
      # compute overall weight change
      w_ch_overall <- 
        abs(diff(c(min(w_subj_df$measurement), max(w_subj_df$measurement))))
      
      # convert years to weeks
      w_subj_df <- w_subj_df[order(w_subj_df$age_years),]
      age_weeks <- w_subj_df$age_years*52.1429
      # get lagged time change
      time_ch <- sapply(2:nrow(w_subj_df), function(x){
        age_weeks[x]-age_weeks[x-1]
      })
      # get lagged weight change
      w_ch <- sapply(2:nrow(w_subj_df), function(x){
        abs(w_subj_df$measurement[x]-w_subj_df$measurement[x-1])
      })
      # note that if weight changes at all for no time difference, it is 
      # implausible (Inf)
      w_ch_per_week <- w_ch/time_ch
      # compensate for 0/0, which is plausible
      w_ch_per_week[is.na(w_ch_per_week)] <- 0 
      
      # [weight change per week > 2 lbs and > 50 lbs overall]
      invalid_1 <- which(
        w_ch_per_week > 2*.453592 & w_ch_overall > 50*.453592
      )
      # [weight change > 100 lbs, no matter the rate]
      invalid_2 <- which(w_ch_per_week > 100*.453592)
      
      # both around those changes should be implausible
      if (length(invalid_1) > 0){
        criteria_b[c(invalid_1, invalid_1+1)] <- T
      }
      if (length(invalid_2) > 0){
        criteria_b[c(invalid_2, invalid_2+1)] <- T
      }
    }
    
    w_subj_keep[criteria_a | criteria_b] <- "Implausible"
    w_subj_reason[criteria_a] <- 
      paste0("Erroneous, Step ", "1wa, W BIV cutoffs")
    w_subj_reason[criteria_b] <- 
      paste0("Erroneous, Step ", "1wb, W BIV rate change")
    
    w_subj_df <- w_subj_df[!(criteria_a | criteria_b),]
    
    # 1bmi, BMI BIV ----
    # 1bmi. remove biologically implausible bmi values for each set of height
    # /weights
    step <- "1bmi, BMI BIV"
    
    # possible removal of height/weights by bmi
    # x = height, y = weight
    comb_df <- merge(h_subj_df, w_subj_df, by = "age_years", all = T)
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
      comb_df$tot_reason[bmi_biv] <- paste0("Erroneous, Step ",step)
      
      # add result to height and weight data
      w_subj_keep[as.character(comb_df$id.y)] <- 
        h_subj_keep[as.character(comb_df$id.x)] <- comb_df$tot_res
      w_subj_reason[as.character(comb_df$id.y)] <- 
        h_subj_reason[as.character(comb_df$id.x)] <- comb_df$tot_reason
      
      # subset height and weight data
      w_subj_df[unique(as.character(comb_df$id.y)),] <- 
        w_subj_df[unique(as.character(comb_df$id.y)),][
          comb_df$tot_res[!duplicated(comb_df$id.y)] == "Include",]
      h_subj_df[unique(as.character(comb_df$id.x)),] <- 
        h_subj_df[unique(as.character(comb_df$id.x)),][
          comb_df$tot_res[!duplicated(comb_df$id.x)] == "Include",]
    }
    
    # 2, remove erroneous values based on SD ----
    # 2. Remove erroneous values that have large standard deviations.
    
    # 2w, W compare difference from average to SD ----
    # 2w. Exclude any weight measurements where: 1) difference between mean
    # weight and recorded weight was greater than the standard deviation (SD) 
    # AND 2) the SD was greater than 10% of the mean.
    step <- "2w, W compare difference from average to SD"
    
    # to compute standard deviation, you need at least 2 plausible values
    if (nrow(w_subj_df) > 1){
      criteria <- remove_diff_from_sd(w_subj_df, .1)
      
      w_subj_keep[as.character(w_subj_df$id[criteria])] <- "Implausible"
      w_subj_reason[as.character(w_subj_df$id[criteria])] <- paste0("Erroneous, Step ", step)
      # do not need to update the weight df, since it's not used again
    }
    
    # 2h, H compare difference from average to SD ----
    # 2h. Exclude any height measurements where: 1) difference between 
    # mean height and recorded height was greater than SD AND 2) SD was greater
    # than 2.5% of mean.
    step <- "2h, H compare difference from average to SD"
    
    # keep for the next step
    orig_h_subj_df <- h_subj_df
    
    # to compute standard deviation, you need at least 2 plausible values
    if (nrow(h_subj_df) > 1){
      criteria <- remove_diff_from_sd(h_subj_df, .025)
      
      h_subj_keep[as.character(h_subj_df$id[criteria])] <- "Implausible"
      h_subj_reason[as.character(h_subj_df$id[criteria])] <- paste0("Erroneous, Step ", step)
      # do not need to update the height df, since it's not used again
    }
    
    # 3h, H compare difference to SD, with most deviant height dropped ----
    # 3h. Run step 2h again, but with the most deviant height dropped to see if 
    # there are any more implausible values.
    step <- "3h, H compare difference to SD, with most deviant height dropped"
    
    # to compute standard deviation, you need at least 2 plausible values (one will
    # be dropped, so you need 3)
    if (nrow(orig_h_subj_df) > 2){
      # find most deviant height
      avg_h <- mean(orig_h_subj_df$measurement)
      most_dev <- which.max(
        sapply(orig_h_subj_df$measurement, function(x){abs(x-avg_h)})
      )
      # remove most deviant height and rerun the algorithm
      orig_h_subj_df <- orig_h_subj_df[-most_dev,]
      criteria <- remove_diff_from_sd(orig_h_subj_df, .025)
      
      h_subj_keep[as.character(orig_h_subj_df$id[criteria])] <- "Implausible"
      h_subj_reason[as.character(orig_h_subj_df$id[criteria])] <- 
        paste0("Erroneous, Step ", step)
      # do not need to update the height df, since it's not used again
    }
    
    # add results to full dataframe ----
    
    df[as.character(names(w_subj_keep)), "result"] <- w_subj_keep
    df[as.character(names(w_subj_reason)), "reason"] <- w_subj_reason
    df[as.character(names(h_subj_keep)), "result"] <- h_subj_keep
    df[as.character(names(h_subj_reason)), "reason"] <- h_subj_reason
  }
  
  return(df)
}