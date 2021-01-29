# Implementing Muthalagu, et al. (2014)
# By Hannah De los Santos
# Originated on: 9/25/2020

# paper: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3974252/

# Note: will have to update to data table upon completion for speed

# implement muthalagu, et al. ----

# function to clean height data by muthalagu, et al.
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
#       or is implausible. "unknown" for weight measurements.
#     reason, which specifies, for implausible values, the reason for exclusion,
#       and the step at which exclusion occurred.
muthalagu_clean_ht <- function(df, inter_vals = F){
  # method specific constants ----
  # this includes specified cutoffs, etc.
  
  # BIVs
  ht_cutoff_low <- 100
  ht_cutoff_high <- 250
  
  # BIVs
  biv_df <- data.frame(
    "low" = c(100),
    "high" = c(250)
  )
  rownames(biv_df) <- c("height")
  
  # in a given range, the cutoff between the max and min height
  # NOTE: this should be customizable
  btwn_range_cutoff <- 3.5
  
  # age buckets
  age_ranges <- data.frame(
    "low" = c(18, 25, 50),
    "high" = c(25, 50, Inf)
  )
  
  # intermediate value columns ... so many!
  inter_cols <- c(
    "Step_1h_H_BIV_Low_Compare",
    "Step_1h_H_BIV_High_Compare",
    "Step_1h_Result",
    "Step_2ha_Bucket_18-25_Height_Range",
    "Step_2ha_Bucket_18-25_Result",
    "Step_2hb_Bucket_18-25_Rounded_Age",
    "Step_2hb_Bucket_18-25_Median_Height",
    "Step_2hb_Bucket_18-25_Difference_from_Prior_Median",
    "Step_2hb_Bucket_18-25_Difference_from_Next_Median",
    "Step_2hb_Bucket_18-25_Flags",
    "Step_2hb_Bucket_18-25_Result",
    "Step_2hc_Bucket_18-25_Rounded_Age",
    "Step_2hc_Bucket_18-25_Corrected_Median",
    "Step_2hc_Bucket_18-25_Max_Difference_Between_Recorded_Heights_and_Median",
    "Step_2hc_Bucket_18-25_Result",
    "Step_2ha_Bucket_25-50_Height_Range",
    "Step_2ha_Bucket_25-50_Result",
    "Step_2hb_Bucket_25-50_Rounded_Age",
    "Step_2hb_Bucket_25-50_Median_Height",
    "Step_2hb_Bucket_25-50_Difference_from_Prior_Median",
    "Step_2hb_Bucket_25-50_Difference_from_Next_Median",
    "Step_2hb_Bucket_25-50_Flags",
    "Step_2hb_Bucket_25-50_Result",
    "Step_2hc_Bucket_25-50_Rounded_Age",
    "Step_2hc_Bucket_25-50_Corrected_Median",
    "Step_2hc_Bucket_25-50_Max_Difference_Between_Recorded_Heights_and_Median",
    "Step_2hc_Bucket_25-50_Result",
    "Step_2ha_Bucket_50-Inf_Height_Range",
    "Step_2ha_Bucket_50-Inf_Result",
    "Step_2hb_Bucket_50-Inf_Rounded_Age",
    "Step_2hb_Bucket_50-Inf_Median_Height",
    "Step_2hb_Bucket_50-Inf_Difference_from_Prior_Median",
    "Step_2hb_Bucket_50-Inf_Difference_from_Next_Median",
    "Step_2hb_Bucket_50-Inf_Flags",
    "Step_2hb_Bucket_50-Inf_Result",
    "Step_2hc_Bucket_50-Inf_Rounded_Age",
    "Step_2hc_Bucket_50-Inf_Corrected_Median",
    "Step_2hc_Bucket_50-Inf_Max_Difference_Between_Recorded_Heights_and_Median",
    "Step_2hc_Bucket_50-Inf_Result"
  )
  
  # begin implementation ----
  
  # if using intermediate values, preallocate values
  if (inter_vals){
    df[, inter_cols] <- NA
    rownames(df) <- df$id
  }
  
  h_df <- df[df$param == "HEIGHTCM",]
  h_df <- h_df[order(h_df$subjid),]
  
  # preallocate final designation
  out <- c()
  out_reason <- c()
  # go through each subject
  for (i in unique(h_df$subjid)){
    # since we're going to do this a fair bit
    slog <- h_df$subjid == i
    
    # if using intermediate values, we want to start storing them
    # keep the ID to collate with the final dataframe
    inter_df <- h_df[slog, "id", drop = F]
    rownames(inter_df) <- inter_df$id
    
    subj_keep <- rep("Include", sum(slog))
    subj_reason <- rep("", sum(slog))
    names(subj_keep) <- names(subj_reason) <- h_df$id[slog]
    
    subj_df <- h_df[slog,]
    
    # 1, H BIV ----
    # 1. remove biologically impossible height records
    step <- "1, H BIV"
    
    criteria <- remove_biv(subj_df, "height", biv_df)
    subj_keep[criteria] <- "Implausible"
    subj_reason[criteria] <- paste0("Erroneous, Step ",step)
    
    # if using intermediate values, we want to keep some
    if(inter_vals){
      inter_df[as.character(subj_df$id), "Step_1h_H_BIV_Low_Compare"] <- 
        remove_biv_low(subj_df, "height", biv_df)
      inter_df[as.character(subj_df$id), "Step_1h_H_BIV_High_Compare"] <- 
        remove_biv_high(subj_df, "height", biv_df)
      inter_df[as.character(subj_df$id), "Step_1h_Result"] <- criteria
    }
    
    subj_df <- subj_df[!criteria,]
    
    # 2 ----
    # 2. Go through each age bucket
    for (ab in 1:nrow(age_ranges)){
      # 2a, H age range check ----
      # 2a: if max - min height < 3.5, plausible
      step <- "2a, H age range check"
      
      subj_df_age <- subj_df[subj_df$age_years >= age_ranges$low[ab] &
                               subj_df$age_years < age_ranges$high[ab],]
      
      # if there's nothing in this age range, there's nothing to consider
      # NOTE: using next to avoid further nesting
      if (nrow(subj_df_age) == 0){
        next
      }
      
      age_low <- age_ranges$low[ab]
      age_high <- age_ranges$high[ab]
      
      # if using intermediate values, we want to keep some
      if(inter_vals){
        rnge <- abs(max(subj_df_age$measurement) - min(subj_df_age$measurement))
        inter_df[as.character(subj_df_age$id), 
                 paste0("Step_2ha_Bucket_", age_low, "-", age_high,
                       "_Height_Range")] <- 
          rnge
        inter_df[as.character(subj_df_age$id),  
                 paste0("Step_2ha_Bucket_", age_low, "-", age_high,
                       "_Result")] <- 
          rnge >= btwn_range_cutoff
        inter_df[as.character(subj_df_age$id),  
                 paste0("Step_2ha_Bucket_", age_low, "-", age_high,
                       "_Result")][rnge >= btwn_range_cutoff] <- "Unknown"
      }
      
      if (abs(max(subj_df_age$measurement) - min(subj_df_age$measurement)) >= 
          btwn_range_cutoff){
        # 2b, H median check ----
        # 2b: if not in range, calculate median height at each age. compare with 
        # prior and next median. if height at current age differs by > 3.5 prior
        # and next median, flag as potentially erroneous
        # if only 2 valid medians and differ by >3.5, flag both as indeterminate
        step <- "2b, H median check"
        
        # if there are only two values, everything will be indeterminate
        if (nrow(subj_df_age) <= 2){
          subj_keep[as.character(subj_df_age$id)] <- "Implausible"
          subj_reason[as.character(subj_df_age$id)] <- 
            paste0("Indeterminate, Step ", step)
          
          # if using intermediate values, we want to keep some
          if(inter_vals){
            inter_df[as.character(subj_df_age$id),  
                     paste0("Step_2hb_Bucket_", age_low, "-", age_high,
                           "_Result")] <- 
              T
          }
        } else {
          # calculate median heights per age (rounded to nearest year) in bucket
          # note: for duplicate rounded ages, this will propagate the same median to
          # both records
          med_hts <- sapply(round(subj_df_age$age_years), function(x){
            median(subj_df_age$measurement[round(subj_df_age$age_years) == x])
          })
          
          # compare only for middle values -- end values are indeterminate
          # results in true if both are under the cutoff
          mid_compare <- sapply(2:(length(med_hts)-1), function(x){
            all(c(abs(med_hts[x] - med_hts[x-1]), abs(med_hts[x] - med_hts[x+1])) <= 
                   btwn_range_cutoff)
          })
          
          # if true, correct height, else, erroneous
          # also include indeterminate (first and last)
          err_hts_idx <- c(1, 
                           if(!all(mid_compare)){which(!mid_compare)+1}, 
                           length(med_hts))
          
          # if using intermediate values, we want to keep some
          if(inter_vals){
            mid_before <- sapply(2:(length(med_hts)-1), function(x){
              abs(med_hts[x] - med_hts[x-1])
            })
            mid_next <- sapply(2:(length(med_hts)-1), function(x){
              abs(med_hts[x] - med_hts[x+1])
            })
            
            inter_df[as.character(subj_df_age$id),  
                     paste0("Step_2hb_Bucket_", age_low, "-", age_high,
                           "_Rounded_Age")] <- 
              round(subj_df_age$age_years)
            inter_df[as.character(subj_df_age$id),  
                     paste0("Step_2hb_Bucket_", age_low, "-", age_high,
                           "_Median_Height")] <- 
              med_hts
            inter_df[as.character(subj_df_age$id),  
                     paste0("Step_2hb_Bucket_", age_low, "-", age_high,
                           "_Difference_from_Prior_Median")] <- 
              c(NA, mid_before, NA)
            inter_df[as.character(subj_df_age$id),  
                     paste0("Step_2hb_Bucket_", age_low, "-", age_high,
                           "_Difference_from_Next_Median")] <- 
              c(NA, mid_next, NA)
            inter_df[as.character(subj_df_age$id)[err_hts_idx],  
                     paste0("Step_2hb_Bucket_", age_low, "-", age_high,
                           "_Flags")] <- 
              "Erroneous"
            inter_df[as.character(subj_df_age$id)[c(1,length(med_hts))],  
                     paste0("Step_2hb_Bucket_", age_low, "-", age_high,
                           "_Flags")] <- 
              "Indeterminate"
            inter_df[as.character(subj_df_age$id),  
                     paste0("Step_2hb_Bucket_", age_low, "-", age_high,
                           "_Result")] <- 
              c(F, !mid_compare, F)
            inter_df[as.character(subj_df_age$id)[!c(F, mid_compare, F)],  
                     paste0("Step_2hb_Bucket_", age_low, "-", age_high,
                           "_Result")] <- 
              "Unknown"
          }
          
          # 2c, H erroneous and indeterminate median check ----
          # 2c: For erroneous and indeterminate medians, assign correct medians within
          # 3 year period. Then compare all other recorded heights to the median at 
          # that age. If the recorded height for any age differs  > 3.5 (for 
          # erroneous) or > 6 (for indeterminate) from cleaned median height for 
          # that age, the value is erroneous.
          step <- "2c, H erroneous and indeterminate median check"
          
          # if there's no correct median height for comparison, it's all indeterminate
          if (sum(mid_compare) == 0){
            subj_keep[as.character(subj_df_age$id[err_hts_idx])] <- "Implausible"
            subj_reason[as.character(subj_df_age$id[err_hts_idx])] <- 
              paste0("Indeterminate, Step ", step)
            
            # if using intermediate values, we want to keep some
            if(inter_vals){
              inter_df[as.character(subj_df_age$id[err_hts_idx]),  
                       paste0("Step_2hc_Bucket_", age_low, "-", age_high,
                             "_Result")] <- 
                T
            }
          } else {
            if(inter_vals){
              inter_df[
                as.character(subj_df_age$id[
                  !(c(1:length(subj_df_age$id)) %in% err_hts_idx)]),  
                paste0("Step_2hc_Bucket_", age_low, "-", age_high,
                      "_Result")] <- 
                F
            }
            
            chg_err_med <- c() #sapply(err_hts_idx, function(x){
            for (x in err_hts_idx){
              # get all the correct medians within a 3 year window
              sub_df <- subj_df_age[-err_hts_idx,] # do not incorrect ages
              avail_med <- 
                med_hts[-err_hts_idx][
                  sub_df$age_years <= (subj_df_age$age_years[x]+1.5) |
                    sub_df$age_years >= (subj_df_age$age_years[x]-1.5)]
              
              # nothing to compare to within 3 years: indeterminate
              if (length(avail_med) == 0){
                return("Indeterminate")
              }
              
              # assign nearest correct median
              new_med <- avail_med[which.min(abs(avail_med - med_hts[x]))]
              
              # compare cleaned median to all recorded heights
              compare <- abs(subj_df_age$measurement[-x]-new_med)
              
              # for end points, they have a wider range
              ret_val <-
                if (x == 1 | x == length(med_hts)){
                  if(any(compare > 6)){
                    "Erroneous"
                  } else {
                    "Include"
                  }
                } else {
                  if(any(compare >= btwn_range_cutoff)){
                    "Erroneous"
                  } else {
                    "Include"
                  }
                }
              
              # if using intermediate values, we want to keep some
              if (inter_vals){
                res_map <- c(
                  "Erroneous" = T,
                  "Include"= F
                )
                
                inter_df[as.character(subj_df_age$id)[x],  
                         paste0("Step_2hc_Bucket_", age_low, "-", age_high,
                                "_Rounded_Age")] <- 
                  round(subj_df_age$age_years)[x]
                inter_df[as.character(subj_df_age$id)[x],  
                         paste0("Step_2hc_Bucket_", age_low, "-", age_high,
                                "_Corrected_Median")] <- 
                  new_med
                inter_df[
                  as.character(subj_df_age$id)[x],  
                  paste0("Step_2hc_Bucket_", age_low, "-", age_high,
                         "_Max_Difference_Between_Recorded_Heights_and_Median")] <- 
                  max(compare)
                inter_df[as.character(subj_df_age$id)[x],  
                         paste0("Step_2hc_Bucket_", age_low, "-", age_high,
                                "_Result")] <- 
                  res_map[ret_val]
              }
              
              chg_err_med <- c(chg_err_med, ret_val)
            }
            
            # adjust the errored/indeterminate heights accordingly
            # make the names consistent across different methods
            err_consistent <- chg_err_med
            err_consistent[err_consistent != "Include"] <- "Implausible"
            subj_keep[as.character(subj_df_age$id[err_hts_idx])] <- err_consistent
            subj_reason[as.character(subj_df_age$id[err_hts_idx]
                                     [chg_err_med != "Include"])] <- 
              paste0(chg_err_med[chg_err_med != "Include"], ", Step ", step)
          }
        }
      }
    }
    
    # record results
    out <- c(out, subj_keep)
    out_reason <- c(out_reason, subj_reason)
    
    # if we're using intermediate values, we want to save them
    if (inter_vals){
      df[as.character(inter_df$id), colnames(inter_df)[-1]] <- inter_df[,-1]
    }
  }
  
  # add results to overall data frame
  h_df <- cbind(h_df, "result" = out, "reason" = out_reason)
  
  # default to unknown for weight values
  df <- cbind(df, "result" = "Unknown", "reason" = "")
  rownames(df) <- df$id
  df[as.character(h_df$id), "result"] <- h_df$result
  df[as.character(h_df$id), "reason"] <- h_df$reason
  
  return(df)
}
