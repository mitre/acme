################################################################################
# carsley_2018.R: Implementation of paper to compare data cleaning algorithms
# for infant data. Original paper is by Carsley et al. and identifies what they
# call "inaccurate measurements" in one of two ways. The first way is just
# measurements that have Z-scores outside a certain range (which differs
# depending on measurement type). The second is their "invalid inlier rule",
# which applies to those z-scores that are greater than or equal to 3 in
# absolute value, but not outliers (this is for BMI, they do not give potential
# cutoffs for height and weight). In this case, standardized versions of the
# z-scores are computed, and an observation is labeled as an "invalid inliers"
# depending upon its standardized distance and time apart from other
# observations.
# The original paper is available at:
# https://informatics.bmj.com/content/25/1/19
#
# @author = Max Olivier
################################################################################

# function to clean height and weight data by carsley, et al.
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
#     intermediate value columns, if specified

carsley_clean_both <- function(df, inter_vals = F){
  
  # method specific constants ----
  
  inter_cols <- c(
    "Step_1h_WHO_Z_for_Age",
    "Step_1h_H_BIV_Low_Compare", 
    "Step_1h_H_BIV_Low_Compare", 
    "Step_1h_Result", 
    "Step_1w_WHO_Z_for_Age",
    "Step_1w_W_BIV_Low_Compare", 
    "Step_1w_W_BIV_Low_Compare", 
    "Step_1w_Result", 
    "Step_2h_First_Age_Diff_and_Criteria",
    "Step_2h_First_SD_Diff_and_Criteria",
    "Step_2h_WHO_Z_for_Age",
    "Step_2h_Subject_SD",
    "Step_2h_Result",
    "Step_2w_First_Age_Diff_and_Criteria",
    "Step_2w_First_SD_Diff_and_Criteria",
    "Step_2w_WHO_Z_for_Age",
    "Step_2w_Subject_SD",
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
  
  # if using intermediate values, we want to start storing them
  # keep the ID to collate with the final dataframe
  inter_df <- df[, "id", drop = F]
  rownames(inter_df) <- inter_df$id
  
  # in order not to add additional columns to output (unless called for),
  # we create a dataframe to use for computation
  all_df <- df
  
  # preprocessing ----
  
  # Create standardized measurement. Assumes function is already loaded.
  all_df$stdz_meas <- get_standardized_infant_scores(
    all_df$param, all_df$age_days, all_df$sex, all_df$measurement
  )
  
  # Sort data by subject, param, and age.
  all_df <- all_df[order(all_df$subjid, all_df$param, all_df$age_days),]
  
  # Generate variables label outliers and "invalid inliers".
  all_df$outlier <- all_df$invalid_inlier <- NA
  
  # 1h, H BIV ----
  # 1h: Outliers are taken to be values where the z-score is outside of the 
  # interval [-6, 6] for height.
  step <- "1h, H BIV"
  
  ht_subset <- all_df$param=="HEIGHTCM" & !is.na(all_df$stdz_meas)
  all_df$outlier[ht_subset] <- abs(all_df$stdz_meas[ht_subset]) > 6
  
  all_df[ht_subset & all_df$outlier, "result"] <- "Implausible"
  all_df[ht_subset & all_df$outlier, "reason"] <- 
    paste0("Implausible, Step ", step)
  
  # if using intermediate values, we want to keep some
  if(inter_vals){
    inter_df[as.character(all_df$id[ht_subset]), "Step_1h_WHO_Z_for_Age"] <- 
      all_df$stdz_meas[ht_subset]
    inter_df[as.character(all_df$id[ht_subset]), "Step_1h_H_BIV_Low_Compare"] <- 
      all_df$stdz_meas[ht_subset] > -6
    inter_df[as.character(all_df$id[ht_subset]), "Step_1h_H_BIV_Low_Compare"] <- 
      all_df$stdz_meas[ht_subset] < 6
    inter_df[as.character(all_df$id[ht_subset]), "Step_1h_Result"] <- 
      all_df$outlier[ht_subset]
  }
  
  # 1w, W BIV ----
  # 1w: Outliers are taken to be values where the z-score is outside of the 
  # interval [-6, 5] for weight.
  step <- "1w, W BIV"
  
  wt_subset <- all_df$param=="WEIGHTKG" & !is.na(all_df$stdz_meas)
  all_df$outlier[wt_subset] <- all_df$stdz_meas[wt_subset] < -6 |
    all_df$stdz_meas[wt_subset] > 5
  
  all_df[wt_subset & all_df$outlier, "result"] <- "Implausible"
  all_df[wt_subset & all_df$outlier, "reason"] <- 
    paste0("Implausible, Step ", step)
  
  # if using intermediate values, we want to keep some
  if(inter_vals){
    inter_df[as.character(all_df$id[wt_subset]), "Step_1w_WHO_Z_for_Age"] <- 
      all_df$stdz_meas[wt_subset]
    inter_df[as.character(all_df$id[wt_subset]), "Step_1w_W_BIV_Low_Compare"] <- 
      all_df$stdz_meas[wt_subset] > -6
    inter_df[as.character(all_df$id[wt_subset]), "Step_1w_W_BIV_High_Compare"] <- 
      all_df$stdz_meas[wt_subset] < 5
    inter_df[as.character(all_df$id[wt_subset]), "Step_1w_Result"] <- 
      all_df$outlier[wt_subset]
  }
  
  # 2h/w, H/W invalid inliers ----
  # 2h/w: Invalid inliers" are observations that are not outliers, but have 
  # a certain SD score away from another observation within
  # a smaller time window. The value of the SD score and the time window depends
  # upon the current age. Note that for the most part this relationship is
  # symmetric, so these "invalid inliers" should come in pairs. It is not clear if
  # that is what the algorithm intended.
  step_base <- "invalid inliers" # 2h/w added in for loop
  
  # Generate a column to store standardized measurements.
  all_df$sd_val <- all_df$invalid_inlier <- NA
  
  # Go through all the ID's looking for invalid inliers.
  # only required for subjects with at least 3 values -- NOTE: table counts in
  # the same order as unique()
  for (i in unique(all_df$subjid)[table(all_df$subjid) > 3]){
    # Do this for both height and weight. Since the code is identical for both
    # except the parameter we filter on, just put it in a loop.
    for (type in c("HEIGHTCM", "WEIGHTKG")) {
      step <- paste0("2", tolower(substr(val, 1, 1)), ", ", 
                     toupper(substr(val, 1, 1)), " ", step_base) 
      
      # Pick out a subset of the data that identifies this subject along with
      # non-null and non-outlier values of the specific measurement. Identify them
      # as the index values since we need to loop through the individual rows
      # later.
      id_locs <- all_df$subjid==i & all_df$param==type &
        !is.na(all_df$stdz_meas) & !all_df$outlier
      
      # Pick out just the data for the rows in question since the age and SD
      # values are needed.
      subj_df <- all_df[id_locs,]
      
      # Make, a standardized version of the z-scores, but excluding the outlier
      # values. Note it was not clear in the paper that the outlier values should
      # be excluded when computing the standardized measurements, but it seems to
      # make sense to do so.
      subj_df$sd_val <- (subj_df$stdz_meas-mean(subj_df$stdz_meas))/
        sd(subj_df$stdz_meas)
      
      # For each of the values in the subset index,
      # No issue with an NA value for the outlier location since the sd_val is only valiud if the stdz value is, and if the stdz value is, we can determine if it is an outlier or not
      
      # calculate "differences" matrix for age days and sd vals
      age_diff <- abs(outer(subj_df$age_days, subj_df$age_days, "-"))
      sd_diff <- abs(outer(subj_df$sd_val, subj_df$sd_val, "-"))
      
      # This is where each value is checked to see if it is an "invalid inlier".
      subj_df$invalid_inlier <- sapply(1:nrow(subj_df), function(x){
        idx <-
          if (subj_df$age_days[x] <= 365.25){
            # There are separate criteria for those under 1 yr. and those over. For
            # those under, the value is an "invalid inlier" if it is more than 2.5
            # SD's aways from another value and those values are less than 3 months
            # apart. So, compare the current SD score to each other SD score on
            # that criteria. The resulting vector is
            
            which(age_diff[x,] < 90 & sd_diff[x,] > 2.5)
          } else {
            # Same check, but for the case of those over 1 yr. In this case, a
            # number is an "invalid inliers" if it is more than 3 SD's away from
            # another observation that is less than 6 months away.

            which(age_diff[x,] < 180 & sd_diff[x,] > 3)
          }
        
        # needs to be done within the loop 
        if (inter_vals & length(idx)> 0){
          step_beg <- paste0("Step_2", tolower(substr(type,1,1)), "_")
          
          # for the intermediate value, we're going to show the first age
          # difference and max sd diff combo
          inter_df[as.character(subj_df$id[x]), 
                   paste0(step_beg, "First_Age_Diff_and_Criteria")] <-
            age_diff[x, idx[1]]
          inter_df[as.character(subj_df$id[x]), 
                   paste0(step_beg, "First_SD_Diff_and_Criteria")] <-
            sd_diff[x, idx[1]]
        }
        
        return(length(idx) > 0)
      })
      
      all_df$invalid_inlier[id_locs] <- subj_df$invalid_inlier
      
      # update result and reason
      all_df$result[id_locs][subj_df$invalid_inlier] <- "Implausible"
      all_df$reason[id_locs][subj_df$invalid_inlier] <- 
        paste0("Implausible, Step ", step)
      
      
      # if using intermediate values, we want to keep some
      if(inter_vals){
        step_beg <- paste0("Step_2", tolower(substr(type,1,1)), "_")
        
        inter_df[as.character(all_df$id[id_locs]), 
                 paste0(step_beg, "WHO_Z_for_Age")] <- 
          all_df$stdz_meas[id_locs]
        inter_df[as.character(all_df$id[id_locs]), 
                 paste0(step_beg, "Subject_SD")] <- 
          subj_df$sd_val
        inter_df[as.character(all_df$id[id_locs]), 
                 paste0(step_beg, "Result")] <- 
          all_df$invalid_inlier[id_locs]
      }
    }
  }
  
  # Fill in the rest of the null values in the "invalid inlier" column as not
  # invalid inliers as long as there is a z-score (otherwise it couldn't have even
  # been under consideration as one).
  all_df$invalid_inlier[!is.na(all_df$stdz_meas) & 
                          is.na(all_df$invalid_inlier)] <- FALSE
  
  # add results to full dataframe
  df[all_df$id, "result"] <- all_df$result
  df[all_df$id, "reason"] <- all_df$reason
  
  # if we're using intermediate values, we want to save them
  if (inter_vals){
    df[as.character(inter_df$id), colnames(inter_df)[-1]] <- inter_df[,-1]
  }
  
  return(df)
}
