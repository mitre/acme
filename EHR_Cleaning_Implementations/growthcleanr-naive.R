# Implementing Daymont, et al. (growthcleanr-naive)
# By Hannah De los Santos
# Originated on: 10/30/2020

# paper: https://academic.oup.com/jamia/article/24/6/1080/3767271

# This is more of a speculative implementation.
# Note: this could be more clean/optimized.

# supporting functions for this implementation only ----

as.matrix.delta <- function(agedays) {
  n <- length(agedays)
  delta <- abs(matrix(rep(agedays, n), n, byrow = T) - agedays)
  
  return(delta)
}

#' Exponentially Weighted Moving Average (EWMA)
#'
#' \code{ewma} calculates the exponentially weighted moving average (EWMA) for a set of numeric observations over time.
#'
#' @param agedays Vector of age in days for each z score (potentially transformed to adjust weighting).
#'
#' @param z Input vector of numeric z-score data.
#'
#' @param ewma.exp Exponent to use for weighting.
#'
#' @param ewma.adjacent Specify whether EWMA values excluding adjacent measurements should be calculated.  Defaults to TRUE.
#'
#' @return Data frame with 3 variables:
#' * The first variable (ewma.all) contains the EWMA at observation time
#'   excluding only the actual observation for that time point.
#' * The second variable (ewma.before) contains the EWMA for each observation excluding both the actual observation
#'   and the immediate prior observation.
#' * The third variable (ewma.after) contains the EWMA for each observation excluding both the actual observation
#'   and the subsequent observation.
ewma <- function(agedays, z, ewma.exp = 1.5, ewma.adjacent = T) {
  # 6.  EWMA calculation description: Most of the next steps will involve calculating the exponentially weighted moving average for each subject and parameter. I will
  #     describe how to calculate EWMASDs, and will describe how it needs to be varied in subsequent steps.
  # a.	The overall goal of the EWMASD calculation is to identify the difference between the SD-score and what we might predict that DS-score should be, in order to
  #     determine whether it should be excluded.
  # b.	Only nonmissing SD-scores for a parameter that are not designated for exclusion are included in the following calculations.
  # c.	For each SD-score SDi and associated agedaysi calculate the following for every other z-score (SDj…SDn) and associated agedays (agedaysj…agedaysn)  for the
  #     same subject and parameter
  #   i.	ΔAgej=agedaysj-agedaysi
  #   ii.	EWMAZ=SDi=[Σj→n(SDj*((5+ΔAgej)^-1.5))]/[ Σj→n((5+ΔAgej)^-1.5)]
  #   iii.	For most EWMASD calculations, there are 3 EWMASDs that need to be calculated. I will note if not all of these need to be done for a given step.
  #     1.	EWMASDall calculated as above
  #     2.	EWMAZbef calculated excluding the SD-score just before the SD-score of interest (sorted by agedays). For the first observation for a parameter for a
  #         subject, this should be identical to EWMASDall rather than missing.
  #     3.	EWMAZaft calculated excluding the z-score just after the SD-score of interest (sorted by agedays). For the lastobservation for a parameter for a subject,
  #         this should be identical to EWMASDall rather than missing.
  #   iv.	For each of the three EWMASDs, calculate the dewma_*=SD-EWMASD
  # d.	EWMASDs and ΔEWMASDs will change if a value is excluded or manipulated using one of the methods below, therefore EWMASDs and ΔEWMASDs be recalculated for each
  #     step where they are needed.
  # e.	For these calculations, use variables that allow for precise storage of numbers (in Stata this is called 'double') because otherwise rounding errors can cause
  #     problems in a few circumstances
  
  n <- length(agedays)
  # initialize response variables
  ewma.all <- ewma.before <- ewma.after <- vector('numeric', 0)
  if (n > 0) {
    # organize into data frame and sort into order of increasing age,
    # but retain original sort order information in index
    if (!all(agedays == cummax(agedays)))
      warning("EWMA ordering is not sorted; double check") #add in a check to make sure the inputs are already sorted (they should be)
    index <- order(agedays)
    
    # calculate matrix of differences in age, and add 5 to each delta per Daymont algorithm
    delta <- as.matrix.delta(agedays)
    delta <- ifelse(delta == 0, 0, (delta + 5) ^ ewma.exp)
    
    # calculate EWMAs, and return in order of original data
    ewma.all[index] <- delta %*% z / apply(delta, 1, sum)
    
    if (ewma.adjacent) {
      if (n > 2) {
        delta2 = delta
        delta2[col(delta2) == row(delta2) - 1] = 0
        ewma.before[index] = delta2 %*% z / apply(delta2, 1, sum)
        delta3 = delta
        delta3[col(delta3) == row(delta3) + 1] = 0
        ewma.after[index] = delta3 %*% z / apply(delta3, 1, sum)
      } else {
        ewma.before <- ewma.after <- ewma.all
      }
    }
  }
  # return all 3 EWMAs as a data frame
  return(if (ewma.adjacent)
    data.frame(ewma.all, ewma.before, ewma.after)
    else
      data.frame(ewma.all))
}

# Function to remove data based on exponentially-weighted moving average 
# (Daymont, et al.). Cutoff defaults adjusted downwards for adults.
# inputs:
# subj_df: subject data frame, which has age in days and z-score
# ewma_cutoff: EWMA past which considered invalid (center value). left and right
#   are .5 less.
# outputs:
#  logical indicating whether to exclude a record
remove_ewma <- function(subj_df, ewma_cutoff = 2, 
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
    ewma_res <- ewma(subj_df$age_days, subj_df$z)
    
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
#   age_years: age, in years
#   param: HEIGHTCM or WEIGHTKG
#   measurement: height or weight measurement
# outputs:
#   df, with additional columns:
#     result, which specifies whether the height measurement should be included,
#       or is implausible.
#     reason, which specifies, for implausible values, the reason for exclusion,
#       and the step at which exclusion occurred.
growthcleanr_clean_both <- function(df, inter_vals = F){
  
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
    
    # 1h, H calculate ewma ----
    # 1h. Exclude extreme errors by calculating the exponentially weighted 
    # moving average and removing by a specified cutoff. If record(s) is/are 
    # found to be extreme, remove the most extreme one and recalculate. Repeat
    # until this no more values are found to be extreme.
    step <- "1h, H calculate ewma"
    
    # convert age years to days
    subj_df$age_days <- subj_df$age_years*365.2425
    
    # if using intermediate values, we want to keep some
    if (inter_vals){
      inter_df[as.character(subj_df$id), "Step_1h_Age_days"] <-
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
        inter_df[as.character(subj_df$id), "Step_1h_Z-Score"] <-
          subj_df$z
      }
      
      # calculate the criteria to remove the ewma
      criteria_list <- remove_ewma(subj_df, ewma_cutoff = 2,
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
    df[names(subj_keep), "result"] <- subj_keep
    df[names(subj_reason), "reason"] <- subj_reason
    
    # then do weight ----
    
    w_df <- df[df$param == "WEIGHTKG" & slog,]
    
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
      criteria_list <- remove_ewma(subj_df, ewma_cutoff = 2,
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
    df[names(subj_keep), "result"] <- subj_keep
    df[names(subj_reason), "reason"] <- subj_reason
    
    # if we're using intermediate values, we want to save them
    if (inter_vals){
      df[as.character(inter_df$id), colnames(inter_df)[-1]] <- inter_df[,-1]
    }
  }
  
  return(df)
}
