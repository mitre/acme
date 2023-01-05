################################################################################
# shi_2018.R: Implementation of paper to compare data cleaning algorithms for
# infant data. Original paper is by Joy Shi et al. and uses jacknife residuals
# to assess "potential biologically implausible values and outliers". The
# algorithm in the original paper is used to clean data for children from birth
# to 24 months.
# The code below is an R implementation of the author's original Stata code
# (contained in Supplementary materials of the paper). The original paper is
# available at: https://www.sciencedirect.com/science/article/pii/S1047279717306129
#
#
# @author = Max Olivier
################################################################################

# function to clean weight data by shi, et al.
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
shi_clean_both <- function(df, inter_vals = F){

  # method specific constants ----

  inter_cols <- c(
    "Step_1h_Standardized_Values",
    "Step_1h_Standardized_Residuals",
    "Step_1h_Raw_Residuals",
    "Step_1h_Result",
    "Step_1w_Standardized_Values",
    "Step_1w_Standardized_Residuals",
    "Step_1w_Raw_Residuals",
    "Step_1w_Result",
    "Step_2h_Next_Measurement",
    "Step_2h_Raw_Residuals",
    "Step_2h_Next_Raw_Residuals",
    "Step_2h_Standardized_Residuals",
    "Step_2h_Next_Standardized_Residuals",
    "Step_2h_0.85_of_Measurement",
    "Step_2h_Result",
    "Step_2w_Next_Measurement",
    "Step_2w_Raw_Residuals",
    "Step_2w_Next_Raw_Residuals",
    "Step_2w_Standardized_Residuals",
    "Step_2w_Next_Standardized_Residuals",
    "Step_2w_0.85_of_Measurement",
    "Step_2w_Result"
  )

  # begin implementation ----

  # preallocate final designation
  df$result <- "Include"
  df$reason <- ""
  df$id <- as.character(df$id)
  rownames(df) <- df$id

  # if using intermediate values, we want to start storing them
  # keep the ID to collate with the final dataframe
  inter_df <- df[, "id", drop = F]
  rownames(inter_df) <- inter_df$id

  # in order not to add additional columns to output (unless called for),
  # we create a dataframe to use for computation
  all_df <- df

  if (inter_vals){
    # if using intermediate values, preallocate values
    df[, inter_cols] <- NA
    inter_df[, inter_cols] <- NA
  }

  # preprocessing ----

  # Create standardized measurement. Assumes function is already loaded.
  all_df$stdz_meas <- get_standardized_infant_scores(
    all_df$param, all_df$age_days, all_df$sex, all_df$measurement
  )

  # Sort data by subject, param, and age.
  all_df <- all_df[order(all_df$subjid, all_df$param, all_df$age_days),]

  # Generate variables label outliers and "invalid inliers".
  all_df$raw_resid <- all_df$stdz_resid <- NA

  # Create a columns that will indicate if values are biologically implausible.
  all_df$biv <- F

  # start going through the main algorithm ----

  for (type in c("HEIGHTCM", "WEIGHTKG")){
    step_base <- tolower(substr(type, 1, 1))


    # Get the number of non-null observations per subject for both standardized and
    # raw measurements for each of height and weight.
    nobs_id_stdz <- table(all_df[!is.na(all_df$stdz_meas) &
                              all_df$param==type, "subjid"])
    nobs_id_raw <- table(all_df[!is.na(all_df$measurement) &
                             all_df$param==type,"subjid"])


    # 1h/w, H/W jackknife comparison ----
    # 1h/w: get jaccknife residuals for both standardized and raw measurements.
    # If the residual > 4, mark as outlier. If it doesn't exist, mark as "should
    # be further investigated."
    step <- paste0("1", step_base, ", ", toupper(step_base),
                   " jackknife comparison")

    # standardized first:

    # Pick out the ids with more than three observations. More than three are needed
    # to be able to compute the jackknife residuals since there are two regression
    # coefficients and we need to run the "one left out" regressions.
    reg_ids_stdz <- names(nobs_id_stdz[nobs_id_stdz > 3])


    # MAYBE TODO: SAPPLY?
    for(x in reg_ids_stdz) {
      subset_obs <- all_df$subjid==x & all_df$param==type &
        !is.na(all_df$stdz_meas)
      reg <- lm(stdz_meas ~ age_days, data = all_df[subset_obs,])
      jk_res <- rstudent(reg) # Built in R function for jackknife residuals
      all_df[subset_obs, "stdz_resid"] <- jk_res
    }

    # raw second:
    # Getting jackknife residuals for raw measurements. Do this for both height and
    # weight.

    # For the same reasons as above, pick out the ids with more than three
    # observations.
    reg_ids_raw <- names(nobs_id_raw[nobs_id_raw > 3])

    # Here the regression in on square root of age instead of age, per the authors'
    # work.
    for(x in reg_ids_raw) {
      subset_obs <- all_df$subjid==x & all_df$param==type &
        !is.na(all_df$measurement)
      reg <- lm(measurement ~ I(sqrt(age_days)), data=all_df[subset_obs,])
      jk_res <- rstudent(reg) # Built in R function for jackknife residuals
      all_df[subset_obs, "raw_resid"] <- jk_res
    }

    # outlier identification
    # Identify potential outliers as those whose jackknife residual is more than
    # four in absolute value.

    # The cases where the outlier value is NA correspond to cases where the residual
    # was NA, meaning the regression wasn't run for this ID since it didn't have
    # enough observations. Per the algorithm, mark these with a 9 to indicate
    # "should be further investigated"

    # assign to output
    all_df[(is.na(all_df$stdz_resid) | is.na(all_df$raw_resid)),
           "reason"] <-
      paste0("No Jackknife Investigate Further, Step ", step)

    all_df$stdz_outlier <- abs(all_df$stdz_resid) > 4 & !is.na(all_df$stdz_resid)

    all_df$raw_outlier <- abs(all_df$raw_resid) > 4 & !is.na(all_df$raw_resid)

    # assign to output
    all_df[all_df$stdz_outlier | all_df$raw_outlier, "result"] <- "Implausible"
    all_df[all_df$stdz_outlier & !all_df$raw_outlier, "reason"] <-
      paste0("Standarized Outlier, Step ", step)
    all_df[!all_df$stdz_outlier & all_df$raw_outlier, "reason"] <-
      paste0("Raw Outlier, Step ", step)
    all_df[all_df$stdz_outlier & all_df$raw_outlier, "reason"] <-
      paste0("Both Outlier, Step ", step)

    # if using intermediate values, we want to keep some
    if (inter_vals){
      step_beg <- paste0("Step_1", step_base, "_")
      val_subset <- all_df$param == type
      
      inter_df[as.character(all_df$id[val_subset]),
               paste0(step_beg, "Standardized_Values")] <-
        all_df$stdz_meas[val_subset]
      inter_df[as.character(all_df$id[val_subset]),
               paste0(step_beg, "Standardized_Residuals")] <-
        all_df$stdz_resid[val_subset]
      inter_df[as.character(all_df$id[val_subset]),
               paste0(step_beg, "Raw_Residuals")] <-
        all_df$raw_resid[val_subset]
      inter_df[as.character(all_df$id[val_subset]), 
               paste0(step_beg, "Result")] <-
        all_df$stdz_outlier[val_subset] | all_df$raw_outlier[val_subset]
    }

    # 2h/w, H/W BIV ----
    # 2h/w, Identify biologically implausible in sequential pairs. If measurement
    # of earlier time point is > next time point (for height) or measurement of
    # earlier time point*0.85 > next time point (for weight), identify larger
    # absolute residual as BIV, for both standardized and raw residuals.
    step <- paste0("2", step_base, ", ", toupper(step_base),
                   " BIV")

    # These are identified by looking for
    # biologically implausible pairs, which are adjacent height observations where
    # the raw measure fell, or adjacent weight observations where the raw measure
    # fell by more than 15%.


    # ONLY CHECK WHERE ALL THE THE SUBJECT ISN'T ALL OUTLIERS

    # Go through all of the ID's to find biologically implausible pairs, and label
    # one value in the pair as a biologically implausible value. To determine which
    # of the two is the implausible value, look at the residuals, and the one with
    # the larger absolute value residual is considered the implausible value. This
    # process is repeated for both standardized and raw residuals. Also address both
    # height and weight together in the same loop for each ID.
    for (id in unique(all_df$subjid)) {
      # Find all the height observations for this ID.
      idvar_locs <- which(all_df$subjid==id & all_df$param==type)

      subj_df <- all_df[idvar_locs,]

      # Only go through checking if there is enough
      if(nrow(subj_df) > 1) {
        # make the column we're comparing to and the lagged-- next value column
        subj_df$raw_resid_next <- c(subj_df$raw_resid[-1], NA)
        subj_df$stdz_resid_next <- c(subj_df$stdz_resid[-1], NA)
        subj_df$measurement_next <- c(subj_df$measurement[-1], NA)

        for (check_val in c("raw_resid", "stdz_resid")){

          # Actually identifying if this pair is biologically implausible. Need to
          # make sure that there are non-null residuals to compare as well as
          # checking the value decrease.
          criteria <- !is.na(subj_df[, check_val]) &
            !is.na(subj_df[, paste0(check_val, "_next")]) &
            if (type == "HEIGHTCM"){
              subj_df$measurement > subj_df$measurement_next
            } else { # weight
              subj_df$measurement*.85 > subj_df$measurement_next
            }

          # first -- check that they both exist
          # then -- check that first measurement is bigger than the next
          subj_df[
            criteria &
              abs(subj_df[, check_val]) >
              abs(subj_df[, paste0(check_val, "_next")]),
            "biv"
          ] <- T

          subj_df[
            criteria &
              abs(subj_df[, check_val]) <=
              abs(subj_df[, paste0(check_val, "_next")]),
            "biv"
          ] <- T
        }
        
        # if using intermediate values, we want to keep some
        if (inter_vals){
          step_beg <- paste0("Step_2", step_base, "_")
          
          inter_df[as.character(subj_df$id),
                   paste0(step_beg, "Next_Measurement")] <-
            subj_df$measurement_next
          inter_df[as.character(subj_df$id),
                   paste0(step_beg, "Raw_Residuals")] <-
            subj_df$raw_resid
          inter_df[as.character(subj_df$id),
                   paste0(step_beg, "Next_Raw_Residuals")] <-
            subj_df$raw_resid_next
          inter_df[as.character(subj_df$id),
                   paste0(step_beg, "Standardized_Residuals")] <-
            subj_df$stdz_resid
          inter_df[as.character(subj_df$id),
                   paste0(step_beg, "Next_Standardized_Residuals")] <-
            subj_df$stdz_resid_next
          if (type == "WEIGHTKG"){
            inter_df[as.character(subj_df$id),
                     paste0(step_beg, "0.85_of_Measurement")] <-
              subj_df$measurement*.85
          }
          inter_df[as.character(subj_df$id), 
                   paste0(step_beg, "Result")] <-
            subj_df$biv
        }
        
        all_df[subj_df$id, "biv"] <- subj_df$biv
        
      }
    }

    # all df has the output

    # assign to output
    all_df[all_df$biv, "result"] <- "Implausible"
    all_df[all_df$biv, "reason"] <-
      paste0("Implausible, Step ", step)

  }

  # add results to full dataframe
  df[all_df$id, "result"] <- all_df$result
  df[all_df$id, "reason"] <- all_df$reason
  
  
  # if we're using intermediate values, we want to save them
  if (inter_vals){
    df[as.character(inter_df$id), colnames(inter_df)[-1]] <- inter_df[,-1]
  }

  return(df)
}

