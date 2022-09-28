################################################################################
# massara_2021.R: Implementation of paper to compare data cleaning algorithms
# for infant data. Original paper is by Massara et al. and detects "modified
# biologically implausible values" (mBIVs) as those that have a a standardized
# score that would make it an outlier by WHO standards and that it either
# does not have any other observations within two years of it, or if it does
# that at least one of those observations has a standardized score more than
# 2 away.
# The original paper is available at:
# https://dl.acm.org/doi/abs/10.5555/3507788.3507821
#
# @author = Max Olivier
################################################################################

# function to clean height and weight data by Massara, et al.
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
massara_clean_both <- function(df, inter_vals=FALSE) {

  # method specific constants ----

  inter_cols <- c(
    "Step_1zwfl_MinAge_Diff",
    "Step_1zwfl_MaxSD_Diff_Under2Yrs",
    "Step_1zwfl_Result",
    "Step_1zwfl_Standardized_Values"
  )

  # Begin implementation ----

  # Seems like it makes sense to not actually preallocate final designations
  # here since we need to merge disaggregated data on later and since the
  # merging data frame is likely to be slightly smaller, we need to use an actual
  # merge() command instead of just assignment, and merging is easier if the
  # columns aren't there already.
  # preallocate final designation
  # df$result <- "Include"
  # df$reason <- ""


  # It seems like the merging later still works correctly even if we change the
  # id to characters (we merge on the id variable), but would it be easier if we
  # left it as an integer and changed to character at the end of the function?
  df$id <- as.character(df$id)
  rownames(df) <- df$id

  # Seems like we don't actually need an intermediate values data frame here,
  # everytuhing gets stored in the mereged_df frame created below. So can
  # probably cut the below.
  # if using intermediate values, we want to start storing them
  # keep the ID to collate with the final dataframe
  # inter_df <- df[, "id", drop = F]
  # rownames(inter_df) <- inter_df$id

  # Doesn't seem like we actually need the separate data frame used for
  # computation here, since data frame for computation has to be a combined data
  # frame with height and weight measurements as columns that is later
  # disaggregated.
  # To avoid adding additional columns to output (unless called for), create a
  # dataframe to use for computation.
  # all_df <- df

  # Again probably do not want to do this here since will need to merge data not
  # just assign column as mentioned above.
  # if using intermediate values, preallocate values
  # if (inter_vals){
  #   df[, inter_cols] <- NA # Why is this here???
  #   inter_df[, inter_cols] <- NA
  # }

  # Preprocessing----

  # Separate out height and weight data and then merge them together based on
  # subject ID, sex, and age_days (the second is not really needed since subject
  # ID's are unique, but it prevents an extra column form being created).
  # Merge here to avoid lots of duplicate columns.
  merged_df <- merge(df[df$param=="HEIGHTCM",], df[df$param=="WEIGHTKG",],
                     by=c("subjid", "sex", "age_days"))

  # Need to make an updated ID for the merged data set in order to get the
  # intermediate values to work.
  merged_df$merge_id <- as.character(1:nrow(merged_df))
  rownames(merged_df) <- merged_df$merge_id

  # Get the standardized scores for weight-for-length, which is the variable we
  # use for determining outliers. This function actually gives several different
  # standardized scores, but we will only pick out the weight-for-length. Need
  # to load a library first.
  library(anthro)
  std_scores <- anthro_zscores(sex=(merged_df$sex+1), age=merged_df$age_days, weight=merged_df$measurement.y,
                               lenhei=merged_df$measurement.x)

  # Add standardized wfl value to data frame and generate variable to label
  # outliers.
  merged_df$zwfl <- std_scores$zwfl
  merged_df$outlier <- NA

  # Add intermediate value columns to fill in if applicable.
  if(inter_vals) {
    merged_df[, inter_cols] <- NA
  }

  # Designating outlier for combined data ----

  # The step-base is the same throughout, so set it here.
  step_base <- "zwfl"

  # Go through each subject individually to see which of there combined height
  # and weight observations should be labeled as outliers depending upon the
  # value of the standardized weight-for-length variable and its relation to
  # other wfl variables within two years.
  for (uid in unique(merged_df$subjid)) {

    # Create a smaller data frame of just observations for this subject ID.
    id_locs <- merged_df$subjid==uid & !is.na(merged_df$zwfl)
    subj_df <- merged_df[id_locs,]

    # Proceed only if there are values for this parameter for this subject.
    if(nrow(subj_df) > 0) {
      # Identify all the potential outlier locations for this subject for this
      # parameter as row locations in "subj_df". This allows us to correspond
      # them with the difference matrices. The criteria outlined in the paper is
      # that a value is a potential outlier if its wfl is greater than 6 in
      # absolute value.
      pot_outlier <- which(abs(subj_df$zwfl) > 6)

      # Calculate difference matrices (row num - col num) for age and difference
      # in standardized values that we use to confirm if the potential outlier
      # is a true outlier.
      age_diff <- abs(outer(subj_df$age_days, subj_df$age_days, "-"))
      sd_diff <- abs(outer(subj_df$zwfl, subj_df$zwfl, "-"))

      for(x in pot_outlier) {
        # Get the two criteria that will determine if this potential outlier is
        # should actually be labeled an outlier. The potential outlier is
        # labeled  an outlier if:
        # -there are no other observations within two years of it
        # -if there are observations withing two years of it, then there is at
        # least one difference in standardized scores greater than 2 in
        # absolute value
        # Making separate variables for readability since the criteria are used
        # multiple times.
        obs_within2y <- sum(age_diff[x,] < 730.5)>0
        sd_g2_within2y <- sum((age_diff[x,] < 730.5) & (sd_diff[x,] > 2)) > 0

        subj_df$outlier[x] <- !obs_within2y | sd_g2_within2y

        if(inter_vals) {
          step_beg <- paste0("Step_1", step_base, "_")
          # Get the minimum absolute age difference between this potential
          # outlier and other observations. Given how the outer matrices are
          # constructed, though, there will be a zero in the row (along the main
          # diagonal where observation is itself), so need to find min of values
          # >0. If there is only one observation of this type for this ID (in
          # which case there are no age differences), return NA. Just put
          # directly into the corresponding intermediate value column of the
          # main data frame. This is where the "merge_id" column is essential.
          merged_df[subj_df$merge_id[x], paste0(step_beg, "MinAge_Diff")] <-
            ifelse(length(age_diff[x,]) > 1,
                   min(age_diff[x,][age_diff[x,] > 0]), NA)

          # Get largest standardized difference that is within two years. If
          # there is no standardized difference within two years, return NA.
          merged_df[subj_df$merge_id[x], paste0(step_beg, "MaxSD_Diff_Under2Yrs")] <-
            ifelse(obs_within2y, max(sd_diff[x,][age_diff[x,] < 730.5]), NA)
        }
      }

      # And set the outlier status in merege_df.
      merged_df$outlier[id_locs] <- subj_df$outlier
    }
  }

  # Set the values of "Step_1zwfl_Result" and "Step_1zwfl_Standardized_Values"
  # which correspond directly to two of the columns already in merged_df.
  if(inter_vals) {
    merged_df$Step_1zwfl_Standardized_Values <- merged_df$zwfl
    merged_df$Step_1zwfl_Result <- merged_df$outlier
  }

  # Disaggregating height and weight data ----

  # List to keep disaggreagted data frames for height and weight that can then
  # be put together with rbind().
  disag_data <- list()

  # Create data set that disaggregates "merged_df" into a format like the
  # original "df". In order to do this, need to create separate data sets for
  # height and weight and then rbind() them. Since just pulling the
  # height/weight values from "merged_df" will have duplicate height/weight
  # observations (that is the same ID multiple times), need to condense those.
  # When condensing, label an outlier if it was part of any outlier pair, and
  # just pick one of the weight-for-length values. Do this in a loop where 1
  # corresponds to height and 2 to weight.
  for(j in 1:2) {
    # Set the columns that need to be read for height/weight.
    if(j==1) {
      param_dat_cols <- c("subjid", "sex", "age_days", "id.x", "param.x",
                          "measurement.x", "zwfl", "outlier")
    } else {
      param_dat_cols <- c("subjid", "sex", "age_days", "id.y", "param.y",
                          "measurement.y", "zwfl", "outlier")
    }
    # The revised column names are the same for both height and weight.
    param_dat_names <- c("subjid", "sex", "age_days", "id", "param",
                         "measurement", "zwfl", "outlier")

    # If we are collecting intermediate values, need to append the intermediate
    # columns.
    if(inter_vals) {
      param_dat_cols <- c(param_dat_cols, inter_cols)
      param_dat_names <- c(param_dat_names, inter_cols)
    }

    # Pull the appropriate data for this parameter, and give the correct column
    # names so that things can be combined later.
    param_dat <- merged_df[,param_dat_cols]
    colnames(param_dat) <- param_dat_names

    # Create a reduced version of the data set, where each observation ID
    # appears only once. We set the length of the new data frame to (hopefully)
    # speed things up a bit.
    param_dat_reduce <- data.frame(matrix(NA, nrow=length(unique(param_dat$id)),
                                          ncol=ncol(param_dat)))
    colnames(param_dat_reduce) <- colnames(param_dat)

    # Now fill in the reduced data frame. Do this by pulling out all the
    # observations associated with an observation ID, checking to see if any of
    # them are part of an outlier, and then picking a row to fill in the reduced
    # data frame. Just choose the first outlier or non-outlier observation. Need
    # the "i" index to keep track of which row is being filled in in the reduced
    # data frame.
    i <- 1
    for(urid in unique(param_dat$id)) {
      temp_df <- param_dat[param_dat$id==urid,]
      outlier_rows <- which(temp_df$outlier==TRUE)

      ret_row <- temp_df[1,]

      if(length(outlier_rows) > 0) {
        ret_row <- temp_df[outlier_rows[1],]
      }
      param_dat_reduce[i,] <- ret_row
      i <- i+1
    }

    # Finally, put the reduced data frame into a list so that we can combine the
    # two data frames later.
    disag_data[[j]] <- param_dat_reduce
  }


  # Combine the disagregated height and weight data into a single data frame.
  # Note that even though each height/weight ID value now appears only once,
  # this data frame almost certainly has fewer columns than the original df
  # since somce height/weight values in the original data frame did not have a
  # corresponding weight/height on that day, and thus a wfl value could not be
  # computed for them (and they were not in merged_df).
  disag_df <- rbind(disag_data[[1]], disag_data[[2]])


  # Create "result" and "reason" columns that will then be merged onto "df".
  disag_df$result <- ifelse(!is.na(disag_df$outlier) & (disag_df$outlier==TRUE),
                            "Implausible", "Include")
  disag_df$reason <- ifelse(!is.na(disag_df$outlier) & (disag_df$outlier==TRUE),
                            "Implausible, Step 1zwfl, ZWFL mBIV", "")

  # And update the values in df. Merge the result and reason columns. on id.
  # QUESTION, what do we want to put in the rows that have NA's for result and
  # reason after the merge?
  df <- merge(df, disag_df[,c("id", "result", "reason")], by="id", all=TRUE)

  # If we want to see intermediate values, merge those on as well.
  if(inter_vals) {
    df <- merge(df, disag_df[,c("id", inter_cols)], by="id", all=TRUE)
  }

  return(df)
}
