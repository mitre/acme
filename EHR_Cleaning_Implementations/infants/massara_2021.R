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

# Import library used (though it isn't required, could do without data.table).
library(data.table)

################################################################################
# Import and clean data for testing----
# Algorithm needs subject ID, age in days, measurement type (param) and z-scores
# to identify outliers, but since we start with raw data, also need sex so that
# we can get z-scpores. For-age-z-scores of measurement are created from WHO
# values (reference 20 in paper). Data should be in long format.
################################################################################

# What to use as cutoffs here? Carsley says WHO cutoffs are outside [-6, 6] for 
# height and putside [-6, 5] for weight. Massara says outside [-5, 5], but they
# are using weight-for-length.

# Import synthetic data set for testing.
dat <- fread("C:/Users/molivier/Documents/Projects/CDC_CODI/Data_sets/gc-observations.csv")

# Filter out the data dropping the age in years.
df <- dat[, c("id", "subjid", "param", "measurement", "sex", "age_days")]

df <- df[df$subjid %in% unique(df$subjid)[1:100],]

df <- as.data.frame(df)

################################################################################
# Setting up variables.----
################################################################################




massara_clean_both <- function(df, inter_vals=FALSE) {
  
  # method specific constants ----

  inter_cols <- c(
    "Step_1h_MinAge_Diff",
    "Step_1h_MaxSD_Diff_Under2Yrs",
    "Step_1h_Result",
    "Step_1h_Standardized_Values",    
    "Step_1w_MinAge_Diff",
    "Step_1w_MaxSD_Diff_Under2Yrs",
    "Step_1w_Result",
    "Step_1w_Standardized_Values"    
  )
  
  
  # Begin implementation ----
  
  # preallocate final designation
  df$result <- "Include"
  df$reason <- ""
  df$id <- as.character(df$id)
  rownames(df) <- df$id
  
  # if using intermediate values, we want to start storing them
  # keep the ID to collate with the final dataframe
  inter_df <- df[, "id", drop = F]
  rownames(inter_df) <- inter_df$id
  
  # To avoid adding additional columns to output (unless called for), create a 
  # dataframe to use for computation.
  all_df <- df
  
  # if using intermediate values, preallocate values
  if (inter_vals){
    df[, inter_cols] <- NA
    inter_df[, inter_cols] <- NA
  }
  
  # Preprocessing----
  
  # Create standardized measurement. Assumes function is already loaded.
  all_df$stdz_meas <- get_standardized_infant_scores(all_df$param, all_df$age_days, all_df$sex, all_df$measurement)
  
  # Sort data by subject, param, and age.
  all_df <- all_df[order(all_df$subjid, all_df$param, all_df$age_days),]
  
  # Generate variable to label outliers.
  all_df$outlier <- NA
  
  # Need to identify potential outliers for each subject individually since
  # comparing values within a subject.
  for (type in c("HEIGHTCM", "WEIGHTKG")) {
    step_base <- tolower(substr(type, 1, 1))
    
    # 1h/w, H/W mBIV ----
    # 1h/w: Check if the observation is biologically implausible according to 
    # the mBIV criteria. The mBIV criteria are that the value has a standardized
    # score that would make it an outlier by WHO standards and that it either 
    # does not have any other observations within two years of it, or if it does
    # that at least one of those observations has a standardized score more than 
    # 2 away. 
    step <- paste0("1", step_base, ", ", toupper(step_base), " mBIV")
    
    # Need to do things separately for height and weight.
    for (uid in unique(df$subjid[df$param==type])) {
      
      # Get all observations for this subject with this data type where there is 
      # as a valid standardized value. Able to link back to full df using "id"
      # column,
      id_locs <- all_df$subjid==uid & all_df$param==type & !is.na(all_df$stdz_meas)
      subj_df <- all_df[id_locs,]
      rownames(subj_df) <- 1:nrow(subj_df)
      
      # Proceed only if there are values for this parameter for this subject.
      if(nrow(subj_df) > 0) {
        # Identify all the potential outlier locations for this subject for this 
        # parameter as row locations in "subj_df". This allows us to correspond 
        # them with the difference matrices. There are different criteria for 
        # height and weight.
        if(type=="HEIGHTCM") {
          pot_outlier <- as.integer(rownames(subj_df)[abs(subj_df$stdz_meas) > 6]) 
        } else {
          pot_outlier <- as.integer(rownames(subj_df)[subj_df$stdz_meas < -6 | 
                                                        subj_df$stdz_meas > 5])
        }

        # Calculate difference matrices
        age_diff <- abs(outer(subj_df$age_days, subj_df$age_days, "-"))
        sd_diff <- abs(outer(subj_df$stdz_meas, subj_df$stdz_meas, "-"))
        
        for(x in pot_outlier) {

          # Get the two criteria. First that there is an observation within two
          # years. Second, if there areobservations within two years, that there
          # is at least one difference in standardized scores greater than 2 in
          # absolute value. Making them separate variables for readability since
          # they are used multiple times.
          obs_within2y <- sum(age_diff[x,] < 730.5)>0
          sd_g2_within2y <- sum((age_diff[x,] < 730.5) & (sd_diff[x,] > 2)) > 0
          
          # Get a vector of the age differences from 
          subj_df$outlier[x] <- !obs_within2y | sd_g2_within2y
          if(inter_vals) {
            step_beg <- paste0("Step_1", step_base, "_")
            # Get the minimum absolute age difference between this outlier and 
            # other observations. Given how the outer matrices are constructed, 
            # though, there will be a zero in the row (along the main diagonal
            # where observation is itself), so need to find min of values >0.
            # If there is only one observation of this type for this ID (in 
            # which case there are no age differences), return NA.
            inter_df[subj_df$id[x], paste0(step_beg, "MinAge_Diff")] <-
              ifelse(length(age_diff[x,]) > 1, 
                     min(age_diff[x,][age_diff[x,] > 0]), NA)
            
            # Get largest standardized difference that is within two years. If
            # there is no standardized difference within two years, return NA. 
            inter_df[subj_df$id[x], paste0(step_beg, "MaxSD_Diff_Under2Yrs")] <-
              ifelse(obs_within2y, max(sd_diff[x,][age_diff[x,] < 730.5]), NA)

            # Change the result for this observation at this step.
            inter_df[subj_df$id[x], paste0(step_beg, "Result")] <-
              !obs_within2y | sd_g2_within2y
          }
        }
        
        # And set the outlier status in all_df.
        all_df$outlier[id_locs] <- subj_df$outlier
        if(inter_vals) {
          step_beg <- paste0("Step_1", step_base, "_")

          inter_df[subj_df$id, paste0(step_beg, "Standardized_Values")] <-
            subj_df$stdz_meas
        }
      }
    }
    # Check for is.na(df$outlier) here instead?
    all_df[!is.na(all_df$outlier) & all_df$outlier & all_df$param==type, "result"] <- "Implausible"
    all_df[!is.na(all_df$outlier) & all_df$outlier & all_df$param==type, "reason"] <- paste0("Implausible, Step ", "check")
  }

  # add results to full dataframe
  df[all_df$id, "result"] <- all_df$result
  df[all_df$id, "reason"] <- all_df$reason
  
  if (inter_vals){
    df[as.character(inter_df$id), colnames(inter_df)[-1]] <- inter_df[,-1]
  }
  
  return(df)
}
