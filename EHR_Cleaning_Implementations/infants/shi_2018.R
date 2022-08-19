################################################################################
# shi_2018.R: Implementation of paper to compare data cleaning algorithms for
# infant data. Original paper is by Joy Shi et al. and uses jacknife residuals
# to assess "potential biologically implausible values and outliers". The
# algorithm in the original paper is used to clean data for children from birth
# to 24 months.
# The code below is an R implementation of the author's original Stata code
# (contained in Supplementary materials of the paper). The original paper is
# available at:
#
#
# @author = Max Olivier
################################################################################

# Import necessary libraries
library(data.table)

################################################################################
# Import and clean data for testing----
# Algorithm needs 4 columns of data: patient/subject ID, age (in days), actual
# measurement, and for-age-z-score of measurement that are created from multiple
# sources (outlined on pg. 211.e1 of paper). Data should be in long format.
################################################################################

# Import synthetic data set for testing.

dat <- fread("C:/Users/molivier/Documents/Projects/CDC_CODI/Data_sets/gc-observations.csv")

# Filter out the data in several ways. Just keep weight data (since original
# algorithm uses weight data) and drop all the columns
df <- dat[, c("subjid", "param", "measurement", "sex",  "age_days")]

# UPLOAD DATA SETS AS WELL

################################################################################
# Shi algorithm, part (2)----
# Setting up variables and lists of ID's to loop over.
################################################################################

# Create standardized measurement. Assumes function is already loaded.
df[, stdz_meas := get_standardized_infant_scores(param, age_days, sex, measurement)]

# Sort data by subject, param, and age.
df <- df[order(df$subjid, df$param, df$age_days),]

df <- df[1:100000,]

# Generate variables to store jackknife residuals. Make sure they are numerics
# so that we can add the values in later.
df[,raw_resid := as.numeric(NA)]
df[,stdz_resid := as.numeric(NA)]

# Get the number of non-null observations per subject for both standardized and
# raw measurements for each of height and weight.
ht_nobs_id_stdz <- table(df[!is.na(df$stdz_meas) &
                            df$param=="HEIGHTCM",]$subjid)
ht_nobs_id_raw <- table(df[!is.na(df$measurement) &
                            df$param=="HEIGHTCM",]$subjid)
wt_nobs_id_stdz <- table(df[!is.na(df$stdz_meas) &
                            df$param=="WEIGHTKG",]$subjid)
wt_nobs_id_raw <- table(df[!is.na(df$measurement) &
                            df$param=="WEIGHTKG",]$subjid)


################################################################################
# Shi algorithm, part (3)----
# Getting jackknife residuals for standardized measurements. We do this for both
# height and weight while they just do the height version in the paper.
################################################################################

# Pick out the ids with more than three observations. More than three are needed
# to be able to compute the jackknife residuals since there are two regression
# coefficients and we need to run the "one left out" regressions.
ht_reg_ids_stdz <- names(ht_nobs_id_stdz[ht_nobs_id_stdz > 3])

for(x in ht_reg_ids_stdz) {
  subset_obs <- df$subjid==x & df$param=="HEIGHTCM" & !is.na(df$stdz_meas)
  reg <- lm(stdz_meas ~ age_days, data=df[subset_obs,])
  jk_res <- rstudent(reg) # Built in R function for jackknife residuals
  df[subset_obs, "stdz_resid"] <- jk_res
}

# Same regression, but for weight observations.
wt_reg_ids_stdz <- names(wt_nobs_id_stdz[wt_nobs_id_stdz > 3])

for(x in wt_reg_ids_stdz) {
  subset_obs <- df$subjid==x & df$param=="WEIGHTKG" & !is.na(df$stdz_meas)
  reg <- lm(stdz_meas ~ age_days, data=df[subset_obs,])
  jk_res <- rstudent(reg) # Built in R function for jackknife residuals
  df[subset_obs, "stdz_resid"] <- jk_res
}

################################################################################
# Shi algorithm, part (4)----
# Getting jackknife residuals for raw measurements. Do this for both height and
# weight.
################################################################################

# For the same reasons as above, pick out the ids with more than three
# observations.
ht_reg_ids_raw <- names(ht_nobs_id_raw[ht_nobs_id_raw > 3])

# Here the regression in on square root of age instead of age, per the authors'
# work.
for(x in ht_reg_ids_raw) {
  subset_obs <- df$subjid==x & df$param=="HEIGHTCM" & !is.na(df$measurement)
  reg <- lm(measurement ~ I(sqrt(age_days)), data=df[subset_obs,])
  jk_res <- rstudent(reg) # Built in R function for jackknife residuals
  df[subset_obs, "raw_resid"] <- jk_res
}

# Repeat for weights.
wt_reg_ids_raw <- names(wt_nobs_id_raw[wt_nobs_id_raw > 3])

for(x in wt_reg_ids_raw) {
  subset_obs <- df$subjid==x & df$param=="WEIGHTKG" & !is.na(df$measurement)
  reg <- lm(measurement ~ I(sqrt(age_days)), data=df[subset_obs,])
  jk_res <- rstudent(reg) # Built in R function for jackknife residuals
  df[subset_obs, "raw_resid"] <- jk_res
}


################################################################################
# Shi algorithm, part (5)----
# Identify potential outliers as those whose jackknife residual is more than
# four in absolute value.
################################################################################

# The cases where the outlier value is NA correspond to cases where the residual
# was NA, meaning the regression wasn't run for this ID since it didn't have
# enough observations. Per the algorithm, mark these with a 9 to indicate
# "should be further investigated"

df$stdz_outlier <- abs(df$stdz_resid) > 4
df$stdz_outlier[is.na(df$stdz_outlier)] <- 9

df$raw_outlier <- abs(df$raw_resid) > 4
df$raw_outlier[is.na(df$raw_outlier)] <- 9

################################################################################
# Shi algorithm, part (6)----
# Identify biologically implausible values. These are identified by looking for
# biologically implausible pairs, which are adjacent height observations where
# the raw measure fell, or adjacent weight observations where the raw measure
# fell by more than 15%.
################################################################################

# Create a columns that will indicate if values are biologically implausible.
df[,raw_biv := as.numeric(NA)]
df[,stdz_biv := as.numeric(NA)]

# Go through all of the ID's to find biologically implausible pairs, and label
# one value in the pair as a biologically implausible value. To determine which
# of the two is the implausible value, look at the residuals, and the one with
# the larger absolute value residual is considered the implausible value. This
# process is repeated for both standardized and raw residuals. Also address both
# height and weight together in the same loop for each ID.
for(id in unique(df$subjid)) {
  # Find all the height observations for this ID.
  ht_idvar_locs <- which(df$subjid==id & df$param=="HEIGHTCM")

  # Only go through checking if there is
  if(length(ht_idvar_locs) > 1) {
    for(i in 1:(length(ht_idvar_locs)-1)) {
      # These two variables are just for readability later on.
      loc1 <- ht_idvar_locs[i]
      loc2 <- ht_idvar_locs[i+1]
      # Actually identifying if this pair is biologically implausible. Need to
      # make sure that there are non-null residuals to compare as well as
      # checking the value decrease.
      if(!is.na(df$raw_resid[loc1]) & !is.na(df$raw_resid[loc2]) &
         df$measurement[loc1] > df$measurement[loc2]) {
        # And find the residual with the larger absolute value.
        if(abs(df$raw_resid[loc1]) > abs(df$raw_resid[loc2])) {
          df[loc1, "raw_biv"] <- 1
        }
        else {
          df[loc2, "raw_biv"] <- 1
        }
      }
      # Repeat for the standardized residuals.
      if(!is.na(df$stdz_resid[loc1]) & !is.na(df$stdz_resid[loc2]) &
         df$measurement[loc1] > df$measurement[loc2]) {
        if(abs(df$stdz_resid[loc1]) > abs(df$stdz_resid[loc2])) {
          df[loc1, "stdz_biv"] <- 1
        }
        else {
          df[loc2, "stdz_biv"] <- 1
        }
      }
    }
  }

  # Repeat for weights.
  wt_idvar_locs <- which(df$subjid==id & df$param=="WEIGHTKG")

  if(length(wt_idvar_locs) > 1) {
    for(i in 1:(length(wt_idvar_locs)-1)) {
      loc1 <- wt_idvar_locs[i]
      loc2 <- wt_idvar_locs[i+1]
      if(!is.na(df$raw_resid[loc1]) & !is.na(df$raw_resid[loc2]) &
         df$measurement[loc2] < .85*df$measurement[loc1]) {
        if(abs(df$raw_resid[loc1]) > abs(df$raw_resid[loc2])) {
          df[loc1, "raw_biv"] <- 1
        }
        else {
          df[loc2, "raw_biv"] <- 1
        }
      }
      if(!is.na(df$stdz_resid[loc1]) & !is.na(df$stdz_resid[loc2]) &
           df$measurement[loc2] < .85*df$measurement[loc1]) {
        if(abs(df$stdz_resid[loc1]) > abs(df$stdz_resid[loc2])) {
          df[loc1, "stdz_biv"] <- 1
        }
        else {
          df[loc2, "stdz_biv"] <- 1
        }
      }
    }
  }
}
