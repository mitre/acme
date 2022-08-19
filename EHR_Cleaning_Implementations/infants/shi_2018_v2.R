# Loop through all ID's and if/else statements for each of 4 regressions:
# height, weight, and standardized verison of each

ptm <- proc.time()
# Loop through all of the potential ID's.
for(id in unique(df$subjid)) {

  # First deal with the regressions for height.
  # Get lists of all the valid height observations for this ID, both for
  # standardized and raw measures. It should be the case that
  height_obs_stdz <- df$subjid==id & df$param=="HEIGHTCM" & !is.na(df$stdz_meas)
  height_obs_raw <- df$subjid==id & df$param=="HEIGHTCM" &
    !is.na(df$measurement)

  # Check if there are enough observations to run the regression do the and if ther are, run the regression.
  if(sum(height_obs_stdz) > 3) {
    reg_height_stdz <- lm(stdz_meas ~ age_days, data=df[height_obs_stdz,])
    # Built in R function for jackknife residuals
    jkres_height_stdz <- rstudent(reg_height_stdz)
    df[height_obs_stdz, "std_resid"] <- jkres_height_stdz
  }

  if(sum(height_obs_raw) > 3) {
    reg_height_raw <- lm(measurement ~ I(sqrt(age_days)),
                         data=df[height_obs_raw,])
    # Built in R function for jackknife residuals
    jkres_height_raw <- rstudent(reg_height_raw)
    df[height_obs_raw, "raw_resid"] <- jkres_height_raw
  }


  # Regressions for weight.
  weight_obs_stdz <- df$subjid==id & df$param=="WEIGHTKG" & !is.na(df$stdz_meas)
  weight_obs_raw <- df$subjid==id & df$param=="WEIGHTKG" &
    !is.na(df$measurement)

  # Check if there are enough observations to run the regression do the and if ther are, run the regression.
  if(sum(weight_obs_stdz) > 3) {
    reg_weight_stdz <- lm(stdz_meas ~ age_days, data=df[weight_obs_stdz,])
    # Built in R function for jackknife residuals
    jkres_weight_stdz <- rstudent(reg_weight_stdz)
    df[weight_obs_stdz, "std_resid"] <- jkres_weight_stdz
  }

  if(sum(weight_obs_raw) > 3) {
    reg_weight_raw <- lm(measurement ~ I(sqrt(age_days)),
                         data=df[weight_obs_raw,])
    # Built in R function for jackknife residuals
    jkres_weight_raw <- rstudent(reg_weight_raw)
    df[weight_obs_raw, "raw_resid"] <- jkres_weight_raw
  }
}
proc.time()-ptm
