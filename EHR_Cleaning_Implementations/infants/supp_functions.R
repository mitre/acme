# Supplemental Functions for Implementing Infants EHR Cleaning
# By Hannah De los Santos, Max Olivier
# Originated on: 8/23/2022

# required libraries ----

library(data.table)

# functions for all ----

# based on read_anthro (growthcleanr): function to calculate z-scores and 
# csd-scores based on anthro tables.
#
# inputs:
#   param: HEIGHTCM or WEIGHTKG
#   agedays: age, in days
#   sex: sex of subject
#   measurement: height or weight measurement
#   csd: include csd value
# outputs: returns vector of standardized infant scores
get_standardized_infant_scores <- function(param, agedays, sex, measurement,
                                           csd = F) {
  # Pull in the needed files for the data.
  base_path <- file.path("EHR_Cleaning_Implementations", "infants",
                         "supporting_data")
  
  weianthro_path <- file.path(base_path, "weianthro.txt")
  lenanthro_path <- file.path(base_path, "lenanthro.txt")
  bmianthro_path <- file.path(base_path, "bmianthro.txt")
  
  # Create a list with all the data tables.
  l <- list(
    with(
      read.table(weianthro_path, header = T),
      data.frame(
        src = 'WHO',
        param = 'WEIGHTKG',
        sex = sex - 1,
        age,
        l,
        m,
        s,
        csdpos = as.double(NA),
        csdneg = as.double(NA)
      )
    ),
    with(
      read.table(lenanthro_path, header = T),
      data.frame(
        src = 'WHO',
        param = 'HEIGHTCM',
        sex = sex - 1,
        age,
        l,
        m,
        s,
        csdpos = as.double(NA),
        csdneg = as.double(NA)
      )
    ),
    with(
      read.table(bmianthro_path, header = T),
      data.frame(
        src = 'WHO',
        param = 'BMI',
        sex = sex - 1,
        age,
        l,
        m,
        s,
        csdpos = as.double(NA),
        csdneg = as.double(NA)
      )
    )
  )
  
  # Bind everything into
  anthro <- rbindlist(l)
  setkey(anthro, src, param, sex, age)
  
  # Set the source.
  src <- "WHO"
  
  # keep column sequence the same fo efficient join
  dt <- data.table(src, param, sex, agedays, measurement)
  dt <- anthro[dt] # HOW DOES THIS WORK!!
  
  dt[, ret := as.double(NA)]
  
  if (csd) {
    dt[measurement < m, ret := (measurement - m) / csdneg]
    dt[measurement >= m, ret := (measurement - m) / csdpos]
  } else {
    dt[l == 0, ret := log(measurement / m) / s]
    dt[l != 0, ret := (((measurement / m) ^ l) - 1) / (l * s)]
  }
  return(dt$ret)
}
