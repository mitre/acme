# Implementing Daymont, et al. (growthcleanr-naive)
# By Hannah De los Santos
# Originated on: 10/30/2020

# paper: https://academic.oup.com/jamia/article/24/6/1080/3767271

# THIS IS GOING TO BE A MORE ITERATIVE IMPLEMENTATION

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

# implement growthcleanr-naive ----

# preallocate final designation
df$result <- "Include"
df$reason <- ""
rownames(df) <- df$id
# go through each subject
for (i in unique(df$subjid)){
  slog <- df$subjid == i
  
  # start with height ----
  h_df <- df[df$param == "HEIGHTCM" & slog,]
  
  subj_keep <- rep("Include", nrow(h_df))
  subj_reason <- rep("", nrow(h_df))
  names(subj_keep) <- names(subj_reason) <- h_df$id
  
  subj_df <- h_df
  
  # 1h, H calculate ewma ----
  # 1h. do some ewma calc
  step <- "1h, H calculate ewma"
  
  # convert age years to days
  subj_df$age_days <- subj_df$age_years*365.2425
  
  # sort by age
  subj_df <- subj_df[order(subj_df$age_days),]
  
  # calculate z-scores (based on the single person)
  if (nrow(subj_df) >= 3 & sd(subj_df$measurement) > 0){
    subj_df$z <- (subj_df$measurement - mean(subj_df$measurement))/
      sd(subj_df$measurement)
    
    # calculate ewma
    ewma_res <- ewma(subj_df$age_days, subj_df$z)
    
    # all three need to be beyond a cutoff for exclusion
    
    # exclude the most extreme, then recalculate again and again
    # this might have to be adjusted for adults
    dewma <- abs(ewma_res-subj_df$z)
    colnames(dewma) <- paste0("d",colnames(ewma_res))
    
    criteria <- 
      (dewma$dewma.all > 3.5 & 
         dewma$dewma.before > 3 & 
         dewma$dewma.after > 3 & 
         abs(subj_df$z) > 3.5)
    
    subj_keep[criteria] <- "Implausible"
    subj_reason[criteria] <- paste0("Implausible, Step ",step)
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
  # 1w. do some ewma calc
  step <- "1w, W calculate ewma"
  
  # convert age years to days
  subj_df$age_days <- subj_df$age_years*365.2425
  
  # sort by age
  subj_df <- subj_df[order(subj_df$age_days),]
  
  # calculate z-scores (based on the single person)
  if (nrow(subj_df) >= 3 & sd(subj_df$measurement) > 0){
    subj_df$z <- (subj_df$measurement - mean(subj_df$measurement))/
      sd(subj_df$measurement)
    
    # calculate ewma
    ewma_res <- ewma(subj_df$age_days, subj_df$z)
    
    # all three need to be beyond a cutoff for exclusion
    
    # exclude the most extreme, then recalculate again and again
    # this might have to be adjusted for adults
    dewma <- abs(ewma_res-subj_df$z)
    colnames(dewma) <- paste0("d",colnames(ewma_res))
    
    criteria <- 
      (dewma$dewma.all > 3.5 & 
         dewma$dewma.before > 3 & 
         dewma$dewma.after > 3 & 
         abs(subj_df$z) > 3.5)
    
    subj_keep[criteria] <- "Implausible"
    subj_reason[criteria] <- paste0("Implausible, Step ",step)
  }
  
  # add results to full dataframe
  df[names(subj_keep), "result"] <- subj_keep
  df[names(subj_reason), "reason"] <- subj_reason
}