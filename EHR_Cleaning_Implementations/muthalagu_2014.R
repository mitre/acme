# Implementing Muthalagu, et al. (2014)
# By Hannah De los Santos
# Originated on: 9/25/2020

# paper: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3974252/

# Note: will have to update to data table upon completion for speed

# load data ----

# fake data, for ease of coding purposes
set.seed(8)
num_subj <- 5
age_years <- seq(18, 68, by = .8)
min_height <- 4*30.48 # 4 ft
max_height <- 8*30.48 # 8 ft
df <- data.frame(
  "id" = 1:(length(age_years)*num_subj),
  "subjid" = rep(1:num_subj, each = length(age_years)),
  "sex" = rep(sample(c(0,1), num_subj, replace = T), each = length(age_years)),
  "age_years" = rep(age_years, num_subj),
  "param" = "HEIGHTCM",
  "measurement" = rep(runif(num_subj, min = min_height, max = max_height),
                      each = length(age_years))
)
# jitter each of the measurements for interest
for (i in 1:num_subj){
  df$measurement[df$subjid == i] <- 
    df$measurement[df$subjid == i]+ rnorm(sum(df$subjid == i), 0, 2)
}

# implement muthalagu, et al. ----

# function to clean height data by muthalagu, et al.
# inputs:
# df: data frame with 7 columns:
#   id: row id
#   subjid: subject id
#   sex: sex of subject
#   age_years: age, in years
#   param: HEIGHTCM or WEIGHTKG
#   measurement: height or weight measurement
# outputs:
#   df, with additional column result, which specifies whether the height measurement
#     should be included, is erroneous, or indeterminate. "unknown" for weight measurements
muthalagu_clean_ht <- function(df){
  # method specific constants ----
  # this includes specified cutoffs, etc.
  
  # BIVs
  ht_cutoff_low <- 100
  ht_cutoff_high <- 250
  
  # in a given range, the cutoff between the max and min height
  # NOTE: this should be customizable
  btwn_range_cutoff <- 3.5
  
  # age buckets
  age_ranges <- data.frame(
    "low" = c(18, 25, 50),
    "high" = c(25, 50, Inf)
  )
  
  # begin implementation ----
  
  h_df <- df[df$param == "HEIGHTCM",]
  
  # preallocate final designation
  out <- c()
  # go through each subject
  for (i in unique(h_df$subjid)){
    # since we're going to do this a fair bit
    slog <- h_df$subjid == i
    
    subj_keep <- rep("Include", sum(slog))
    names(subj_keep) <- h_df$id[slog]
    
    subj_df <- h_df[slog,]
    
    # 1 ----
    # 1. remove biologically impossible height records
    too_low <- subj_df$measurement < ht_cutoff_low
    too_high <- subj_df$measurement > ht_cutoff_high
    subj_keep[too_low | too_high] <- "Erroneous"
    
    subj_df <- subj_df[!(too_low | too_high),]
    
    # 2 ----
    # 2. Go through each age bucket
    for (ab in 1:nrow(age_ranges)){
      # 2a ----
      # 2a: if max - min height < 3.5, plausible
      subj_df_age <- subj_df[subj_df$age_years >= age_ranges$low[ab] &
                               subj_df$age_years < age_ranges$high[ab],]
      
      # if there's nothing in this age range, there's nothing to consider
      # NOTE: using next to avoid further nesting
      if (nrow(subj_df_age) == 0){
        next
      }
      
      if (abs(max(subj_df_age$measurement) - min(subj_df_age$measurement)) >= 
          btwn_range_cutoff){
        # 2b ----
        # 2b: if not in range, calculate median height at each age. compare with 
        # prior and next median. if height at current age differs by > 3.5 prior
        # and next median, flag as potentially erroneous
        # if only 2 valid medians and differ by >3.5, flag both as indeterminate
        
        # if there are only two values, everything will be indeterminate
        if (nrow(subj_df_age) <= 2){
          subj_keep[as.character(subj_df_age$id)] <- "Indeterminate"
        } else {
          # calculate median heights per age (rounded to nearest year) in bucket
          med_hts <- sapply(round(unique(subj_df_age$age_years)), function(x){
            median(subj_df_age$measurement[round(subj_df_age$age_years) == x])
          })
          
          # compare only for middle values -- end values are indeterminate
          # results in true if both are under the cutoff
          mid_compare <- sapply(2:(length(med_hts)-1), function(x){
            !all(c(abs(med_hts[x] - med_hts[x-1]), abs(med_hts[x] - med_hts[x+1])) >= 
                   btwn_range_cutoff)
          })
          
          # if true, correct height, else, erroneous
          # also include indeterminate (first and last)
          err_hts_idx <- c(1, 
                           if(!all(mid_compare)){which(!mid_compare)+1}, 
                           length(med_hts))
          
          # 2c ----
          # 2c: For erroneous and indeterminate medians, assign algorithms within
          # 3 year period. Then compare all other recorded heights to the median at 
          # that are. If the recorded height for any age differs  > 3.5 (for 
          # erroneous) or > 6 (for indeterminate) from cleaned median height for 
          # that age, the value is erroneous.
          
          # if there's no correct median height for comparison, it's all indeterminate
          if (sum(mid_compare) == 0){
            subj_keep[as.character(subj_df_age$id[err_hts_idx])] <- "Indeterminate"
          } else {
            chg_err_med <- sapply(err_hts_idx, function(x){
              # get all the correct medians within a 3 year window
              sub_df <- subj_df_age[-err_hts_idx,] # do not incorrect ages
              avail_med <- 
                med_hts[-err_hts_idx][sub_df$age_years <= (sub_df$age_years[x]+1.5) |
                                        sub_df$age_years >= (sub_df$age_years[x]-1.5)]
              
              # assign nearest correct median
              new_med <- avail_med[which.min(abs(avail_med - med_hts[x]))]
              
              # compare cleaned median to all recorded heights
              compare <- abs(subj_df_age$measurement[-x]-new_med)
              # for end points, they have a wider range
              if (x == 1 | x == length(med_hts)){
                if(any(compare > 6)){
                  return("Erroneous")
                } else {
                  return("Include")
                }
              } else {
                if(any(compare >= btwn_range_cutoff)){
                  return("Erroneous")
                } else {
                  return("Include")
                }
              }
              
            })
            
            # adjust the errored/indeterminate heights accordingly
            subj_keep[as.character(subj_df_age$id[err_hts_idx])] <- chg_err_med
          }
        }
      }
    }
    
    # record results
    out <- c(out, subj_keep)
  }
  
  # add results to overall data frame
  h_df <- cbind(h_df, "result" = out)
  
  # default to unknown for weight values
  df <- cbind(df, "result" = "Unknown")
  rownames(df) <- df$id
  df[as.character(h_df$id), "result"] <- h_df$result
  
  
  return(df)
}

clean_df <- muthalagu_clean_ht(df)
