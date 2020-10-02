# Implementing Cheng, et al. (2016)
# By Hannah De los Santos
# Originated on: 10/1/2020

# paper: https://onlinelibrary.wiley.com/doi/full/10.1002/oby.21612

# Note: will have to update to data table upon completion for speed
# Note 2: must remove missing values before running method

# load data ----

# fake data, for ease of coding purposes
set.seed(8)
num_subj <- 5
age_years <- seq(18, 68, by = .8)
min_height <- 4*30.48 # 4 ft
max_height <- 8*30.48 # 8 ft
min_weight <- 55*.4535 # 55 lbs
max_weight <- 1000*.4535 # 1000 lbs
df <- data.frame(
  "id" = 1:(length(age_years)*num_subj*2),
  "subjid" = rep(1:num_subj, each = length(age_years)*2),
  "sex" = rep(sample(c(0,1), num_subj, replace = T), each = length(age_years)*2),
  "age_years" = rep(rep(age_years,2), num_subj),
  "param" = rep(rep(c("HEIGHTCM", "WEIGHTKG"), each = length(age_years)), num_subj),
  "measurement" = c(
    rep(
      c(rbind(runif(num_subj, min = min_height, max = max_height),
            runif(num_subj, min = min_weight, max = max_weight))), 
        each = length(age_years))
  )
)
# jitter each of the measurements for interest
for (i in 1:num_subj){
  # jitter height
  df$measurement[df$subjid == i & df$param == "HEIGHTCM"] <- 
    df$measurement[df$subjid == i & df$param == "HEIGHTCM"]+ 
    rnorm(sum(df$subjid == i & df$param == "HEIGHTCM"), 0, 2)
  
  # jitter weight
  df$measurement[df$subjid == i & df$param == "WEIGHTKG"] <- 
    df$measurement[df$subjid == i & df$param == "WEIGHTKG"]+ 
    rnorm(sum(df$subjid == i & df$param == "WEIGHTKG"), 0, 5)
}

# supporting functions ----

remove_biv <- function(subj_df, type, biv_df){
  too_low <- subj_df$measurement < biv_df[type, "low"]
  too_high <- subj_df$measurement > biv_df[type, "high"]
  
  return(too_low | too_high)
}

# implement cheng, et al. ----

# function to clean height and weight data by cheng, et al.
# inputs:
# df: data frame with 7 columns:
#   id: row id
#   subjid: subject id
#   sex: sex of subject
#   age_years: age, in years
#   param: HEIGHTCM or WEIGHTKG
#   measurement: height or weight measurement
# outputs:
#   df, with additional column "result", which specifies whether the height 
#     measurement should be included or implausible.
cheng_clean_both <- function(df){
  # method specific constants ----
  # this includes specified cutoffs, etc.
  
  # BIVs
  biv_df <- data.frame(
    "low" = c(111.8, 24.9, 12),
    "high" = c(228.6, 453.6, 70)
  )
  rownames(biv_df) <- c("height", "weight", "bmi")
  
  # begin implementation ----
  
  # preallocate final designation
  df$result <- "Include"
  rownames(df) <- df$id
  # go through each subject
  for (i in unique(df$subjid)){
    slog <- df$subjid == i
    
    # start with height ----
    
    h_df <- df[df$param == "HEIGHTCM" & slog,]
    
    subj_keep <- rep("Include", nrow(h_df))
    names(subj_keep) <- h_df$id
    
    subj_df <- h_df
    
    # 1h ----
    # 1h. remove biologically impossible height records
    criteria <- remove_biv(subj_df, "height", biv_df)
    subj_keep[criteria] <- "Implausible"
    
    subj_df <- subj_df[!criteria,]
    
    if (nrow(subj_df) > 0){
      #2h ----
      # 2h. Exclude height if a) absolute difference between that height and average
      # height > standard deviation (SD) AND b) SD > 2.5% of average height.
      
      # calculate standard deviation of height for a given subject
      st_dev <- sd(subj_df$measurement)
      avg <- mean(subj_df$measurement)
      
      # if the standard deviation is > than 2.5%, we can say something about height
      if (st_dev/avg > .025){
        # criteria (a)
        excl_ht <- sapply(subj_df$measurement, function(x){abs(x-avg) > st_dev})
        
        subj_keep[as.character(subj_df$id[excl_ht])] <- "Implausible"
      }
    }
    
    h_df$result <- subj_keep
    
    # add results to full dataframe
    df[as.character(h_df$id), "result"] <- h_df$result
    
    # then do weight ----
    
    w_df <- df[df$param == "WEIGHTKG" & slog,]
    
    subj_keep <- rep("Include", nrow(w_df))
    names(subj_keep) <- w_df$id
    
    subj_df <- w_df
    
    # 1w ----
    # 1w. remove biologically impossible weight records
    criteria <- remove_biv(subj_df, "weight", biv_df)
    subj_keep[criteria] <- "Implausible"
    
    subj_df <- subj_df[!criteria,]
    
    if (nrow(subj_df) > 0){
      # 2w ----
      # 2w. weight inaccurate if:
      # a) the range was > 22.7 kg AND absolute difference between recorded weight 
      # and avg weight was > 70% of range
      # OR
      # b) SD was >20% of the average weight AND absolute difference between that 
      # weight and average weight > the SD
      
      avg_w <- mean(subj_df$measurement)
      
      # first calculate criteria a)
      w_range <- abs(max(subj_df$measurement) - min(subj_df$measurement))
      # if range is > 22.7, we can possibly say something about weight
      criteria_a <- 
        if (w_range > 22.7){
          # weight difference is more than 70% of range
          sapply(subj_df$measurement, function(x){
            abs(x - avg_w) > .7*w_range
          })
        } else {
          rep(F, nrow(subj_df))
        }
      
      # criteria b)
      st_dev <- sd(subj_df$measurement)
      # if sd was >20% of avg weight, we can possibly say something about weight
      criteria_b <- 
        if (st_dev/avg_w > .2){
          # weight difference is > SD
          sapply(subj_df$measurement, function(x){
            abs(x - avg_w) > st_dev
          })
        } else {
          rep(F, nrow(subj_df))
        }
      
      subj_keep[as.character(subj_df$id[criteria_a | criteria_b])] <- "Implausible"
    }
    
    w_df$result <- subj_keep
    
    # add results to full dataframe
    df[as.character(w_df$id), "result"] <- w_df$result
    
    # 3 ----
    # 3. If BMI for a given set of height/weights is < 12 or > 70, deem implausible
    
    # possible removal of height/weights by bmi
    comb_df <- data.frame(
      "ht" = h_df$measurement,
      "ht_res" = h_df$result,
      "wt" = w_df$measurement,
      "wt_res" = w_df$result
    )
    
    
  }
  
  return(df)
}
