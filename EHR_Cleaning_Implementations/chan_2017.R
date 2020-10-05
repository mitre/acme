# Implementing Chan, et al. (2017)
# By Hannah De los Santos
# Originated on: 10/5/2020

# paper: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5359164/

# Note: will have to update to data table upon completion for speed

# add data ----

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

# implement chan, et al. ----

# method specific constants ----
# this includes specified cutoffs, etc.

# BIVs
biv_df <- data.frame(
  "low" = c(48*2.54, 22.7, 10),
  "high" = c(84*2.54, 340.2, 100)
)
rownames(biv_df) <- c("height", "weight", "bmi")

# begin implementation ----


# preallocate final designation
df$result <- "Include"
rownames(df) <- df$id
# go through each subject
for (i in unique(df$subjid)){
  slog <- df$subjid == i
  
  # start with height
  h_df <- df[df$param == "HEIGHTCM" & slog,]
  
  subj_keep <- rep("Include", nrow(h_df))
  names(subj_keep) <- h_df$id
  
  subj_df <- h_df
  
  # 1h ----
  # 1h. remove biologically impossible height records
  criteria <- remove_biv(subj_df, "height", biv_df)
  subj_keep[criteria] <- "Implausible"
  
  subj_df <- subj_df[!criteria,]
  
  # 2h ----
  # 2h. Exclude heights that were greater than 3 standard deviations from the 
  # mean.
  
  if (nrow(subj_df) > 0){
    # calculate mean and standard deviation
    avg_ht <- mean(subj_df$measurement)
    st_dev_ht <- sd(subj_df$measurement)
    
    # calculate exclusion criteria
    criteria <- abs(subj_df$measurement - avg_ht) > 3*st_dev_ht
    
    subj_keep[as.character(subj_df$id)][criteria] <- "Implausible"
    
    # add the full calculation
    h_df$result <- subj_keep
  }
  
}
