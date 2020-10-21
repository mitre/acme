# Implementing Littman, et al. (2012)
# By Hannah De los Santos
# Originated on: 10/21/2020

# paper: https://www.cdc.gov/pcd/issues/2012/11_0267.htm

# implement littman, et al. ----

# method specific constants ----
# this includes specified cutoffs, etc.

# BIVs
biv_df <- data.frame(
  "low" = c(49*2.54, 75*.453592, 0),
  "high" = c(94*2.54, 600*.453592, 80)
)
rownames(biv_df) <- c("height", "weight", "bmi")

# begin implementation ----

# preallocate final designation
df$result <- "Include"
df$reason <- ""
rownames(df) <- df$id
# go through each subject
for (i in unique(df$subjid)){
  slog <- df$subjid == i
  
  # start by removing BIVs ----
  
  # get weight and height data
  h_df <- df[df$param == "HEIGHTCM" & slog,]
  w_df <- df[df$param == "WEIGHTKG" & slog,]
  
  h_subj_keep <- rep("Include", nrow(h_df))
  h_subj_reason <- rep("", nrow(h_df))
  names(h_subj_keep) <- names(h_subj_reason) <- h_df$id
  
  h_subj_df <- h_df
  
  w_subj_keep <- rep("Include", nrow(w_df))
  w_subj_reason <- rep("", nrow(w_df))
  names(w_subj_keep) <- names(w_subj_reason) <- w_df$id
  
  w_subj_df <- w_df
  
  # 1h, H BIV ----
  # 1h. remove biologically impossible height records
  step <- "1h, H BIV"
  
  criteria <- remove_biv(h_subj_df, "height", biv_df)
  h_subj_keep[criteria] <- "Implausible"
  h_subj_reason[criteria] <- paste0("Erroneous, Step ",step)
  
  h_subj_df <- h_subj_df[!criteria,]
  
  # 1w, W BIV ----
  # 1w. remove biologically impossible weight records
  step <- "1w, W BIV"
  
  criteria <- remove_biv(w_subj_df, "weight", biv_df)
  w_subj_keep[criteria] <- "Implausible"
  w_subj_reason[criteria] <- paste0("Erroneous, Step ",step)
  
  w_subj_df <- w_subj_df[!criteria,]
  
  # 1bmi, BMI BIV ----
  # 1bmi. remove biologically implausible bmi values for each set of height
  # /weights
  step <- "1bmi, BMI BIV"
  
  # possible removal of height/weights by bmi
  # x = height, y = weight
  comb_df <- merge(h_df, w_df, by = "age_years", all = T)
  # remove ones that don't match
  comb_df <- comb_df[complete.cases(comb_df),]
  # also remove ones that are not plausible
  comb_df <- comb_df[comb_df$result.x == "Include" & comb_df$result.y == "Include",]
  
  if (nrow(comb_df) > 0){
    # calculate bmi
    comb_df$measurement <- comb_df$measurement.y/((comb_df$measurement.x/100)^2)
    
    bmi_biv <- remove_biv(comb_df, "bmi", biv_df)
    
    # add result to interim df
    comb_df$tot_res <- "Include"
    comb_df$tot_res[bmi_biv] <- "Implausible"
    comb_df$tot_reason <- ""
    comb_df$tot_reason[bmi_biv] <- paste0("Erroneous, Step ",step)
    
    # add result to height and weight data
    w_subj_keep[as.character(comb_df$id.y)] <- 
      h_subj_keep[as.character(comb_df$id.x)] <- comb_df$tot_res
    w_subj_reason[as.character(comb_df$id.y)] <- 
      h_subj_reason[as.character(comb_df$id.x)] <- comb_df$tot_reason
    
    # subset height and weight data
    rownames(w_subj_df) <- w_subj_df$id
    w_subj_df[as.character(comb_df$id.y),] <- 
      w_subj_df[as.character(comb_df$id.y),][comb_df$tot_res == "Include",]
    rownames(h_subj_df) <- h_subj_df$id
    h_subj_df[as.character(comb_df$id.x),] <- 
      h_subj_df[as.character(comb_df$id.x),][comb_df$tot_res == "Include",]
  }
  
  # 2, remove erroneous values based on SD ----
  # 2. Remove erroneous values that have large standard deviations.
  
  # 2w, W compare difference from average to SD ----
  # 2w. Exclude any weight measurements where: 1) difference between mean weight
  # and recorded weight was greater than the standard deviation (SD) AND 2) the 
  # SD was greater than 10% of the mean.
  step <- "2w, W compare difference from average to SD"
  
  avg_w <- mean(w_subj_df$measurement)
  st_dev_w <- sd(w_subj_df$measurement)
  
  # if the SD is greater than 10% of the mean, we can evaluate the record
  if (st_dev_w/avg_w > .1){
    # criteria to exclude the record
    criteria <- sapply(w_subj_df$measurement, function(x){
      abs(x-avg_w) > st_dev_w
    })
  }
  
  w_subj_keep[w_subj_df$id[criteria]] <- "Implausible"
  w_subj_reason[w_subj_df$id[criteria]] <- paste0("Erroneous, Step ", step)
  w_subj_keep <- w_subj_df[!criteria,]
  
  
  
}
