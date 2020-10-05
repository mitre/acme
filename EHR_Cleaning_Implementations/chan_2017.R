# Implementing Chan, et al. (2017)
# By Hannah De los Santos
# Originated on: 10/5/2020

# paper: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5359164/

# Note: will have to update to data table upon completion for speed

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
