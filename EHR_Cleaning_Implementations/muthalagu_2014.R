# Implementing Muthalagu, et al. (2014)
# By Hannah De los Santos
# Originated on: 9/25/2020

# paper: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3974252/

# Note: will have to update to data table upon completion for speed

# load data and libraries ----

# fake data, for ease of coding purposes
set.seed(8)
num_subj <- 5
age_years <- seq(18, 68, by = 2.5)
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
    df$measurement[df$subjid == i]+ rnorm(sum(df$subjid == i))
}

# supporting functions ----

# overarching data ----

# this includes specified cutoffs, etc.
ht_cutoff_low <- 100
ht_cutoff_high <- 250

# implement muthalgu, et al. ----

# preallocate final designation
out <- c()
# go through each subject
for (i in unique(subjid)){
  # since we're going to do this a fair bit
  slog <- df$subjid == i
  
  subj_keep <- rep("Include", sum(slog))
  names(subj_keep) <- df$id[slog]
  
  subj_df <- df[slog,]
  
  # 1. remove biologically impossible height records
  too_low <- subj_df$measurement < ht_cutoff_low
  too_high <- subj_df$measurement > ht_cutoff_high
  subj_keep[too_low | too_high] <- "Remove"
  
  subj_df <- subj_df[!(too_low | too_high),]
  
}
