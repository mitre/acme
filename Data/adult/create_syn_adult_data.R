# Creating Synthetic Data for Adult Growthcleanr
# By Hannah De los Santos
# Originated on: 10/7/2020

# set libraries ----

#https://stackoverflow.com/questions/3452086/getting-path-of-an-r-script/35842176#35842176
# set working directory - only works in RStudio (with rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# create data ----

# fake data, for ease of coding purposes
seed <- 8
set.seed(seed)

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
  # jitter height - normal distribution
  df$measurement[df$subjid == i & df$param == "HEIGHTCM"] <- 
    df$measurement[df$subjid == i & df$param == "HEIGHTCM"]+ 
    rnorm(sum(df$subjid == i & df$param == "HEIGHTCM"), 0, 2)
  
  # jitter weight - normal distribution
  df$measurement[df$subjid == i & df$param == "WEIGHTKG"] <- 
    df$measurement[df$subjid == i & df$param == "WEIGHTKG"]+ 
    rnorm(sum(df$subjid == i & df$param == "WEIGHTKG"), 0, 5)
}

# write out data ----

write.csv(df, 
          file = paste0("adult_synthetic_data_seed_", seed, ".csv"),
          row.names = F,
          na = "")
