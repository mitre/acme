# Subset Synthea Data for Testing
# By Hannah De los Santos
# Originated on: 10/12/2020

# load data ----

#https://stackoverflow.com/questions/3452086/getting-path-of-an-r-script/35842176#35842176
# set working directory - only works in RStudio (with rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load data - fake for now, will get a better
dat <- read.csv("synthea-adults.csv")

# subset data ----

# number of subjects to output
num_output_subj <- 100

subj_sub <- unique(dat$subjid)[1:num_output_subj]

sub_dat <- dat[dat$subjid %in% subj_sub,]

# quick test ----

clean_df <- muthalagu_clean_ht(sub_dat)
clean_df <- cheng_clean_both(sub_dat)
clean_df <- chan_clean_both(sub_dat)

# write out ----

write.csv(sub_dat, 
          file = paste0("synthea-adults-sub-",num_output_subj,"subj.csv"),
          row.names = F, 
          na = "")
