# Implementing Breland, et al. (2017)
# By Hannah De los Santos
# Originated on: 10/28/2020

# paper: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5359156/

# implement breland, et al. ----

# method specific constants ----
# this includes specified cutoffs, etc.

# BIVs -- NOTE THESE ARE IN INCHES AND LBS
biv_df <- data.frame(
  "low" = c(48, 75),
  "high" = c(84, 700)
)
rownames(biv_df) <- c("height", "weight")

# begin implementation ----

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
  
  # convert all heights to inches
  subj_df$measurement <- subj_df$measurement/2.54

  # 1h, H BIV ----
  # 1h. remove biologically implausible height records
  step <- "1h, H BIV"

  criteria <- remove_biv(subj_df, "height", biv_df)
  subj_keep[criteria] <- "Implausible"
  subj_reason[criteria] <- paste0("Implausible, Step ",step)

  subj_df <- subj_df[!criteria,]

}

return(df)