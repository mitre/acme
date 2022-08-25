################################################################################
# yang_hutcheon_2015.R: Implementation of paper to compare data cleaning
# algorithms for infant data. Original paper is by Seungmi Yang and Jennifer
# Hutcheon and uses conditional growth percentiles "to systematically identify
# implausible measurements in growth trajectory data". The conditional growth
# percentiles are computed using a random effects model. The algorithm in the
# original paper is used to clean data for children from birth to 6.5 years old.
# The code below is an R implementation of the author's original Stata code
# (contained in Appendix A) of the paper. The original paper is available at:
# https://www.sciencedirect.com/science/article/pii/S1047279715004184?viewFullText=true
#
# @author = Max Olivier
################################################################################

# Import necessary libraries
library(lme4)

# function to clean weight data by yang, et al.
# inputs:
# df: data frame with 7 columns:
#   id: row id, must be unique
#   subjid: subject id
#   sex: sex of subject
#   age_days: age, in days
#   param: HEIGHTCM or WEIGHTKG
#   measurement: height or weight measurement
# inter_vals: boolean, return intermediate values
# outputs:
#   df, with additional columns:
#     result, which specifies whether the height measurement should be included,
#       or is implausible.
#     reason, which specifies, for implausible values, the reason for exclusion,
#       and the step at which exclusion occurred.
#     intermediate value columns, if specified
yang_clean_both <- function(df, inter_vals = F){
  
  # method specific constants ----
  
  # same step for both height and weight (originally in weight)
  
  inter_cols <- c(
    "Step_1w_W_Unconditional_Mean",
    "Step_1w_W_Unconditional_Variance", 
    "Step_1w_W_Covariance", 
    "Step_1w_W_Conditional_Mean", 
    "Step_1w_W_Conditional_Variance", 
    "Step_1w_W_4_SD", 
    "Step_1w_W_Mean_plus_4_SD", 
    "Step_1w_W_Mean_minus_4_SD",
    "Step_1w_Result",
    "Step_1h_H_Unconditional_Mean",
    "Step_1h_H_Unconditional_Variance", 
    "Step_1h_H_Covariance", 
    "Step_1h_H_Conditional_Mean", 
    "Step_1h_H_Conditional_Variance", 
    "Step_1h_H_4_SD", 
    "Step_1h_H_Mean_plus_4_SD", 
    "Step_1h_H_Mean_minus_4_SD",
    "Step_1h_Result"
  )
  
  # begin implementation ----
  
  # preallocate final designation
  df$result <- "Include" 
  df$reason <- ""
  rownames(df) <- df$id
  # if using intermediate values, preallocate values
  if (inter_vals){
    df[, inter_cols] <- NA
  }
  
  # if using intermediate values, we want to start storing them
  # keep the ID to collate with the final dataframe
  inter_df <- df[, "id", drop = F]
  rownames(inter_df) <- inter_df$id
  
  # iterate through height and weight (steps are the same)
  for (type in c("w", "h")){
    # in order not to add additional columns to output (unless called for),
    # we create a dataframe to use for computation
    subj_df <- df
    
    # 1w/h, W/H conditional growth percentiles ----
    # 1w/h: calculate conditional growth percentiles using a random effects model,
    # using conditional mean weights/heights and 4 SD range for an individual's 
    # weight/ height. if weight/height measurement is >+/- 4SD, classify as outlier.
    type_upper <- toupper(type)
    step <- paste0("1", type, ", ", type_upper,
                   " conditional growth percentiles")
    
    # filter down to weights or heights only
    subj_df <- 
      if (type == "w"){
        subj_df[subj_df$param == "WEIGHTKG",]
      } else {
        subj_df[subj_df$param == "HEIGHTCM",]
      }
    
    # Drop missing observations and sort data
    subj_df <- subj_df[!(is.na(subj_df$measurement) | is.na(subj_df$age_days)),]
    subj_df <- subj_df[order(subj_df$subjid, subj_df$age_days),]
    
    # Generate a variable that indexes the visits for each subject.
    subj_df$visit <- ave(1:nrow(subj_df), subj_df$subjid, FUN = seq_along)
    
    # Random effects model for unconditional mean weight by age (equation 1 from
    # Appendix A)
    model <- suppressWarnings(
      lmer(measurement ~ (1 + age_days | subjid) + age_days, 
           data = subj_df, 
           REML = FALSE)
    )
    
    # Get predicited value of weight for each subject at each time.
    subj_df$uncond_mean <- predict(model)
    
    # Level 1 residual variance
    resid_var <- summary(model)$sigma^2
    
    # Pull out the variance/covariance matrix and the variances for the individual
    # random effects.
    vcov_mat <- VarCorr(model)$subjid
    cons_var <- vcov_mat[2,2]
    slope_var <- vcov_mat[1,1]
    cons_slope_cov <- vcov_mat[1,2]
    
    # Get the unconditional variance for each observation.
    subj_df$uncond_var <- cons_var + slope_var*subj_df$age_days^2 +
      2*subj_df$age_days*cons_slope_cov + resid_var
    
    # Get conditional centiles
    
    # Covariance between successive observations. First get vector of lagged ages
    # since it will be used a lot.
    lag_age <- c(NA, subj_df$age_days[1:nrow(subj_df)-1])
    subj_df$cov12 <- cons_var + lag_age*cons_slope_cov + 
      lag_age*subj_df$age_days*slope_var
    
    # All the cases where visit is 1 are not valid, so make those NA.
    subj_df$cov12[subj_df$visit==1] <- NA
    
    # Get conditional mean (equation 3 from paper).
    subj_df$cond_mean <- subj_df$uncond_mean +
      c(NA, (subj_df$measurement-subj_df$uncond_mean)[1:nrow(subj_df)-1])*
      subj_df$cov12/c(NA, subj_df$uncond_var[1:nrow(subj_df)-1])
    
    # And conditional variance (equation 4 from paper).
    subj_df$cond_var <- subj_df$uncond_var-
      (subj_df$cov12^2/c(NA, subj_df$uncond_var[1:nrow(subj_df)-1]))
    
    # Identifying outliers.
    outliers <- (subj_df$measurement > subj_df$cond_mean+4*sqrt(subj_df$cond_var)) |
      (subj_df$measurement < subj_df$cond_mean-4*sqrt(subj_df$cond_var))
    subj_df$result[outliers] <- "Implausible"
    subj_df$reason[outliers] <- paste0("Outlier, Step ", step)
    
    # Remove second outlier if you have two outliers in succession.
    second_outlier <- subj_df$outlier == "Implausible" & 
      c(NA, subj_df$result[1:nrow(subj_df)-1])=="Implausible"
    
    subj_df$result[second_outlier] <- "Include"
    subj_df$reason[second_outlier] <- ""
    
    # add intermediate values
    if (inter_vals){
      step_name <- paste0("Step_1", type, "_", type_upper, "_")
      
      inter_df[as.character(subj_df$id), 
               paste0(step_name, "Unconditional_Mean")] <-
        subj_df$uncond_mean
      inter_df[as.character(subj_df$id), 
               paste0(step_name, "Unconditional_Variance")] <-
        subj_df$uncond_var
      inter_df[as.character(subj_df$id), paste0(step_name, "Covariance")] <-
        subj_df$cov12
      inter_df[as.character(subj_df$id), 
               paste0(step_name, "Conditional_Mean")] <-
        subj_df$cond_mean
      inter_df[as.character(subj_df$id), 
               paste0(step_name, "Conditional_Variance")] <-
        subj_df$cond_var
      inter_df[as.character(subj_df$id), paste0(step_name, "4_SD")] <-
        4*sqrt(subj_df$cond_var)
      inter_df[as.character(subj_df$id), paste0(step_name, "Mean_plus_4_SD")] <-
        subj_df$cond_mean+4*sqrt(subj_df$cond_var)
      inter_df[as.character(subj_df$id), paste0(step_name, "Mean_minus_4_SD")] <-
        subj_df$cond_mean-4*sqrt(subj_df$cond_var)
      inter_df[as.character(subj_df$id), paste0("Step_1", type, "_Result")] <- F
      inter_df[as.character(subj_df$id)[subj_df$result == "Implausible"], 
               paste0("Step_1", type, "_Result")] <- T
    }
    
    # add results to full dataframe
    df[rownames(subj_df), c("result", "reason")] <- subj_df[, c("result", "reason")]
    
    # if we're using intermediate values, we want to save them
    if (inter_vals){
      df[as.character(inter_df$id), colnames(inter_df)[-1]] <- inter_df[,-1]
    }
  }
  
  return(df)
}


