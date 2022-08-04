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
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(miceadds)
library(broom)
library(lme4)
library(data.table)

################################################################################
# Import and clean data----
# At end data should be in three colummns: ID for
################################################################################

# Import synthetic data set for testing.

dat <- fread("C:/Users/molivier/Documents/Projects/CDC_CODI/Data_sets/gc-observations.csv")

# Filter out the data in several ways. Just keep weight data (since original
# algorithm uses weight data) and drop all the columns
df <- dat[param=="WEIGHTKG", c("subjid", "measurement", "sex",  "age_days")]

################################################################################
# Drop missing observations and sort data.----
################################################################################

df <- df[!(is.na(df$measurement) | is.na(df$age_days)),]
df <- df[order(df$subjid, df$age_days),]

# Generate a variable that indexes the visits for each subject.
df[, visit := seq_len(.N), by=subjid]
df[, visit := rowid(subjid)]

# Include cubic spline, though not sure what this is for

# Random effects model for unconditional mean weight by age (equation 1 from
# Appendix A)
model = lmer(measurement ~ (1 + age_days | subjid) + age_days, data=df, REML = FALSE)

# Get predicited value of weight for each subject at each time.
df$uncond_mean <- predict(model)

# Level 1 residual variance
resid_var <- summary(model)$sigma^2

# Pull out the variance/covariance matrix and the variances for the individual
# random effects.
vcov_mat <- VarCorr(model)$subjid
cons_var <- vcov_mat[2,2]
slope_var <- vcov_mat[1,1]
cons_slope_cov <- vcov_mat[1,2]

# Get the unconditional variance for each observation.
df$uncond_var <- cons_var + slope_var*df$age_days^2 +
  2*df$age_days*cons_slope_cov + resid_var

# Get conditional centiles.

# Store a value for "last minus 1", the second to last index in the data frame
# since it will be used a lot.
#Lm1 <- nrow(df)-1

# Covariance between successive observations. First get vector of lagged ages
# since it will be used a lot.
lag_age <- c(NA, df$age_days[1:nrow(df)-1])
df$cov12 = cons_var + lag_age*cons_slope_cov + lag_age*df$age_days*slope_var

# All the cases where visit is 1 are not valid, so make those NA.
df$cov12[df$visit==1] <- NA

# Get conditional mean (equation 3 from paper).
df$cond_mean <- df$uncond_mean +
              c(NA, (df$measurement-df$uncond_mean)[1:nrow(df)-1])*
              df$cov12/c(NA, df$uncond_var[1:nrow(df)-1])

# And conditional variance (equation 4 from paper).
df$cond_var <- df$uncond_var-(df$cov12^2/c(NA, df$uncond_var[1:nrow(df)-1]))

# Identifying outliers.
outliers <- (df$measurement > df$cond_mean+4*sqrt(df$cond_var)) |
  (df$measurement < df$cond_mean-4*sqrt(df$cond_var))
df$outlier <- "not outlier"
df$outlier[outliers] <- "outlier"
#df$outlier[df$visit==1] <- NA

# Remove second outlier if you have two outliers in succession. DO WE WANT THIS?
second_outlier <- df$outlier == "outlier" & c(NA, df$outlier[1:nrow(df)-1])=="outlier"

df$outlier[second_outlier] <- "not outlier"

# Quick check for number of second outliers. This is not part of algorithm.
sum(second_outlier, na.rm = TRUE)

table(second_outlier)

table(df$outlier)
