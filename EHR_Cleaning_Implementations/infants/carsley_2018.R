################################################################################
# carsley_2018.R: Implementation of paper to compare data cleaning algorithms
# for infant data. Original paper is by Carsley et al. and identifies what they
# call "inaccurate measurements" in one of two ways. The first way is just
# measurements that have Z-scores outside a certain range (which differs
# depending on measurement type). The second is their "invalid inlier rule",
# which applies to those z-scores that are greater than or equal to 3 in
# absolute value, but not outliers (this is for BMI, they do not give potential
# cutoffs for height and weight). In this case, standardized versions of the
# z-scores are computed, and an observation is labeled as an "invalid inliers"
# depending upon its standardized distance and time apart from other
# observations.
# The original paper is available at:
# https://informatics.bmj.com/content/25/1/19
#
# @author = Max Olivier
################################################################################

# Import library used (though it isn't required, could do without data.table).
library(data.table)

################################################################################
# Import and clean data for testing----
# Algorithm needs subject ID, age in days, measurement type (param) and z-scores
# to identify outliers, but since we start with raw data, also need sex so that
# we can get z-scpores. For-age-z-scores of measurement are created from WHO
# values (reference 20 in paper). Data should be in long format.
################################################################################

# Import synthetic data set for testing.
dat <- fread("C:/Users/molivier/Documents/Projects/CDC_CODI/Data_sets/gc-observations.csv")

# Filter out the data in several ways. Just keep weight data (since original
# algorithm uses weight data) and drop all the columns
df <- dat[, c("subjid", "param", "measurement", "sex", "age_days")]

################################################################################
# Setting up variables.----
################################################################################

# Create standardized measurement. Assumes function is already loaded.
df[, stdz_meas := get_standardized_infant_scores(param, age_days, sex, measurement)]

# Sort data by subject, param, and age.
df <- df[order(df$subjid, df$param, df$age_days),]

# Generate variables label outliers and "invalid inliers".
df[,outlier := as.numeric(NA)]
df[,invalid_inlier := as.numeric(NA)]

################################################################################
# Identify outliers----
# Per the "Accuracy" section on pages 20 and 21 of the paper, outliers are taken
# to be values where the z-score is outside of the interval [-6, 6] for height
# and outside the interval [-6, 5] for weight.
################################################################################

ht_subset <- df$param=="HEIGHTCM" & !is.na(df$stdz_meas)
df$outlier[ht_subset] <- abs(df$stdz_meas[ht_subset]) > 6

wt_subset <- df$param=="WEIGHTKG" & !is.na(df$stdz_meas)
df$outlier[wt_subset] <- df$stdz_meas[wt_subset] < -6 |
                          df$stdz_meas[wt_subset] > 5


################################################################################
# Identify invalid inliers----
# "Invalid inliers" are observations that are not outliers, but have a z-score
# of more than 3 and are a certain SD score away from another observation within
# a smaller time window. The value of the SD score and the time window depends
# upon the current age. Note that for the most part this relationship is
# symmetric, so these "invalid inliers" should come in pairs. It is not clear if
# that is what the algorithm intended.
################################################################################

# Generate a column to store standardized measurements.
df[,sd_val := as.numeric(NA)]

# Could potentially try to speed things up by only looking at ID's where there
# is a z-score greater than 3 in absolute value. Since, currently, this is slow.

# Go through all the ID's looking for invalid inliers.
for(id in unique(df$subjid)) {
  # Do this for both height and weight. Since the code is identical for both
  # except the parameter we filter on, just put it in a loop.
  for(type in c("HEIGHTCM", "WEIGHTKG")) {
    # Pick out a subset of the data that identifies this subject along with
    # non-null and non-outlier values of the specific measurement. Identify them
    # as the index values since we need to loop through the individual rows
    # later.
    id_locs <- which(df$subjid==id & df$param==type & !is.na(df$stdz_meas)
                    & df$outlier != 1)

    # Make, a standardized version of the z-scores, but excluding the outlier
    # values. Note it was not clear in the paper that the outlier values should
    # be excluded when computing the standardized measurements, but it seems to
    # make sense to do so.
    df$sd_val[id_locs] <- (df$stdz_meas[id_locs]-mean(df$stdz_meas[id_locs]))/
                                sd(df$stdz_meas[id_locs])

    # For each of the values in the subset index,
    # No issue with an NA value for the outlier location since the sd_val is only valiud if the stdz value is, and if the stdz value is, we can determine if it is an outlier or not

    # Pick out just the data for the rows in question since the age and SD
    # values are needed.
    Z <- df[id_locs]

    # This is where each value is checked to see if it is an "invalid inlier".
    for(val in id_locs) {
      # Can only be an "invalid inlier" if the original z-score was greater than
      # 3 in absolute value and not an outlier. SInce outliers were already
      # filtered out, only need to check the first criteria here.
      if(abs(df$stdz_meas[val]) > 3) {
        # There are separate criteria for those under 1 yr. and those over. For
        # those under, the value is an "invalid inlier" if it is more than 2.5
        # SD's aways from another value and those values are less than 3 months
        # apart. So, compare the current SD score to each other SD score on
        # that criteria. The resulting vector is
        if(df$age_days[val] < 365) {
          meets_criteria <- abs(df$age_days[val] - Z$age_days) < 90 &
            abs(df$sd_val[val] - Z$sd_val) > 2.5
        }
        # Same check, but for the case of those over 1 yr. In this case, a
        # number is an "invalid inliers" if it is more than 3 SD's away from
        # another observation that is less than 6 months away.
        else {
          meets_criteria <- abs(df$age_days[val] - Z$age_days) < 180 &
            abs(df$sd_val[val] - Z$sd_val) > 3
        }
        # If any of the values in "meets_criteria" are 1, then the current
        # observation should be labeled as an outlier.
        df$invalid_inlier[val] <- sum(meets_criteria) > 0
      }
    }
  }
}

# Fill in the rest of the null values in the "invalid inlier" column as not
# invalid inliers as long as there is a z-score (otherwise it couldn't have even
# been under consideration as one).
df$invalid_inlier[!is.na(df$stdz_meas) & is.na(df$invalid_inlier)] <- 0
