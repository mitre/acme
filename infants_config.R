# Specifications to run ACME with the Infants Framework
# By Hannah De los Santos
# Originated on: 12/21/22

# comparison title (same case as file directories) ----

comp_title <- "infants"

# load method functions ----

sourceDir(file.path("EHR_Cleaning_Implementations", comp_title))

# data specification ----

# load default data - fake
dat <- read.csv(file.path("Data", comp_title, "synthea-infants-sub-100subj.csv"))
dat_res <- read.csv(
  file.path("Data", comp_title, "synthea-infants-sub-100subj.csv")
)

# method specification ----

# regular methods

methods_avail <- c("yang", "shi", "carsley", "massara", "growthcleanr",
                   "growthcleanr-naive")

# types cleaned for each method
m_types <- list(
  "HEIGHTCM" = methods_avail,
  "WEIGHTKG" = methods_avail
)

methods_func <- list(yang_clean_both,
                     shi_clean_both,
                     carsley_clean_both,
                     massara_clean_both,
                     growthcleanr_clean_both,
                     growthcleanr_naive_clean_both
)
names(methods_func) <- methods_avail

# method colors
m_colors <- viridisLite::viridis(length(methods_avail))
names(m_colors) <- simpleCap(methods_avail)

# intermediate methods

methods_inter_avail <- c("yang", "shi", "carsley", "massara",
                         "growthcleanr-naive")

# types cleaned for each method
m_inter_types <- list(
  "HEIGHTCM" = methods_avail,
  "WEIGHTKG" = methods_avail
)

methods_inter_func <- list(yang_clean_both,
                           shi_clean_both,
                           # NOTE: CARSLEY HAVING PROBLEMS ATM?
                           carsley_clean_both,
                           massara_clean_both,
                           growthcleanr_naive_clean_both)
names(methods_inter_func) <- methods_inter_avail

# list of steps for each method
m_inter_steps <- list(
  "yang" = c("1w", "1h"),
  "shi" = c("1h", "1w", "2h", "2w"),
  "carsley" = c("1h", "1w", "2h", "2w"),
  "massara" = c("1zwfl"),
  "growthcleanr-naive" = c("1h", "1w")
)

m_inter_steps_full_title <- list(
  "yang" = c(
    "1w" = "1w: W conditional growth percentiles",
    "1h" = "1h: H conditional growth percentiles"
  ),
  "shi" = c(
    "1h" = "1h: H jackknife comparison",
    "1w" = "1w: W jackknife comparison",
    "2h" = "2h: H BIV",
    "2w" = "2w: W BIV"
  ),
  "carsley" = c(
    "1h" = "1h: H BIV",
    "1w" = "1w: W BIV",
    "2h" = "2h: H invalid inliers",
    "2w" = "2w: W invalid inliers"
  ),
  "massara" = c(
    "1zwfl" = "1zwfl: ZWFL mBIV"
  ),
  "growthcleanr-naive" = c(
    "1h" = "1h: H calculate ewma",
    "1w" = "1w: W calculate ewma"
  )
)

m_inter_steps_full_subtitle <- list(
  "yang" = c(
    "1w" = "1w: Calculate conditional growth percentiles using a random effects model, using conditional mean weights and 4 SD range for an individual's weight. If weight measurement is outside mean +/- 4SD, classify as outlier.",
    "1h" = "1h: Calculate conditional growth percentiles using a random effects model, using conditional mean heights and 4 SD range for an individual's height. If height measurement is outside mean +/- 4SD, classify as outlier."
  ),
  "shi" = c(
    "1h" = "1h: Calculate jackknife residuals for both standardized and raw measurements. If the residual > 4, mark as outlier. If it doesn't exist, mark as \"should be further investigated.\" Does not disqualify for consideration in next step.",
    "1w" = "1w: Calculate jackknife residuals for both standardized and raw measurements. If the residual > 4, mark as outlier. If it doesn't exist, mark as \"should be further investigated.\" Does not disqualify for consideration in next step.",
    "2h" = "2h: Identify biologically implausible values in sequential pairs. If measurement of earlier time point is > next time point, identify larger absolute residual as BIV, for both standardized and raw residuals.",
    "2w" = "2w: Identify biologically implausible values in sequential pairs. If 0.85* measurement of earlier time point > next time point, identify larger absolute residual as BIV, for both standardized and raw residuals."
  ),
  "carsley" = c(
    "1h" = "1h: If the z-score is outside of the interval [-6, 6] for height, classify as implausible.",
    "1w" = "1w: If the z-score is outside of the interval [-6, 5] for weight, classify as implausible.",
    "2h" = "2h: For measurements with ages < 1 year, if there is a SD score > 2.5 SD away within 90 days, classify as implausible. For measurements with ages > 1 year, if there is an SD score > 3 SD away within 180 days, classify as implausible.",
    "2w" = "2w: For measurements with ages < 1 year, if there is a SD score > 2.5 SD away within 90 days, classify as implausible. For measurements with ages > 1 year, if there is an SD score > 3 SD away within 180 days, classify as implausible."
  ),
  "massara" = c(
    "1zwfl" = "1zwfl: Identify biologically implausible according to the mBIV criteria: has a standardized score that would make it an outlier by WHO standards and that it either does not have any other observations within two years of it, or if it does that at least one of those observations has a standardized score more than 2 away."
  ),
  "growthcleanr-naive" = c(
    "1h" = "Exclude extreme errors by calculating the exponentially weighted moving average and removing by a specified cutoff (3 for all, 2.5 for before/after). If record(s) is/are found to be extreme, remove the most extreme one and recalculate. Repeat until this no more values are found to be extreme.",
    "1w" = "Exclude extreme errors by calculating the exponentially weighted moving average and removing by a specified cutoff (3 for all, 2.5 for before/after). If record(s) is/are found to be extreme, remove the most extreme one and recalculate. Repeat until this no more values are found to be extreme."
  )
)

# method documentation (module) ----

methods_docs_UI <- function(id){
  list(
    # UI: yang ----
    tabPanel(
      "Yang, et al. (2016)",
      fluidRow(
        column(width = 3),
        column(
          width = 6,
          HTML(
            "<h3>Yang, et al. (2016)</h3>",
            "<h4>Cleans: Weight and Height Records</h4><p>",
            "Shi, et al. seeks identify implausible values and outliers in longitudinal childhood anthropometric data, deciding outliers based on jackknife residuals and biologically implausible values. More information on this method can be found <a href='https://www.sciencedirect.com/science/article/pii/S1047279717306129' target = 'blank'>here</a>. Steps for this method, along with their titles (used in output) and descriptions, are below.<p>"
          ),
          hr(),
          HTML(
            "<h4>Steps:</h4>",
            "<b>Step 1w, W conditional growth percentiles</b><br>",
            "<ul><li>Calculate conditional growth percentiles using a random effects model, using conditional mean weights and 4 SD range for an individual's weight. If weight measurement is greater than 4 SD above mean or less than 4 SD below mean, classify as outlier.</li></ul>",
            "<b>Step 1h, H conditional growth percentiles</b><br>",
            "<ul><li>Calculate conditional growth percentiles using a random effects model, using conditional mean heights and 4 SD range for an individual's height. If height measurement is greater than 4 SD above mean or less than 4 SD below mean, classify as outlier.</li></ul>"
          )
        ),
        column(width = 3)
      )
    ),
    
    # UI: shi ----
    tabPanel(
      "Shi, et al. (2018)",
      fluidRow(
        column(width = 3),
        column(
          width = 6,
          HTML(
            "<h3>Shi, et al. (2018)</h3>",
            "<h4>Cleans: Height and Weight Records</h4><p>",
            "Yang, et al. seeks \"to systematically identify implausible measurements in growth trajectory [EHR] data\", deciding implausible values based on conditional growth percentiles. More information on this method can be found <a href='https://www.sciencedirect.com/science/article/pii/S1047279715004184' target = 'blank'>here</a>. Steps for this method, along with their titles (used in output) and descriptions, are below.<p>"
          ),
          hr(),
          HTML(
            "<h4>Steps:</h4>",
            "<b>Step 1h, H jackknife comparison</b><br>",
            "<ul><li>Calculate jackknife residuals for both WHO standardized and raw measurements. If the residual > 4, mark as outlier. If the standardized or raw measurement doesn't exist, mark as \"should be further investigated.\" Does not disqualify for consideration in next step.</li></ul>",
            "<b>Step 1w, W jackknife comparison</b><br>",
            "<ul><li>Calculate jackknife residuals for both WHO standardized and raw measurements. If the residual > 4, mark as outlier. If the standardized or raw measurement doesn't exist, mark as \"should be further investigated.\" Does not disqualify for consideration in next step.</li></ul>",
            "<b>Step 2h, H BIV</b><br>",
            "<ul><li>Identify biologically implausible values in sequential pairs. If measurement of earlier time point is greater than the next time point, identify larger absolute residual as BIV, for both standardized and raw residuals.</li></ul>",
            "<b>Step 2w, W BIV</b><br>",
            "<ul><li>Identify biologically implausible values in sequential pairs. If 0.85*measurement of earlier time point is greater than next time point, identify larger absolute residual as BIV, for both standardized and raw residuals.</li></ul>"
          )
        ),
        column(width = 3)
      )
    ),
    
    
    # UI: carsley ----
    tabPanel(
      "Carsley, et al. (2018)",
      fluidRow(
        column(width = 3),
        column(
          width = 6,
          HTML(
            "<h3>Carsley, et al. (2018)</h3>",
            "<h4>Cleans: Height and Weight Records</h4><p>",
            "Carsley, et al. aims to determine the data completeness and accuracy of child EMR data, deciding implausible values based on z-scores and subject inliers. More information on this method can be found <a href='https://informatics.bmj.com/content/25/1/19.long' target = 'blank'>here</a>. Steps for this method, along with their titles (used in output) and descriptions, are below.<p>"
          ),
          hr(),
          HTML(
            "<h4>Steps:</h4>",
            "<b>Step 1h, H BIV</b><br>",
            "<ul><li>If the WHO z-score is outside of the interval [-6, 6] for height, classify as implausible.</li></ul>",
            "<b>Step 1w, W BIV</b><br>",
            "<ul><li>If the WHO z-score is outside of the interval [-6, 5] for weight, classify as implausible.</li></ul>",
            "<b>Step 2h, H invalid inliers</b><br>",
            "<ul><li>Calculate standard deviations (SD) for each subject. For measurements with ages less than 1 year, if there is a SD score more than 2.5 SD away within 90 days, classify as implausible. For measurements with ages > 1 year, if there is an SD score more than 3 SD away within 180 days, classify as implausible.</li></ul>",
            "<b>Step 2w, W invalid inliers</b><br>",
            "<ul><li>Calculate standard deviations (SD) for each subject. For measurements with ages less than 1 year, if there is a SD score more than 2.5 SD away within 90 days, classify as implausible. For measurements with ages > 1 year, if there is an SD score more than 3 SD away within 180 days, classify as implausible.</li></ul>"
          )
        ),
        column(width = 3)
      )
    ),
    
    # UI: massara ----
    tabPanel(
      "Massara, et al. (2021)",
      fluidRow(
        column(width = 3),
        column(
          width = 6,
          HTML(
            "<h3>Massara, et al. (2021)</h3>",
            "<h4>Cleans: Height and Weight Records</h4><p>",
            "Massara, et al. aims to use both height and weight longitudinal data to identify outliers. This implementation focuses on their method for deciding implausible values based on a modified BIV criteria. More information on this method can be found <a href='https://dl.acm.org/doi/abs/10.5555/3507788.3507821' target = 'blank'>here</a>. Steps for this method, along with their titles (used in output) and descriptions, are below.<p>"
          ),
          hr(),
          HTML(
            "<h4>Steps:</h4>",
            "<b>Step 1zwfl, ZWFL mBIV</b><br>",
            "<ul><li>Identify biologically implausible according to the mBIV criteria: has a standardized score that would make it an outlier by WHO standards and that it either does not have any other observations within two years of it, or if it does that at least one of those observations has a standardized score more than 2 away.</li></ul>"
          )
        ),
        column(width = 3)
      )
    ),
    
    # UI: growthcleanr ----
    tabPanel(
      "growthcleanr (2022, Daymont, et al. (2017))",
      fluidRow(
        column(width = 3),
        column(
          width = 6,
          HTML(
            "<h3>growthcleanr (2022, Daymont, et al. (2017))</h3>",
            "<h4>Cleans: Height and Weight Records</h4><p>",
            "growthcleanr is based on Daymont, et al. (2017) creates automated method for identifying implausible values in pediatric EHR growth data, deciding implausible values based many factors including exponentially weighted moving averages (EWMA). More information on this method can be found <a href='https://academic.oup.com/jamia/article/24/6/1080/3767271' target = 'blank'>here</a>. Note that this method does not have intermediate values. Steps for this method, along with their titles (used in output) and descriptions, are below.<p>"
          ),
          hr(),
          HTML(
            "<h4>Steps:</h4>",
            "<b>Step 1h, H run growthcleanr</b><br>",
            "<ul><li>Run growthcleanr on height values.</li></ul>",
            "<b>Step 1w, W run growthcleanr</b><br>",
            "<ul><li>Run growthcleanr on height values.</li></ul>"
          )
        ),
        column(width = 3)
      )
    ),
    
    # UI: growthcleanr-naive ----
    tabPanel(
      "growthcleanr-naive (2022, Daymont, et al. (2017))",
      fluidRow(
        column(width = 3),
        column(
          width = 6,
          HTML(
            "<h3>growthcleanr-naive (Daymont, et al. (2017))</h3>",
            "<h4>Cleans: Height and Weight Records</h4><p>",
            "Daymont, et al. aims to automatically detect implausible values in pediatric electronic health records, deciding implausible values based on cutoffs for exponentially weighted moving averages (EWMA). This implementation is a truncated/more simplified version of this method, only using the EWMA protocol. More information on this method can be found <a href='https://academic.oup.com/jamia/article/24/6/1080/3767271' target = 'blank'>here</a>. Steps for this method, along with their titles (used in output) and descriptions, are below.<p>"
          ),
          hr(),
          HTML(
            "<h4>Steps:</h4>",
            "<b>Step 1h, H calculate ewma</b><br>",
            "<ul><li>Exclude extreme errors by calculating the exponentially weighted moving average and removing by a specified cutoff. If record(s) is/are found to be extreme, remove the most extreme one and recalculate. Repeat until this no more values are found to be extreme.</li></ul>",
            "<b>Step 1w, W calculate ewma</b><br>",
            "<ul><li>Exclude extreme errors by calculating the exponentially weighted moving average and removing by a specified cutoff. If record(s) is/are found to be extreme, remove the most extreme one and recalculate. Repeat until this no more values are found to be extreme.</li></ul>"
          )
        ),
        column(width = 3)
      )
    )
  )
}