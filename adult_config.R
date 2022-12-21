# Specifications to run ACME with the Adult Framework
# By Hannah De los Santos
# Originated on: 12/21/22

# comparison title (same case as file directories) ----

comp_title <- "adult"

# load method functions ----

sourceDir(file.path("EHR_Cleaning_Implementations", comp_title))

# data specification ----

# load default data - fake
dat <- read.csv(file.path("Data", comp_title, "synthea-adults-sub-100subj.csv"))
dat_res <- read.csv(
  file.path("Data", comp_title, "Adult_EHR_Cleaning_Results_data_example.csv")
)

# default age range to focus on ----

age_low <- 18
age_high <- Inf

# method specification ----

# regular methods

methods_avail <- c("muthalagu", "cheng", "chan", "littman", "breland", "growthcleanr-naive")

# types cleaned for each method
m_types <- list(
  "HEIGHTCM" = methods_avail,
  "WEIGHTKG" = methods_avail[-1]
)

methods_func <- list(muthalagu_clean_ht,
                     cheng_clean_both,
                     chan_clean_both,
                     littman_clean_both,
                     breland_clean_both,
                     growthcleanr_clean_both)
names(methods_func) <- methods_avail

# method colors
m_colors <- viridisLite::viridis(length(methods_avail))
names(m_colors) <- simpleCap(methods_avail)

# intermediate methods

methods_inter_avail <- c("muthalagu", "cheng", "chan", "littman", "breland", "growthcleanr-naive")

# types cleaned for each method
m_inter_types <- list(
  "HEIGHTCM" = c("muthalagu", "cheng", "chan", "littman", "breland",
                 "growthcleanr-naive"),
  "WEIGHTKG" = c("muthalagu", "cheng", "chan", "littman", "breland",
                 "growthcleanr-naive")
)

methods_inter_func <- list(muthalagu_clean_ht,
                           cheng_clean_both,
                           chan_clean_both,
                           littman_clean_both,
                           breland_clean_both,
                           growthcleanr_clean_both)
names(methods_inter_func) <- methods_inter_avail

# list of steps for each method
m_inter_steps <- list(
  "muthalagu" = c("1h", "2ha", "2hb", "2hc"),
  "cheng" = c("1h", "2h", "1w", "2w", "3"),
  "chan" = c("1h", "2h", "1w", "2w", "3w"),
  "littman" = c("1h", "1wa", "1wb", "1bmi", "2w", "2h", "3h"),
  "breland" = c("Preprocessing", "1h", "1w", "2w"),
  "growthcleanr-naive" = c("1h", "1w")
)

m_inter_steps_full_title <- list(
  "muthalagu" = c(
    "1h" = "1h: H BIV",
    "2ha" = "2ha: H age range check",
    "2hb" = "2hb: H median check",
    "2hc"= "2hc: H erroneous and indeterminate median check"
  ),
  "cheng" = c(
    "1h" = "1h: H BIV",
    "2h" = "2h: H compare difference from average to SD",
    "1w" = "1w: W BIV",
    "2w" = "2w: W compare difference from average to range or SD",
    "3" = "3: BMI BIV"
  ),
  "chan" = c(
    "1h" = "1h: H BIV",
    "2h" = "2h: H check SD away from mean",
    "1w" = "1w: W BIV",
    "2w" = "2w: W BMI BIV",
    "3w" = "3w: W check SD away from mean"
  ),
  "littman" = c(
    "1h" = "1h: H BIV", 
    "1wa" = "1wa: W BIV cutoffs", 
    "1wb" = "1wb: W BIV rate change", 
    "1bmi" = "1bmi: BMI BIV", 
    "2w" = "2w: W compare difference from average to SD", 
    "2h" = "2h: H compare difference from average to SD", 
    "3h" = "3h: H compare difference to SD, with most deviant height dropped"
  ),
  "breland" = c(
    "Preprocessing" = "Preprocessing: Convert to U.S. measurements",
    "1h" = "1h: H BIV",
    "1w" = "1w: W BIV",
    "2w" = "2w: W compare weight trajectory ratios"
  ),
  "growthcleanr-naive" = c(
    "1h" = "1h: H calculate ewma",
    "1w" = "1w: W calculate ewma"
  )
)

m_inter_steps_full_subtitle <- list(
  "muthalagu" = c(
    "1h" = "1: Remove biologically implausible height records. Heights are biologically implausible if less than 100 cm or greater than 250 cm.",
    "2ha" = "2a: If height range < 3.5 cm, all heights in that bucket are plausible.",
    "2hb" = "2b: If the height range is > 3.5 cm, calculate median height at each age. Compare with prior and next median. If height at current age differs by > 3.5 cm compared to prior and next median, flag as potentially erroneous. If only two valid medians and differ by > 3.5 cm, flag both as indeterminate. First and last records by age in bucket are indeterminate.",
    "2hc"= "2c: For erroneous and indeterminate medians, assign correct medians within 3 year period. Then compare all other recorded heights to the median at that age. If the recorded height for any age differs > 3.5 cm (for erroneous) or > 6 cm (for indeterminate) from cleaned median height for that age, the value is erroneous."
  ),
  "cheng" = c(
    "1h" = "Remove biologically implausible height records. Heights are biologically implausible if less than 111.8 cm or greater than 228.6 cm.",
    "2h" = "Exclude height if a) absolute difference between that height and average height > standard deviation (SD) AND b) SD > 2.5% of average height.",
    "1w" = "Remove biologically implausible weight records. Weights are biologically implausible if less than 24.9 kg or greater than 453.6 kg.",
    "2w" = "Weight was determined to be inaccurate if: a) the range was > 22.7 kg AND absolute difference between recorded weight and avg weight was > 70% of range OR b) SD was >20% of the average weight AND absolute difference between that weight and average weight > the SD.",
    "3" = "Remove biologically implausible BMI records. If BMI for a given set of height/weights is < 12 or > 70, deem implausible."
  ),
  "chan" = c(
    "1h" = "Remove biologically implausible height records. Heights are biologically implausible if less than 121.92 cm (48 in) or greater than 213 cm (84 in).",
    "2h" = "Exclude heights that were greater than 3 standard deviations from the mean.",
    "1w" = "Remove biologically implausible weight records. Weights are biologically implausible if less than 22.7 kg or greater than 340.2 kg.",
    "2w" = "Calculate BMI based on average height for all weight records, then remove biologically implausible weights. BMIs are biologically implausible if less than 10 or greater than 100.",
    "3w" = "Exclude weights that were greater than 3 standard deviations from the mean."
  ),
  "littman" = c(
    "1h" = "Remove biologically implausible height records. Heights are biologically implausible if less than 49 in (124.46 cm) or greater than 94 in (238.76 cm).", 
    "1wa" = "Remove biologically implausible weight records. Weights are biologically implausible if less than 75 lbs (34.0194 kg) or greater than 600 lbs (272.1552 kg).", 
    "1wb" = "Remove biologically implausible weight records based on rate of weight change over time. Weights are biologically implausible if the weight change per week is greater than 2 lbs (0.907184 kg) and greater than 50 lbs (22.6796 kg) overall, OR the rate of weight change is greater than 100 lbs (45.3592 kg).", 
    "1bmi" = "Remove biologically implausible BMI records. If BMI for a given set of height/weights is > 80, deem implausible.", 
    "2w" = "Exclude any weight measurements where: 1) difference between mean weight and recorded weight was greater than the standard deviation (SD) AND 2) the SD was greater than 10% of the mean.", 
    "2h" = "Exclude any height measurements where: 1) difference between mean height and recorded height was greater than SD AND 2) SD was greater than 2.5% of mean.", 
    "3h" = "Run step 2h again, but with the most deviant height dropped to see if there are any more implausible values."
  ),
  "breland" = c(
    "Preprocessing" = "Convert all heights to inches and weights to pounds. Round height to the nearest whole inch. Round weight to the nearest hundreth pound.",
    "1h" = "Remove biologically implausible height records. Heights are biologically implausible if less than 48 in or greater than 84 in.",
    "1w" = "Remove biologically implausible weight records. Weights are biologically implausible if less than 75 lbs or greater than 700 lbs.",
    "2w" = "Compute ratios of weight trajectories (ratio 1: current record/prior record, ratio 2: current record/next record). Compute indicator variables based on the ratios:<br>
        if ratio <= .67, indicator = -1<br>
        if ratio <= 1.50, indicator = 1<br>
        else, indicator = 0<br>
    Set record to missing if both ratios are -1 OR both ratios are 1."
  ),
  "growthcleanr-naive" = c(
    "1h" = "Exclude extreme errors by calculating the exponentially weighted moving average and removing by a specified cutoff (2 for all, 1.5 for before/after). If record(s) is/are found to be extreme, remove the most extreme one and recalculate. Repeat until this no more values are found to be extreme.",
    "1w" = "Exclude extreme errors by calculating the exponentially weighted moving average and removing by a specified cutoff (2 for all, 1.5 for before/after). If record(s) is/are found to be extreme, remove the most extreme one and recalculate. Repeat until this no more values are found to be extreme."
  )
)

# method documentation (module) ----

methods_docs_UI <- function(id){
  list(
    # UI: muthalagu ----
    tabPanel(
      "Muthalagu, et al. (2014)",
      fluidRow(
        column(width = 3),
        column(
          width = 6,
          HTML(
            "<h3>Muthalagu, et al. (2014)</h3>",
            "<h4>Cleans: Height Records</h4><p>",
            "Muthalagu, et al. aims to transform EHR adult height data into \"research-ready\" values using age and height values, deciding implausible values based on median comparisons within age ranges. More information on this method can be found <a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3974252/' target = 'blank'>here</a>. Steps for this method, along with their titles (used in output) and descriptions, are below.<p>"
          ),
          hr(),
          HTML(
            "<h4>Steps:</h4>",
            "<b>Step 1, H BIV</b><br>",
            "<ul><li>Remove biologically implausible height records. Heights are biologically implausible if less than 100 cm or greater than 250 cm.</li></ul>",
            "<b>Step 2, Go through each age bucket</b><br>",
            "<ul><li>Age bucket are defined to be 18 - 25, 25 - 50, and 50 and above.</li></ul>",
            "<b>Step 2a, H age range check</b><br>",
            "<ul><li>If height range < 3.5 cm, all heights in that bucket are plausible.</li></ul>",
            "<b>Step 2b, H median check</b><br>",
            "<ul><li>If the height range is > 3.5 cm, calculate median height at each age. Compare with prior and next median. If height at current age differs by > 3.5 cm compared to prior and next median, flag as potentially erroneous. If only two valid medians and differ by > 3.5 cm, flag both as indeterminate. First and last records by age in bucket are indeterminate.</li></ul>",
            "<b>Step 2c, H erroneous and indeterminate median check</b><br>",
            "<ul><li>For erroneous and indeterminate medians, assign correct medians within 3 year period. Then compare all other recorded heights to the median at that age. If the recorded height for any age differs  > 3.5 cm (for erroneous) or > 6 cm (for indeterminate) from cleaned median height for that age, the value is erroneous.</li></ul>"
          )
        ),
        column(width = 3)
      )
    ),
    # UI: cheng ----
    tabPanel(
      "Cheng, et al. (2016)",
      fluidRow(
        column(width = 3),
        column(
          width = 6,
          HTML(
            "<h3>Cheng, et al. (2016)</h3>",
            "<h4>Cleans: Height and Weight Records</h4><p>",
            "Cheng, et al. aims to examine association between baseline BMI and all-cause mortality, deciding implausible values based on computing ranges and comparing standard deviations to means. More information on this method can be found <a href='https://onlinelibrary.wiley.com/doi/full/10.1002/oby.21612' target = 'blank'>here</a>. Steps for this method, along with their titles (used in output) and descriptions, are below.<p>"
          ),
          hr(),
          HTML(
            "<h4>Steps:</h4>",
            "<b>Step 1h, H BIV</b><br>",
            "<ul><li>Remove biologically implausible height records. Heights are biologically implausible if less than 111.8 cm or greater than 228.6 cm.</li></ul>",
            "<b>Step 2h, H compare difference from average to SD</b><br>",
            "<ul><li>Exclude height if a) absolute difference between that height and average height > standard deviation (SD) AND b) SD > 2.5% of average height.</li></ul>",
            "<b>Step 1w, W BIV</b><br>",
            "<ul><li>Remove biologically implausible weight records. Weights are biologically implausible if less than 24.9 kg or greater than 453.6 kg.</li></ul>",
            "<b>Step 2w, W compare difference from average to range or SD</b><br>",
            "<ul><li>Weight was determined to be inaccurate if: a) the range was > 22.7 kg AND absolute difference between recorded weight and avg weight was > 70% of range OR b) SD was >20% of the average weight AND absolute difference between that weight and average weight > the SD.</li></ul>",
            "<b>Step 3, BMI BIV</b><br>",
            "<ul><li>Remove biologically implausible BMI records. If BMI for a given set of height/weights is < 12 or > 70, deem implausible.</li></ul>"
          )
        ),
        column(width = 3)
      )
    ),
    # UI: chan ----
    tabPanel(
      "Chan, et al. (2017)",
      fluidRow(
        column(width = 3),
        column(
          width = 6,
          HTML(
            "<h3>Chan, et al. (2017)</h3>",
            "<h4>Cleans: Height and Weight Records</h4><p>",
            "Chan, et al. aims to examine the relationship between MOVE! and weight outcomes (odds of achieving clinically relevant weight loss at 12 months), deciding implausible values based on comparing standard deviations to means. More information on this method can be found <a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5359164/' target = 'blank'>here</a>. Steps for this method, along with their titles (used in output) and descriptions, are below.<p>"
          ),
          hr(),
          HTML(
            "<h4>Steps:</h4>",
            "<b>Step 1h, H BIV</b><br>",
            "<ul><li>Remove biologically implausible height records. Heights are biologically implausible if less than 121.92 cm (48 in) or greater than 213 cm (84 in).</li></ul>",
            "<b>Step 2h, H check SD away from mean</b><br>",
            "<ul><li>Exclude heights that were greater than 3 standard deviations from the mean.</li></ul>",
            "<b>Step 1w, W BIV</b><br>",
            "<ul><li>Remove biologically implausible weight records. Weights are biologically implausible if less than 22.7 kg or greater than 340.2 kg.</li></ul>",
            "<b>Step 2w, W BMI BIV</b><br>",
            "<ul><li>Calculate BMI based on average height for all weight records, then remove biologically implausible weights. BMIs are biologically implausible if less than 10 or greater than 100.</li></ul>",
            "<b>Step 3w, W check SD away from mean</b><br>",
            "<ul><li>Exclude weights that were greater than 3 standard deviations from the mean.</li></ul>"
          )
        ),
        column(width = 3)
      )
    ),
    # UI: littman ----
    tabPanel(
      "Littman, et al. (2012)",
      fluidRow(
        column(width = 3),
        column(
          width = 6,
          HTML(
            "<h3>Littman, et al. (2012)</h3>",
            "<h4>Cleans: Height and Weight Records</h4><p>",
            "Littman, et al. aims to assess the reach and effectiveness of the MOVE! study on veteran obesity, and in doing so, decides implausible values based on comparing standard deviations to means and differences between records and means. More information on this method can be found <a href='https://www.cdc.gov/pcd/issues/2012/11_0267.htm' target = 'blank'>here</a>. Steps for this method, along with their titles (used in output) and descriptions, are below.<p>"
          ),
          hr(),
          HTML(
            "<h4>Steps:</h4>",
            "<b>Step 1h, H BIV</b><br>",
            "<ul><li>Remove biologically implausible height records. Heights are biologically implausible if less than 49 in (124.46 cm) or greater than 94 in (238.76 cm).</li></ul>",
            "<b>Step 1wa, W BIV cutoffs</b><br>",
            "<ul><li>Remove biologically implausible weight records. Weights are biologically implausible if less than 75 lbs (34.0194 kg) or greater than 600 lbs (272.1552 kg).</li></ul>",
            "<b>Step 1wb, W BIV rate change</b><br>",
            "<ul><li>Remove biologically implausible weight records based on rate of weight change over time. Weights are biologically implausible if the weight change per week is greater than 2 lbs (0.907184 kg) and greater than 50 lbs (22.6796 kg) overall, OR the rate of weight change is greater than 100 lbs (45.3592 kg).</li></ul>",
            "<b>Step 1bmi, BMI BIV</b><br>",
            "<ul><li>Remove biologically implausible BMI records. If BMI for a given set of height/weights is > 80, deem implausible.</li></ul>",
            "<b>Step 2w, W compare difference from average to SD</b><br>",
            "<ul><li>Exclude any weight measurements where: 1) difference between mean weight and recorded weight was greater than the standard deviation (SD) AND 2) the SD was greater than 10% of the mean.</li></ul>",
            "<b>Step 2h, H compare difference from average to SD</b><br>",
            "<ul><li>Exclude any height measurements where: 1) difference between mean height and recorded height was greater than SD AND 2) SD was greater than 2.5% of mean.</li></ul>",
            "<b>3h, H compare difference to SD, with most deviant height dropped</b><br>",
            "<ul><li>Run step 2h again, but with the most deviant height dropped to see if there are any more implausible values.</li></ul>"
          )
        ),
        column(width = 3)
      )
    ),
    # UI: breland ----
    tabPanel(
      "Breland, et al. (2017)",
      fluidRow(
        column(width = 3),
        column(
          width = 6,
          HTML(
            "<h3>Breland, et al. (2017)</h3>",
            "<h4>Cleans: Height and Weight Records</h4><p>",
            "Breland, et al. aims to describe the prevalence of obesity among Veteran sub-populations to inform weight management programs, deciding implausible values based on computing biologically implausible values and weight trajectories. More information on this method can be found <a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5359156/' target = 'blank'>here</a>. Steps for this method, along with their titles (used in output) and descriptions, are below.<p>"
          ),
          hr(),
          HTML(
            "<h4>Steps:</h4>",
            "<b>Preprocessing Step:</b><br>",
            "<ul><li>Convert all heights to inches and weights to pounds. Round height to the nearest whole inch. Round weight to the nearest hundreth pound.</li></ul>",
            "<b>Step 1h, H BIV</b><br>",
            "<ul><li>Remove biologically implausible height records. Heights are biologically implausible if less than 48 in or greater than 84 in.</li></ul>",
            "<b>Step 1w, W BIV</b><br>",
            "<ul><li>Remove biologically implausible weight records. Weights are biologically implausible if less than 75 lbs or greater than 700 lbs.</li></ul>",
            "<b>Step 2w, W compare weight trajectory ratios</b><br>",
            "<ul><li>Compute ratios of weight trajectories (ratio 1: current record/prior record, ratio 2: current record/next record). Compute indicator variables based on the ratios:<ul>
                <li>if ratio <= .67, indicator = -1</li>
                <li>if ratio <= 1.50, indicator = 1</li>
                <li>else, indicator = 0</li></ul>
                Set record to missing if both ratios are -1 OR both ratios are 1.</li></ul>",
            "</ul>"
          )
        ),
        column(width = 3)
      )
    ),
    # UI: growthcleanr-naive ----
    tabPanel(
      "Growthcleanr-naive (Daymont, et al. (2017))",
      fluidRow(
        column(width = 3),
        column(
          width = 6,
          HTML(
            "<h3>Growthcleanr-naive (Daymont, et al. (2017))</h3>",
            "<h4>Cleans: Height and Weight Records</h4><p>",
            "Daymont, et al. aims to automatically detect implausible values in pediatric electronic health records, deciding implausible values based on cutoffs for exponentially weighted moving averages (EWMA). This implementation is a truncated/more simplified version of this method, only using the EWMA protocol and adjusting the cutoffs for adults. More information on this method can be found <a href='https://academic.oup.com/jamia/article/24/6/1080/3767271' target = 'blank'>here</a>. Steps for this method, along with their titles (used in output) and descriptions, are below.<p>"
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
    # end ----
  )
}