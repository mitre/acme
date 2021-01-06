# Adult growthcleanr Explorer
# By Hannah De los Santos
# Originated on: 10/7/2020

# This implements a prototype application to explore adult EHR cleaning 
# implementations.

vers_adult_ehr <- "0.2.0"

# load libraries, scripts, and data ----

library(shiny)
library(ggplot2)
library(rstudioapi)
library(colorspace)
library(plotly)
library(viridisLite)
library(ggplotify)
library(reshape2)
library(shinyBS)

#https://stackoverflow.com/questions/3452086/getting-path-of-an-r-script/35842176#35842176
# set working directory - only works in RStudio (with rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load default data - fake 
# dat <- read.csv(file.path("Data", "adult_synthetic_data_seed_8.csv"))
dat <- read.csv(file.path("Data", "synthea-adults-sub-100subj.csv"))
dat_res <- read.csv(
  file.path("Data", "Adult_EHR_Cleaning_Results_data_example.csv")
)

# add "answers" (completely made up), as an example
seed <- 7
set.seed(seed)
dat_answers <- sample(c("Include","Implausible"), 
                      size = nrow(dat), replace = T, prob = c(75, 25))
dat <- cbind(dat, "answers" = dat_answers)
dat_res <- cbind(dat_res, "answers" = dat_answers)

# load requisite functions
# function below from ?source
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    # if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    # if(trace) cat("\n")
  }
}

sourceDir("EHR_Cleaning_Implementations")

# supporting data ----

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

methods_inter_avail <- c("chan")

# types cleaned for each method
m_inter_types <- list(
  "HEIGHTCM" = methods_avail,
  "WEIGHTKG" = methods_avail
)

methods_inter_func <- list(chan_clean_both)
names(methods_inter_func) <- methods_inter_avail

# capitalize first letter of words, from ?toupper, edited to handle vector
simpleCap <- function(y) {
  sapply(y, function(x){
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
  }, USE.NAMES = F)
}

# supporting functions ----

sourceDir("App_Supporting_Functions")

# UI ----

# TODO: LOOK INTO MODULES

ui <- navbarPage(
  # UI: compute and compare results ----
  "Adult EHR Data Cleaning",
  tabPanel(
    "Compare Results",
    sidebarLayout(
      # UI: sidebar options ----
      sidebarPanel(
        width = 3,
        bsCollapse(
          id = "settings",
          multiple = T,
          open = "Start: Run Data/Upload Results",
          bsCollapsePanel(
            "Start: Run Data/Upload Results",
            HTML("<b>Upload adult EHR data or results and click the corresponding button below to get started!</b> If no data is input, default synthetic data/results will be used. More information on data format can be found in the \"About\" tab.<p>"),
            fileInput("dat_file", "Upload Data/Results CSV",
                      accept = c(".csv", ".CSV")),
            div(style="display:inline-block",
                actionButton("run_data", "Run data!"),
                actionButton("upload_res", "Upload Results"),
                downloadButton("download_results", label = "Download Results")
            ),
            style = "default"
          ),
          bsCollapsePanel(
            "Options: All Plots",
            textAreaInput("subj_focus", 
                          "Enter subjects to focus on (line separated):",
                          width = "100%",
                          height = "100px"),
            div(style="display:inline-block",
                actionButton("update_subj", "Update Subjects"),
                actionButton("reset_subj", "Reset"),
                downloadButton("download_focus", label = "Download Subjects")
            ),
            HTML("<p>"),
            checkboxGroupInput(
              "togg_methods",
              "Choose methods to compare:",
              choices = setNames(methods_avail, simpleCap(methods_avail)),
              selected = methods_avail,
              inline = T
            ),
            actionButton("update_methods", "Update Methods"),
            style = "default"
          ),
          bsCollapsePanel(
            "Options: Overall Plots",
            selectInput(
              "togg_res_count",
              label = "Which result would you like to see counted in bar graphs?",
              choices = c("Implausible", "Include"),
              selected = "Implausible"
            ),
            checkboxInput(
              "show_reason_count", 
              label = HTML("<b>Show counts in reasons for implausible values?</b>"),
              value = F
            ),
            style = "default"
          ),
          bsCollapsePanel(
            "Options: Individual/Individual By Method Plots",
            uiOutput("indiv_choose"),
            div(style="display:inline-block",
                actionButton("add_subj_focus", "Add Subject to Focus On")
            ),
            HTML("<p>"),
            selectInput(
              "method_indiv_type",
              "Choose which parameter to display in individual by method plots:",
              choices = c("Height (cm)" = "HEIGHTCM", "Weight (kg)" = "WEIGHTKG")
            ),
            checkboxInput(
              "show_fit_line",
              label = HTML("<b>Show linear fit?</b>"),
              value = T
            ),
            checkboxInput(
              "show_sd_shade",
              label = HTML("<b>Show standard deviation shading?</b> If fit included, this will be around the fit. Otherwise, this will be added around the points."),
              value = T
            ),
            checkboxInput(
              "calc_fit_w_impl",
              label = HTML("<b>Calculate fit/standard deviation with implausible values?</b> If unchecked, records with at least one implausible determination are excluded."),
              value = F
            ),
            style = "default"
          ),
          bsCollapsePanel(
            "Options: All Individuals Heat Map",
            checkboxInput(
              "heat_side_by_side", 
              HTML("<b>Display both height and weight heat maps side by side?</b>"),
              value = T
            ),
            selectInput(
              "heat_type",
              "Choose which parameter to display in all individuals heat map (if displaying only one type):",
              choices = c("Height (cm)" = "HEIGHTCM", "Weight (kg)" = "WEIGHTKG")
            ),
            selectInput(
              "heat_sort_col",
              "Columns to sort on (in order, \"None\" alone indicates no sorting):",
              choices = c(
                "None" = "none",
                "ID" = "id",
                "Subject" = "subj",
                "Measurement" = "measurement",
                "Age (years)" = "age_years",
                "Sex" = "sex"
              ),
              selected = "none",
              multiple = T
            ),
            checkboxInput(
              "heat_show_answers", 
              HTML("<b>Show answers, if available?</b>"),
              value = T
            ),
            
            checkboxInput(
              "heat_hl_incorr", 
              HTML("<b>If showing answers, highlight incorrect answers?</b> If selected, incorrect answers will appear darker than correct answers. Otherwise, correct answers will be highlighted."),
              value = T
            ),
            checkboxInput(
              "heat_sort_dec", 
              HTML("<b>Sort decreasing?</b>"),
              value = F
            ),
            checkboxInput(
              "heat_hide_agree", 
              HTML("<b>Hide rows where all methods include, or are correct (when showing answers)?</b>"),
              value = F
            ),
            checkboxInput(
              "heat_interactive", 
              HTML("<b>Make interactive?</b> Interactivity will not render if the amount of records and methods selected exceeds 1500."),
              value = T
            ),
            checkboxInput(
              "heat_show_y_lab", 
              HTML("<b>Show y labels?</b> Will not be shown if the amount of records exceeds 100. Not recommended with long subject labels when plots are interactive and both types are shown side by side."),
              value = T
            ),
            style = "default"
          ),
          bsCollapsePanel(
            "Options: Check Answers Plots",
            selectInput(
              "answer_bar_tab",
              label = "Which tabulation method would you like to see in bar graphs?",
              choices = c("Count", "Percent"),
              selected = "Count"
            ),
            checkboxInput(
              "answer_group",
              label = HTML("<b>Group bars by Implausible/Include?</b>"), 
              value = T
            ),
            checkboxInput(
              "answer_stack",
              label = HTML("<b>Stack grouped bars?</b> Otherwise, will display bars side-by-side. Not recommended for percent tabulation."), 
              value = F
            ),
            style = "default"
          )
        )
      ),
      # UI: result visualizations ----
      mainPanel(
        width = 9,
        tabsetPanel(
          id = "res_tabset",
          tabPanel(
            "Overall",
            fluidRow(
              width = 12,
              uiOutput("overall_subj_title")
            ),
            fluidRow(
              column(
                width = 6, style='border-right: 1px solid black', 
                HTML("<h3><center>Height Results</center></h3>"),
                plotlyOutput("overall_ht"),
                hr(),
                HTML("<h4><center><b>Top Reasons for Implausible Values</center></b></h4>"),
                dataTableOutput("overall_ht_top_reasons"),
                plotlyOutput("overall_corr_ht", height = "500px")
              ),
              column(
                width = 6, 
                HTML("<h3><center>Weight Results</center></h3>"),
                plotlyOutput("overall_wt"),
                hr(),
                HTML("<h4><center><b>Top Reasons for Implausible Values</center></b></h4>"),
                dataTableOutput("overall_wt_top_reasons"),
                plotlyOutput("overall_corr_wt", height = "500px")
              )
            )
          ),
          tabPanel(
            "Individual",
            fluidRow(
              width = 12,
              uiOutput("indiv_subj_title")
            ),
            fluidRow(
              column(
                width = 6, 
                style='padding-right: 20px; border-right: 1px solid black',
                HTML("<h3><center>Height Results</center></h3>"),
                plotlyOutput("subj_ht"),
                plotOutput("subj_legn_ht", height = "30px"),
                HTML("<br>Note: shading indicates standard deviations (SD) away from fit/data (darker for 1 SD, lighter for 2 SD)."),
                hr(),
                fluidRow(
                  style = "border: 1px #e3e3e3; border-style: solid; border-radius: 10px; background: #f5f5f5; padding: 10px;",
                  uiOutput("about_subj_ht")
                )
              ),
              column(
                width = 6, style = "padding-left: 20px;",
                HTML("<h3><center>Weight Results</center></h3>"),
                plotlyOutput("subj_wt"),
                plotOutput("subj_legn_wt", height = "30px"),
                HTML("<br>Note: shading indicates standard deviations (SD) away from fit/data (darker for 1 SD, lighter for 2 SD)."),
                hr(),
                fluidRow(
                  style = "border: 1px #e3e3e3; border-style: solid; border-radius: 10px; background: #f5f5f5; padding: 10px;",
                  uiOutput("about_subj_wt")
                )
              )
            )
          ),
          tabPanel(
            "Individual by Method",
            fluidRow(
              width = 12,
              uiOutput("method_subj_title")
            ),
            fluidRow(
              width = 12,
              uiOutput("method_subj_plots")
            )
          ),
          tabPanel(
            "All Individuals",
            fluidRow(
              width = 12,
              uiOutput("all_indiv_title"),
            ),
            conditionalPanel(
              "input.heat_side_by_side == true",
              fluidRow(
                column(
                  width = 6, 
                  style='padding-right: 20px; border-right: 1px solid black',
                  HTML("<h3><center>Height (cm)</center></h3>"),
                  conditionalPanel(
                    "input.heat_interactive == true",
                    plotlyOutput("ht_heat_all_plotly", height = 800)
                  ),
                  conditionalPanel(
                    "input.heat_interactive == false",
                    plotOutput("ht_heat_all_plot", height = 800)
                  )
                ),
                column(
                  width = 6,
                  HTML("<h3><center>Weight (kg)</center></h3>"),
                  conditionalPanel(
                    "input.heat_interactive == true",
                    plotlyOutput("wt_heat_all_plotly", height = 800)
                  ),
                  conditionalPanel(
                    "input.heat_interactive == false",
                    plotOutput("wt_heat_all_plot", height = 800)
                  )
                )
              )
            ),
            conditionalPanel(
              "input.heat_side_by_side == false",
              uiOutput("one_heat_type_title"),
              conditionalPanel(
                "input.heat_interactive == true",
                plotlyOutput("one_heat_all_plotly", height = 800)
              ),
              conditionalPanel(
                "input.heat_interactive == false",
                plotOutput("one_heat_all_plot", height = 800)
              )
            )
          ),
          tabPanel(
            "Check Answers",
            fluidRow(
              uiOutput("check_res_title"),
              uiOutput("check_answer_warning")
            ),
            fluidRow(
              column(
                width = 6, style='border-right: 1px solid black', 
                HTML("<h3><center>Height Results</center></h3>"),
                uiOutput("check_ht_possible"),
                plotlyOutput("check_ht"),
                plotOutput("check_legn_ht", height = "30px")
              ),
              column(
                width = 6, 
                HTML("<h3><center>Weight Results</center></h3>"),
                uiOutput("check_wt_possible"),
                plotlyOutput("check_wt"),
                plotOutput("check_legn_wt", height = "30px")
              )
            )
          ),
          tabPanel(
            "View Results",
            uiOutput("res_subj_title"),
            fluidRow(
              column(
                width = 12,
                dataTableOutput("run_output")
              )
            )
          )
        )
      )
    )
  ),
  # UI: intermediate values ----
  tabPanel(
    "Examine Methods",
    sidebarLayout(
      # UI: intermediate sidebar options ----
      sidebarPanel(
        width = 3,
        HTML("<b>Upload adult EHR data or results and click the corresponding button below to understand intermediate values!</b> If no data is input, default synthetic data/results will be used. More information on data format can be found in the \"About\" tab.<p>"),
        fileInput("dat_inter_file", "Upload Data/Results CSV",
                  accept = c(".csv", ".CSV")),
        div(style="display:inline-block",
            actionButton("run_inter_data", "Run data!"),
            actionButton("upload_inter_res", "Upload Results"),
            downloadButton("download_inter_results", label = "Download Results")
        ),
        hr(),
        uiOutput("indiv_inter_choose")
      ),
      # UI: intermediate value visualizations ----
      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel(
            "Chan"
          )
        )
      )
    )
  ),
  # UI: documentation ----
  
  tabPanel(
    "About",
    mainPanel(
      width = 12,
      tabsetPanel(
        # UI: about formatting ----
        tabPanel(
          "About Adult EHR Cleaning and Data Format",
          fluidRow(
            column(width = 3),
            column(
              width = 6,
              HTML(
                "<center><h3>Welcome to the Adult EHR Cleaning Application!</h3></center><p>",
                paste0("<center><h4>Version ", vers_adult_ehr, "</h4></center>"),
                "This application seeks to compare different methods of cleaning adult EHR data, implementing a variety of methods. This currently includes Muthalagu, et al., Cheng, et al., Chan, et al., Littman, et al., Breland, et al., and Growthcleanr-naive. To find out more about these methods, please click on their respective tabs. This application is best viewed in a full screen window.<p><p>",
                "To start, you'll begin by uploading your data in the sidebar under the 'Compare' tab. This data should be a CSV in the following format:"
              ),
              dataTableOutput("dat_example"),
              HTML(  
                "where columns are as follows (names must be exact):<br><ul>",
                "<li><b>id:</b> number for each row, must be unique</li>",
                "<li><b>subjid:</b> subject ID</li>",
                "<li><b>param:</b> parameter for each measurement. must be either HEIGHTCM (height in centimeters) or WEIGHTKG (weight in kilograms)</li>",
                "<li><b>measurement:</b> measurement of height or weight, corresponding to the parameter</li>",
                "<li><b>age_years:</b> age in years</li>",
                "<li><b>sex:</b> 0 (male) or 1 (female)</li>",
                "<li><b>answers:</b> (<u>not required</u>) an answer column, indicating whether the value should be Include or Implausible</li>",
                "</ul><p>",
                "If no data is input, the app will use synthetic data (to find out more about this example data, click on the 'Synthetic Data' tab). Then click run to get started! Note: \"answers\" included in synthetic data are randomly generated for illustration purposes.<p>",
                paste0("Version: ", vers_adult_ehr, "<p>")
              ),
              column(width = 3)
            )
          )
        ),
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
        ),
        # UI: about synthetic data ----
        tabPanel(
          "About Synthetic Data",
          fluidRow(
            column(width = 3),
            column(
              width = 6,
              uiOutput("about_syn_dat"),
              hr(),
              HTML("<center>"),
              fluidRow(
                column(
                  width = 6,
                  plotlyOutput("syn_age_dens")
                ),
                column(
                  width = 6,
                  plotlyOutput("syn_sex_bar")
                )
              ),
              hr(),
              fluidRow(
                column(
                  width = 6,
                  plotlyOutput("syn_ht_dens")
                ),
                column(
                  width = 6,
                  plotlyOutput("syn_wt_dens")
                )
              ),
              HTML("<center>")
            ),
            column(width = 3)
          )
        )
      )
    )
  )
)

# SERVER ----

server <- function(input, output, session) {
  # preallocate reactive values ----
  
  cleaned_df <- reactiveValues(
    "full" = data.frame(),
    "sub" = data.frame()
  )
  
  methods_chosen <- reactiveValues(
    "m" = methods_avail
  )
  
  subj_focus <- reactiveValues(
    "subj" = c()
  )
  
  # save run button clicks
  run_clicks <- reactiveValues(
    "reg" = 0,
    "inter" = 0
  )
  # create a listener for multiple buttons
  run_listener <- reactive({
    if (any(list(input$run_data, input$run_inter_data) > 0)){
      return(list(input$run_data, input$run_inter_data))
    } else {
      return()
    }
  })
  
  # observe button/click inputs ----
  
  observeEvent(run_listener(), {
    withProgress(message = "Cleaning data!", value = 0, {
      # check which button got pressed; if it's not the regular, it's the 
      # intermediate values
      inter <- !(input$run_data[1] > run_clicks$reg)
      
      # update the saved values
      run_clicks$reg <- input$run_data[1]
      run_clicks$inter <- input$run_inter_data[1]
      
      # which methods are we running?
      m_run <- if(inter){methods_inter_avail} else {methods_avail}
      
      tot_increments <- 1+1+length(m_run)
      
      incProgress(1/tot_increments, 
                  message = "Uploading data!",
                  detail = Sys.time())
      
      df <-
        if (is.null(input$dat_file)){
          # use example data
          dat
        } else {
          read.csv(input$dat_file$datapath)
        }
      
      # run each method and save the results
      c_df <- df
      for (m in m_run){
        incProgress(1/tot_increments,
                    message = paste("Running", simpleCap(m)),
                    detail = Sys.time())
        
        # clean data
        m_func <- if (inter){methods_inter_func} else {methods_func}
        clean_df <- 
          m_func[[m]](
            df, inter_vals = inter
          )
        
        # add the results to the overall dataframe
        c_df[,paste0(m, "_result")] <- clean_df$result
        c_df[,paste0(m, "_reason")] <- clean_df$reason
        
        if (inter){
          # if adding intermediate values, we add those at the end
          inter_cols <- colnames(clean_df)[grepl("Step_", colnames(clean_df))]
          c_df[,paste0(m, "_", inter_cols)] <- clean_df[, inter_cols]
        }
      }
      
      # initialize subset (cleaned_df holds all subjects)
      cleaned_df$full <- cleaned_df$sub <- c_df
      
      # now let the tabs update
      all_collapse_names <- c(
        "Start: Run Data/Upload Results",
        "Options: All Plots", 
        "Options: Overall Plots", 
        "Options: Individual/Individual By Method Plots", 
        "Options: Individual/Individual By Method Plots", 
        "Options: All Individuals Heat Map"
      )
      
      tab_map_open <- c(
        "Overall" = "Options: Overall Plots",
        "Individual" = "Options: Individual/Individual By Method Plots",
        "Individual by Method" = "Options: Individual/Individual By Method Plots",
        "All Individuals" = "Options: All Individuals Heat Map",
        "View Results" = NA
      )
      
      open_settings <- c("Options: All Plots", 
                         unname(tab_map_open[input$res_tabset]))
      
      updateCollapse(
        session, 
        id = "settings",
        open = open_settings,
        close = all_collapse_names[!all_collapse_names %in% open_settings]
      )
    })
  })
  
  # upload result data
  observeEvent(input$upload_res, {
    c_df <-
      if (is.null(input$dat_file)){
        # use example data
        dat_res
      } else {
        read.csv(input$dat_file$datapath)
      }
    # because it reads in dashes as periods
    colnames(c_df)[grepl("growthcleanr", colnames(c_df))] <-
      c("growthcleanr-naive_result", "growthcleanr-naive_reason")
    
    # initialize subset
    cleaned_df$full <- cleaned_df$sub <- c_df
  })
  
  # download data results
  output$download_results <- downloadHandler(
    filename = function() {
      if (is.null(input$dat_file)){
        "Adult_EHR_Cleaning_Results_data_example.csv"
      } else {
        paste0("Adult_EHR_Cleaning_Results_", input$dat_file$name)
      }
    },
    content = function(file) {
      write.csv(cleaned_df$full, file, row.names = FALSE, na = "")
    }
  )
  
  # update output to only focus on specified subjects
  observeEvent(input$update_subj, {
    subj <- strsplit(input$subj_focus, "\n")[[1]]
    cleaned_df$sub <-
      cleaned_df$full[as.character(cleaned_df$full$subj) %in% subj,]
  })
  
  # reset output to include all subjects
  observeEvent(input$reset_subj, {
    cleaned_df$sub <- cleaned_df$full
    subj_focus$subj <- c()
    
    updateTextAreaInput(session,
                        "subj_focus",
                        value = "")
  })
  
  # update output to only focus on specified methods
  observeEvent(input$update_methods, {
    methods_chosen$m <- input$togg_methods
  })
  
  observeEvent(input$add_subj_focus, {
    subj_focus$subj[length(subj_focus$subj)+1] <- input$subj
    
    updateTextAreaInput(session,
                        "subj_focus",
                        value = paste(subj_focus$subj, collapse = "\n"))
  })
  
  output$download_focus <- downloadHandler(
    filename = function() {
      if (is.null(input$dat_file)){
        "Adult_EHR_Cleaning_Subject_Focus_List_Example_Data.csv"
      } else {
        paste0("Adult_EHR_Cleaning_Subject_Focus_List_", input$dat_file$name)
      }
    },
    content = function(file) {
      write.csv(data.frame("Focus.Subjects" = subj_focus$subj),
                file, row.names = FALSE, na = "")
    }
  )
  
  # open the options for the given tab with tab opening
  observeEvent(input$res_tabset, {
    all_collapse_names <- c(
      "Start: Run Data/Upload Results",
      "Options: All Plots", 
      "Options: Overall Plots", 
      "Options: Individual/Individual By Method Plots", 
      "Options: Individual/Individual By Method Plots", 
      "Options: All Individuals Heat Map",
      "Options: Check Answers Plots"
    )
    
    tab_map_open <- c(
      "Overall" = "Options: Overall Plots",
      "Individual" = "Options: Individual/Individual By Method Plots",
      "Individual by Method" = "Options: Individual/Individual By Method Plots",
      "All Individuals" = "Options: All Individuals Heat Map",
      "Check Answers" = "Options: Check Answers Plots",
      "View Results" = NA
    )
    
    open_settings <- c("Options: All Plots", 
                       unname(tab_map_open[input$res_tabset]))
    
    if (nrow(cleaned_df$full) > 0){
      updateCollapse(
        session, 
        id = "settings",
        open = open_settings,
        close = all_collapse_names[!all_collapse_names %in% open_settings]
      )
    }
  })
  
  # plot overall results ----
  
  output$overall_subj_title <- renderUI({
    gen_title(nrow(cleaned_df$full) == nrow(cleaned_df$sub), "Overall")
  })
  
  output$overall_ht <- renderPlotly({
    ht_tab <- tab_clean_res(cleaned_df$sub, "HEIGHTCM", methods_chosen$m)
    plot_bar(ht_tab, input$togg_res_count)
  })
  
  output$overall_wt <- renderPlotly({
    wt_tab <- tab_clean_res(cleaned_df$sub, "WEIGHTKG", methods_chosen$m)
    plot_bar(wt_tab, input$togg_res_count)
  })
  
  output$overall_ht_top_reasons <- renderDataTable({
    tab_clean_reason(cleaned_df$sub, "HEIGHTCM", 
                     input$show_reason_count, methods_chosen$m) 
  }, 
  options = list(scrollX = TRUE,
                 pageLength = 5)
  )
  
  output$overall_wt_top_reasons <- renderDataTable({
    tab_clean_reason(cleaned_df$sub, "WEIGHTKG", 
                     input$show_reason_count, methods_chosen$m) 
  }, 
  options = list(scrollX = TRUE,
                 pageLength = 5)
  )
  
  output$overall_corr_ht <- renderPlotly({
    plot_methods_corr(cleaned_df$sub, "HEIGHTCM",
                      methods_chosen = methods_chosen$m)
  })
  
  output$overall_corr_wt <- renderPlotly({
    plot_methods_corr(cleaned_df$sub, "WEIGHTKG",
                      methods_chosen = methods_chosen$m)
  })
  
  # plot individual results ----
  
  output$indiv_choose <- renderUI({
    selectInput(
      "subj",
      label = HTML("<p style = 'font-weight: normal'><b>Which subject's cleaned data would you like to visualize?</b> Search for subjects by pressing backspace and typing.</p>"),
      choices = 
        if (nrow(cleaned_df$sub) == 0){c()} else {unique(cleaned_df$sub$subjid)}
    )
  })
  
  output$indiv_subj_title <- renderUI({
    gen_title(nrow(cleaned_df$full) == nrow(cleaned_df$sub), "Individual")
  })
  
  output$subj_legn_ht <- renderPlot({
    plot_cleaned(cleaned_df$sub, "HEIGHTCM", input$subj,
                 methods_chosen$m, legn = T)
  })
  
  output$subj_legn_wt <- renderPlot({
    plot_cleaned(cleaned_df$sub, "WEIGHTKG", input$subj, 
                 methods_chosen$m, legn = T)
  })
  
  output$subj_ht <- renderPlotly({
    plot_cleaned(cleaned_df$sub, "HEIGHTCM", input$subj, 
                 methods_chosen$m,
                 input$show_fit_line, input$show_sd_shade, 
                 input$calc_fit_w_impl)
  })
  
  output$subj_wt <- renderPlotly({
    plot_cleaned(cleaned_df$sub, "WEIGHTKG", input$subj, 
                 methods_chosen$m,
                 input$show_fit_line, input$show_sd_shade, 
                 input$calc_fit_w_impl)
  })
  
  output$about_subj_ht <- renderUI({
    gen_subj_text(cleaned_df$sub, "HEIGHTCM", input$subj, methods_chosen$m)
  })
  
  output$about_subj_wt <- renderUI({
    gen_subj_text(cleaned_df$sub, "WEIGHTKG", input$subj, methods_chosen$m)
  })
  
  # plot individual results by method ----
  
  # give the overall title
  output$method_subj_title <- renderUI({
    type_map <- c(
      "HEIGHTCM" = "Height (cm)",
      "WEIGHTKG" = "Weight (kg)"
    )
    
    HTML(paste0("<center><h3>Individual ",
                type_map[input$method_indiv_type],
                " for Subject: ",
                input$subj, 
                "</center></h3>"))
  })
  
  # set up a grid to plot each individual
  output$method_subj_plots <- renderUI({
    nc <- 2
    nr <- ceiling(length(methods_chosen$m)/nc)
    
    vert_list <- lapply(1:nr, function(r){
      fluidRow(
        width = 12,
        column(
          style='padding-right: 20px; border-right: 1px solid black',
          width = 6,
          plotlyOutput(paste0("method", (r*2)-1)),
          fluidRow(
            style = "border: 1px #e3e3e3; border-style: solid; border-radius: 10px; background: #f5f5f5; padding: 10px;",
            uiOutput(paste0("method_text", (r*2)-1))
          ),
          hr()
        ),
        column(
          style = "padding-left: 20px;",
          width = 6,
          plotlyOutput(paste0("method", (r*2))),
          fluidRow(
            style = "border: 1px #e3e3e3; border-style: solid; border-radius: 10px; background: #f5f5f5; padding: 10px;",
            uiOutput(paste0("method_text", (r*2)))
          ),
          hr()
        )
      )
      
    })
    # do.call(tagList, plot_output_list)
    
    verticalLayout(
      vert_list
    )
  })
  
  # generate up to the maximum number of plots
  for (m in 1:length(methods_avail)){
    local({
      my_m <- m
      output[[paste0("method", my_m)]] <- renderPlotly({
        if (my_m <= length(methods_chosen$m)){
          plot_cleaned(cleaned_df$sub, input$method_indiv_type,
                       input$subj, 
                       methods_chosen$m[my_m],
                       input$show_fit_line, input$show_sd_shade, 
                       input$calc_fit_w_impl, 
                       single = T)
        } else {
          ggplotly(ggplot()+theme(panel.background = element_blank())) %>% 
            config(displayModeBar = F)
        }
      })
      
      output[[paste0("method_text", my_m)]] <- renderUI({
        gen_subj_text(cleaned_df$sub, input$method_indiv_type, input$subj,
                      methods_chosen$m[my_m], single = T)
      })
    })
  }
  
  # plot all individuals heat map ----
  
  # give the overall title
  output$all_indiv_title <- renderUI({
    HTML(paste0("<center><h3>All ",
                "Records for ",
                length(unique(cleaned_df$sub$subj)),
                " Subjects",
                "</center></h3>"))
  })
  
  output$one_heat_type_title <- renderUI({
    type_map <- c(
      "HEIGHTCM" = "Height (cm)",
      "WEIGHTKG" = "Weight (kg)"
    )
    
    HTML(paste0("<center><h3>",
                type_map[input$heat_type],
                "</center></h3>"))
  })
  
  # render plotly version -- will only render if UI is allocated
  output$ht_heat_all_plotly <- renderPlotly({
    plot_result_heat_map(cleaned_df$sub, 
                         "HEIGHTCM",
                         methods_chosen = methods_chosen$m,
                         sort_col = input$heat_sort_col,
                         hide_agree = input$heat_hide_agree,
                         sort_dec = input$heat_sort_dec,
                         interactive = T,
                         show_y_lab = input$heat_show_y_lab,
                         show_answers = input$heat_show_answers,
                         hl_incorr = input$heat_hl_incorr)
  })
  
  # render ggplot version -- will only render if UI is allocated
  output$ht_heat_all_plot <- renderPlot({
    plot_result_heat_map(cleaned_df$sub, 
                         "HEIGHTCM",
                         methods_chosen = methods_chosen$m,
                         sort_col = input$heat_sort_col,
                         hide_agree = input$heat_hide_agree,
                         sort_dec = input$heat_sort_dec,
                         interactive = F,
                         show_y_lab = input$heat_show_y_lab,
                         show_answers = input$heat_show_answers,
                         hl_incorr = input$heat_hl_incorr)
  })
  
  # render plotly version -- will only render if UI is allocated
  output$wt_heat_all_plotly <- renderPlotly({
    plot_result_heat_map(cleaned_df$sub, 
                         "WEIGHTKG",
                         methods_chosen = methods_chosen$m,
                         sort_col = input$heat_sort_col,
                         hide_agree = input$heat_hide_agree,
                         sort_dec = input$heat_sort_dec,
                         interactive = T,
                         show_y_lab = input$heat_show_y_lab,
                         show_answers = input$heat_show_answers,
                         hl_incorr = input$heat_hl_incorr)
  })
  
  # render ggplot version -- will only render if UI is allocated
  output$wt_heat_all_plot <- renderPlot({
    plot_result_heat_map(cleaned_df$sub, 
                         "WEIGHTKG",
                         methods_chosen = methods_chosen$m,
                         sort_col = input$heat_sort_col,
                         hide_agree = input$heat_hide_agree,
                         sort_dec = input$heat_sort_dec,
                         interactive = F,
                         show_y_lab = input$heat_show_y_lab,
                         show_answers = input$heat_show_answers,
                         hl_incorr = input$heat_hl_incorr)
  })
  
  # render plotly version -- will only render if UI is allocated
  output$one_heat_all_plotly <- renderPlotly({
    plot_result_heat_map(cleaned_df$sub, 
                         input$heat_type,
                         methods_chosen = methods_chosen$m,
                         sort_col = input$heat_sort_col,
                         hide_agree = input$heat_hide_agree,
                         sort_dec = input$heat_sort_dec,
                         interactive = T,
                         show_y_lab = input$heat_show_y_lab,
                         show_answers = input$heat_show_answers,
                         hl_incorr = input$heat_hl_incorr)
  })
  
  # render ggplot version -- will only render if UI is allocated
  output$one_heat_all_plot <- renderPlot({
    plot_result_heat_map(cleaned_df$sub, 
                         input$heat_type,
                         methods_chosen = methods_chosen$m,
                         sort_col = input$heat_sort_col,
                         hide_agree = input$heat_hide_agree,
                         sort_dec = input$heat_sort_dec,
                         interactive = F,
                         show_y_lab = input$heat_show_y_lab,
                         show_answers = input$heat_show_answers,
                         hl_incorr = input$heat_hl_incorr)
  })
  
  # plot check answers ----
  
  output$check_res_title <- renderUI({
    gen_title(nrow(cleaned_df$full) == nrow(cleaned_df$sub), "Check")
  })
  
  output$check_answer_warning <- renderUI({
    if (length(colnames(cleaned_df$sub)) == 0 ||
        !any(grepl("answers", colnames(cleaned_df$sub)))){
      HTML("<center><h4>Checking answers not available, as there is no \"answers\" column in the data. \"answers\" designate whether a record is truly Implausible or Include. For more information on data format, see the \"About\" tab.</h4></center>")
    } else {
      HTML("<center><h4>Note that answers for individual observations can be observed as a visualization in the \"All Individuals\" and \"View Results\" tabs.</h4></center>")
    }
  })
  
  output$check_ht_possible <- renderUI({
    tot_poss_answers(cleaned_df$sub, "HEIGHTCM")
  })
  
  output$check_wt_possible <- renderUI({
    tot_poss_answers(cleaned_df$sub, "WEIGHTKG")
  })
  
  output$check_ht <- renderPlotly({
    ht_tab <- tab_answers(cleaned_df$sub, "HEIGHTCM", methods_chosen$m,
                          group = input$answer_group)
    plot_answer_bar(ht_tab, input$answer_bar_tab, 
                    group = input$answer_group, ontop = input$answer_stack
    )
  })
  
  output$check_wt <- renderPlotly({
    wt_tab <- tab_answers(cleaned_df$sub, "WEIGHTKG", methods_chosen$m,
                          group = input$answer_group)
    plot_answer_bar(wt_tab, input$answer_bar_tab, 
                    group = input$answer_group, ontop = input$answer_stack
    )
  })
  
  output$check_legn_ht <- renderPlot({
    if (input$answer_group){
      ht_tab <- tab_answers(cleaned_df$sub, "HEIGHTCM", methods_chosen$m,
                            group = input$answer_group)
      plot_answer_bar(ht_tab, input$answer_bar_tab, 
                      group = input$answer_group, ontop = input$answer_stack,
                      legn = T
      )
    }
  })
  
  output$check_legn_wt <- renderPlot({
    if (input$answer_group){
      wt_tab <- tab_answers(cleaned_df$sub, "WEIGHTKG", methods_chosen$m,
                            group = input$answer_group)
      plot_answer_bar(wt_tab, input$answer_bar_tab, 
                      group = input$answer_group, ontop = input$answer_stack,
                      legn = T
      )
    }
  })
  
  # output run results ----
  
  output$res_subj_title <- renderUI({
    gen_title(nrow(cleaned_df$full) == nrow(cleaned_df$sub), "Run")
  })
  
  output$run_output <- renderDataTable({
    cleaned_df$sub
  }, 
  options = list(scrollX = TRUE,
                 pageLength = 10)
  )
  
  # output for 'examine methods' (intermediate steps) tab ----
  
  output$indiv_inter_choose <- renderUI({
    selectInput(
      "subj",
      label = HTML("<p style = 'font-weight: normal'><b>Which subject's intermediate steps would you like to examine?</b> Search for subjects by pressing backspace and typing.</p>"),
      choices = 
        if (run_clicks$inter > 0 | nrow(cleaned_df$sub) == 0){
          c()
        } else {
          unique(cleaned_df$sub$subjid)
        }
    )
  })
  
  # output for 'about' tab ----
  
  output$dat_example <- renderDataTable({
    head(dat)
  }, 
  options = list(scrollX = TRUE)
  )
  
  output$about_syn_dat <- renderUI({
    HTML(
      paste0(
        "<h3>About Synthetic Data</h3><p>",
        "Synthetic data was generated by <a href='https://synthetichealth.github.io/synthea/' target = 'blank'>Synthea</a> for ", length(unique(dat$subjid)), " subjects and ", nrow(dat), " records, with ages ranging from ", min(dat$age_years), " to ", max(dat$age_years), ". Descriptive data plots are below. Note: \"answers\" included in synthetic data are randomly generated for illustration purposes.<p>"
      )
    )
  })
  
  output$syn_age_dens <- renderPlotly({
    ggplotly(
      ggplot(dat, aes(age_years))+
        geom_density(fill = "#78abd7", alpha = .7, color = "#78abd7")+
        theme_bw()+
        scale_y_continuous(expand = expansion(mult = c(0,.05)))+
        scale_x_continuous(expand = expansion(mult = c(0,0)))+
        xlab("Age (years)")+
        ylab("Density")+
        ggtitle("Age Distribution")+
        theme(plot.title = element_text(hjust = .5))+
        NULL
    ) %>% config(displayModeBar = F)
  })
  
  output$syn_ht_dens <- renderPlotly({
    ggplotly(
      ggplot(dat[dat$param == "HEIGHTCM",], aes(measurement))+
        geom_density(fill = "#78abd7", alpha = .7, color = "#78abd7")+
        theme_bw()+
        scale_y_continuous(expand = expansion(mult = c(0,.05)))+
        scale_x_continuous(expand = expansion(mult = c(0,0)))+
        xlab("Height (cm)")+
        ylab("Density")+
        ggtitle("Height Distribution")+
        theme(plot.title = element_text(hjust = .5))+
        NULL
    ) %>% config(displayModeBar = F)
  })
  
  output$syn_wt_dens <- renderPlotly({
    ggplotly(
      ggplot(dat[dat$param == "WEIGHTKG",], aes(measurement))+
        geom_density(fill = "#78abd7", alpha = .7, color = "#78abd7")+
        theme_bw()+
        scale_y_continuous(expand = expansion(mult = c(0,.05)))+
        scale_x_continuous(expand = expansion(mult = c(0,0)))+
        xlab("Weight (kg)")+
        ylab("Density")+
        ggtitle("Weight Distribution")+
        theme(plot.title = element_text(hjust = .5))+
        NULL
    ) %>% config(displayModeBar = F)
  })
  
  output$syn_sex_bar <- renderPlotly({
    dat_sub <- dat[!duplicated(dat$subjid),]
    sex_map <- c("0" = "Male", "1" = "Female")
    dat_sub$sex <- sex_map[as.character(dat_sub$sex)]
    
    ggplotly(
      ggplot(dat_sub, aes(sex, fill = sex))+
        geom_bar()+
        theme_bw()+
        scale_fill_manual(
          values = c("Male" = "#3F7FBF", "Female" = "#7F3FBF")
        )+
        scale_y_continuous(expand = expansion(mult = c(0,.05)))+
        xlab("Sex")+
        ylab("Count")+
        ggtitle("Sex Distribution")+
        theme(plot.title = element_text(hjust = .5),
              legend.position = "none")+
        NULL
    ) %>% config(displayModeBar = F)
  })
}

# RUN ----

shinyApp(ui, server)
