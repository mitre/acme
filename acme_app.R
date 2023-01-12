# Anthropometric Cleaning Methods Explorer
# By Hannah De los Santos
# Originated on: 8/9/2022

# This implements an application to explore anthropometric cleaning
# implementations.

# USER: uncomment/edit one of the lines below to compare different anthropometric 
# methods for your configuration

comp_config <- "infants_config.R"
# comp_config <- "adult_config.R"
# comp_config <- "NEW_config.R"

# version and options ---- 

vers_ehr <- "2.0.0"

options(shiny.maxRequestSize=500*1024^2)

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
library(shinyWidgets)
# to install:
# install.packages(devtools)
# devtools::install_github("zeehio/facetscales")
library(facetscales)

#https://stackoverflow.com/questions/3452086/getting-path-of-an-r-script/35842176#35842176
# set working directory - only works in RStudio (with rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# source supporting functions and method specifications ----

source(file.path("Supporting_Functions", "load_func.R"))
source(file.path("Supporting_Functions", "processing_func.R"))
source(file.path("Supporting_Functions", "plotting_func.R"))

source(comp_config)

# add "answers" (completely made up), as an example
seed <- 7
set.seed(seed)
dat_answers <- sample(c("Include","Implausible"),
                      size = nrow(dat), replace = T, prob = c(75, 25))
dat <- cbind(dat, "answers" = dat_answers)
dat_res <- cbind(dat_res, "answers" = dat_answers)

# UI ----

# TODO: LOOK INTO MODULES

ui <- navbarPage(
  # UI: compute and compare results ----
  paste0("Anthropometric Methods Cleaning Explorer (ACME): ", 
         tools::toTitleCase(comp_title)),
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
            HTML("<b>Upload EHR data or results and click the corresponding button below to get started!</b> If no data is input, default synthetic data/results will be used. More information on data format can be found in the \"About\" tab. Note that this will update data in \"Examine Methods\" tab.<p>"),
            fileInput("dat_file", "Upload Data/Results CSV",
                      accept = c(".csv", ".CSV")),
            checkboxInput(
              "run_ex",
              HTML("<b>Run example data?</b>"),
              value = F
            ),
            numericRangeInput(
              "run_age_cap",
              "Run only on records between ages (in years):",
              value = c(age_low, age_high),
              min = 0
            ),
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
              "show_perc_bar",
              label = HTML("<b>Show percentages of total in bar graphs?</b>"),
              value = F
            ),
            checkboxInput(
              "show_reason_count",
              label = HTML("<b>Show counts in reasons for implausible values?</b>"),
              value = F
            ),
            checkboxInput(
              "show_heat_corr",
              label = HTML("<b>Show correlation between methods in heat map?</b>"),
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
            checkboxInput(
              "show_indiv_impl",
              label = HTML("<b>Only choose from subjects with at least one implausible value?</b>"),
              value = F
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
              HTML("<b>Display both height and weight heat maps side by side?</b> Legends may be cut off if checked."),
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
            div(
              style="display: inline-block;",
              checkboxInput(
                "heat_reduce_lines",
                HTML("<b>Show first X entries?</b>"),
                value = F
              )
            ),
            div(style="display: inline-block; width: 70px;",
                numericInput(
                  "heat_reduce_amount",
                  "X:",
                  value = 10,
                  step = 5,
                  min = 0)
            ),
            div(style="display: inline-block; width: 70px;",
                numericInput(
                  "heat_offset_amount",
                  "Offset?:",
                  value = 0,
                  step = 10,
                  min = 0)
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
              HTML("<p align = 'right'>Note: Clicking on a tile will add that subject/measurement to the focus list in both \"Compare Results\" and \"Examine Methods\".</p>"),
            ),
            fluidRow(
              width = 12,
              plotOutput("all_indiv_legn", height = "30px"),
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
        HTML("<b>Upload EHR data or results and click the corresponding button below to understand intermediate values!</b> If no data is input, default synthetic data/results will be used. More information on data format can be found in the \"About\" tab. Note that this will update data in \"Compare Results\" tab.<p>"),
        fileInput("dat_inter_file", "Upload Data/Results CSV",
                  accept = c(".csv", ".CSV")),
        checkboxInput(
          "run_inter_ex",
          HTML("<b>Run example data?</b>"),
          value = F
        ),
        div(style="display:inline-block",
            actionButton("run_inter_data", "Run data!"),
            actionButton("upload_inter_res", "Upload Results"),
            downloadButton("download_inter_results", label = "Download Results")
        ),
        hr(),
        textAreaInput("subj_inter_focus",
                      "Enter subjects/record IDs to focus on (line separated,  with / between subject and record ID):",
                      width = "100%",
                      height = "100px"),
        div(style="display:inline-block",
            actionButton("update_inter_subj", "Update Focus"),
            actionButton("reset_inter_subj", "Reset"),
            downloadButton("download_inter_focus", label = "Download Focus")
        ),
        p(),
        uiOutput("indiv_inter_choose"),
        checkboxInput(
          "show_indiv_inter_impl",
          label = HTML("<b>Only choose from subjects with at least one implausible value?</b>"),
          value = F
        ),
        checkboxInput(
          "inter_color_ans",
          HTML("<b>Show answers (if available)?</b>"),
          value = T
        )
      ),
      # UI: intermediate value visualizations ----
      mainPanel(
        width = 9,
        do.call(
          tabsetPanel,
          c(
            id = "inter_tabset",
            lapply(methods_inter_avail, function(m_name){
              tabPanel(
                simpleCap(m_name),
                br(),
                HTML("<center>"),
                sliderTextInput(
                  paste0(m_name, "_method_step"),
                  "Choose Step:",
                  choices = c(
                    "Before",
                    m_inter_steps[[m_name]],
                    "After"
                  ),
                  selected = "Before"
                ),
                uiOutput(paste0(m_name, "_iter_step_ui")),
                uiOutput(paste0(m_name, "_bucket_step_ui")),
                HTML("</center>"),
                uiOutput(paste0(m_name, "_step_title")),
                uiOutput(paste0(m_name, "_step_subtitle")),
                plotlyOutput(paste0(m_name, "_inter_plot")),
                plotOutput(paste0(m_name, "_inter_plot_legn"), height = 50),
                div(style = 'overflow-x: scroll',
                    tableOutput(paste0(m_name, '_inter_table'))
                )
              )
            })
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
      do.call(
        tabsetPanel,
        c(
          # UI: about formatting ----
          list(
            tabPanel(
              "About ACME and Data Format",
              fluidRow(
                column(width = 3),
                column(
                  width = 6,
                  HTML(
                    "<center><h3>Welcome to the Anthropometric Methods Cleaning Explorer (ACME)!</h3></center><p>",
                    paste0("<center><h4>Version ", vers_ehr, "</h4></center>"),
                    "This application seeks to compare different methods of cleaning EHR data, implementing a variety of methods. To find out more about these methods, please click on their respective tabs. This application is best viewed in a full screen window.<p><p>",
                    "To start, you'll begin by uploading your data in the sidebar under the 'Compare' tab. This data should be a CSV in the following format:"
                  ),
                  dataTableOutput("dat_example"),
                  HTML(
                    "where columns are as follows (names must be exact):<br><ul>",
                    "<li><b>id:</b> number for each row, must be unique</li>",
                    "<li><b>subjid:</b> subject ID</li>",
                    "<li><b>param:</b> parameter for each measurement. must be either HEIGHTCM (height in centimeters) or WEIGHTKG (weight in kilograms)</li>",
                    "<li><b>measurement:</b> measurement of height or weight, corresponding to the parameter</li>",
                    "<li><b>age_years:</b> age in years (ages < 18 will be filtered out) (can also be <b>agedays</b>, as in the original growthcleanr format, and will be automatically converted to years)</li>",
                    "<li><b>sex:</b> 0 (male) or 1 (female)</li>",
                    "<li><b>answers:</b> (<u>not required</u>) an answer column, indicating whether the value should be Include or Implausible</li>",
                    "</ul><p>",
                    "If no data is input, the app will use synthetic data (to find out more about this example data, click on the 'Synthetic Data' tab). Then click run to get started! Note: \"answers\" included in synthetic data are randomly generated for illustration purposes.<p>",
                    paste0("Version: ", vers_ehr, "<p>")
                  ),
                  column(width = 3)
                )
              )
            )
          ),
          # UI: method documentation ----
          methods_docs_UI("method_docs"),
          # UI: about synthetic data ----
          list(
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
  )
)

# SERVER ----

server <- function(input, output, session) {
  # preallocate reactive values ----
  
  cleaned_df <- reactiveValues(
    "full" = data.frame(),
    "sub" = data.frame()
  )
  
  cleaned_inter_df <- reactiveValues(
    "full" = data.frame(),
    "sub" = data.frame()
  )
  
  methods_chosen <- reactiveValues(
    "m" = methods_avail
  )
  
  subj_focus <- reactiveValues(
    "subj" = c(),
    "subj_inter" = c()
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
      
      # which methods are we running? -- we're running all of them
      # idea -> run all so that both datasets are available
      # m_run <- if(inter){methods_inter_avail} else {methods_avail}
      m_run <- methods_avail
      
      tot_increments <- 1+1+length(m_run)
      
      incProgress(1/tot_increments,
                  message = "Uploading data!",
                  detail = Sys.time())
      
      df <-
        if (inter){
          if (is.null(input$dat_inter_file) | input$run_inter_ex){
            # use example data
            dat
          } else {
            read.csv(input$dat_inter_file$datapath)
          }
        } else {
          if (is.null(input$dat_file) | input$run_ex){
            # use example data
            dat
          } else {
            read.csv(input$dat_file$datapath)
          }
        }
      
      # check that age_years is not "ageyears"
      if ("ageyears" %in% colnames(df)){
        colnames(df)[colnames(df) == "ageyears"] <- "age_years"
      }
      if ("agedays" %in% colnames(df)){
        colnames(df)[colnames(df) == "agedays"] <- "age_days"
      }
      # check that df has age_years or age days, preferring age_years
      if ("age_days" %in% colnames(df) & !"age_years" %in% colnames(df)){
        df$age_years <- df$agedays /365.25
      }
      # fix id if not unique or if it doesn't exist
      if (is.null(df$id) || length(unique(df$id)) != nrow(df)){
        df$id <- 1:nrow(df)
      }
      
      # data checks for specified age ranges
      high_cutoff <- ifelse(is.na(input$run_age_cap[2]),
                            Inf,
                            input$run_age_cap[2])
      low_cutoff <- ifelse(is.na(input$run_age_cap[1]) | input$run_age_cap[1] < 0,
                           0,
                           input$run_age_cap[1])
      
      # run only records specified
      df <- df[df$age_years <= high_cutoff, ]
      df <- df[df$age_years >= low_cutoff, ]
      
      # run each method and save the results
      c_df <- df
      for (m in m_run){
        incProgress(1/tot_increments,
                    message = paste("Running", simpleCap(m)),
                    detail = Sys.time())
        
        # clean data
        # m_func <- if (inter){methods_inter_func} else {methods_func}
        m_func <- methods_func
        clean_df <-
          m_func[[m]](
            df, inter_vals = T
          )
        
        # add the results to the overall dataframe
        c_df[,paste0(m, "_result")] <- clean_df$result
        c_df[,paste0(m, "_reason")] <- clean_df$reason
        
        if (m %in% methods_inter_avail){
          # if adding intermediate values, we add those at the end
          inter_cols <- colnames(clean_df)[grepl("Step_", colnames(clean_df))]
          c_df[,paste0(m, "_", inter_cols)] <- clean_df[, inter_cols]
        }
        
      }
      
      # initialize subset (cleaned_df holds all subjects)
      cleaned_inter_df$full <- cleaned_inter_df$sub <- c_df
      cleaned_df$full <- cleaned_df$sub <-
        c_df[, !grepl("Step_", colnames(c_df))]
      
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
    withProgress(message = "Uploading data!", {
      c_df <-
        if (is.null(input$dat_file)){
          # use example data
          dat_res
        } else {
          read.csv(input$dat_file$datapath)
        }
      # because it reads in dashes as periods
      colnames(c_df)[grepl("naive", colnames(c_df))] <-
        c("growthcleanr-naive_result", "growthcleanr-naive_reason")
      
      # initialize subset
      cleaned_df$full <- cleaned_df$sub <- c_df[, !grepl("Step_", colnames(c_df))]
      
      if (any(grepl("Step_", colnames(c_df)))){
        cleaned_inter_df$full <- cleaned_inter_df$sub <- c_df
      }
    })
  })
  
  # upload result data
  observeEvent(input$upload_inter_res, {
    withProgress(message = "Uploading data!", {
      c_df <-
        if (is.null(input$dat_inter_file)){
          # use example data
          dat_res
        } else {
          read.csv(input$dat_inter_file$datapath)
        }
      # because it reads in dashes as periods
      colnames(c_df)[grepl("naive", colnames(c_df))] <-
        c("growthcleanr-naive_result", "growthcleanr-naive_reason")
      
      # initialize subset
      cleaned_df$full <- cleaned_df$sub <- 
        c_df[, !grepl("Step_", colnames(c_df))]
      
      if (any(grepl("Step_", colnames(c_df)))){
        cleaned_inter_df$full <- cleaned_inter_df$sub <- c_df
      }
    })
  })
  
  # download data results
  output$download_results <- downloadHandler(
    filename = function() {
      if ((is.null(input$dat_file) & is.null(input$dat_inter_file)) |
          input$run_inter_ex){
        paste0(tools::toTitleCase(comp_title),
               "_EHR_Cleaning_Results_data_example.csv")
      } else {
        fn <- ifelse(is.null(input$dat_file),
                     input$dat_inter_file$name,
                     input$dat_file$name
        )
        
        paste0(tools::toTitleCase(comp_title), "_EHR_Cleaning_Results_", fn)
      }
    },
    content = function(file) {
      write.csv(cleaned_df$full, file, row.names = FALSE, na = "")
    }
  )
  
  # download intermediate data results
  output$download_inter_results <- downloadHandler(
    filename = function() {
      if ((is.null(input$dat_file) & is.null(input$dat_inter_file)) |
          input$run_inter_ex){
        paste0(tools::toTitleCase(comp_title),
               "_EHR_Cleaning_Results_w_Intermediate_Values_data_example.csv")
      } else {
        fn <- ifelse(is.null(input$dat_file),
                     input$dat_inter_file$name,
                     input$dat_file$name
        )
        paste0(tools::toTitleCase(comp_title),
               "_EHR_Cleaning_Results_w_Intermediate_Values_", fn)
      }
    },
    content = function(file) {
      write.csv(cleaned_inter_df$full, file, row.names = FALSE, na = "")
    }
  )
  
  # update output to only focus on specified subjects
  observeEvent(input$update_subj, {
    subj <- subj_focus$subj <- strsplit(input$subj_focus, "\n")[[1]]
    cleaned_df$sub <-
      cleaned_df$full[as.character(cleaned_df$full$subj) %in% subj,]
  })
  
  # update output to only focus on specified individual subjects
  observeEvent(input$update_inter_subj, {
    subj_ids <- subj_focus$subj_inter <-
      strsplit(input$subj_inter_focus, "\n")[[1]]
    # get the the subjects - ids will be be baked
    subjids <- sapply(strsplit(subj_ids, "/"), `[[`, 1)
    
    cleaned_inter_df$sub <-
      cleaned_inter_df$full[
        as.character(cleaned_inter_df$full$subj) %in% subjids,]
  })
  
  # reset output to include all subjects
  observeEvent(input$reset_subj, {
    cleaned_df$sub <- cleaned_df$full
    subj_focus$subj <- c()
    
    updateTextAreaInput(session,
                        "subj_focus",
                        value = "")
  })
  
  # reset output to include all subjects for examining subjects
  observeEvent(input$reset_inter_subj, {
    cleaned_inter_df$sub <- cleaned_inter_df$full
    subj_focus$subj_inter <- c()
    
    updateTextAreaInput(session,
                        "subj_inter_focus",
                        value = "")
  })
  
  # click on the height heat map to add subjects and ids to focus
  observeEvent(event_data("plotly_click", source = "all_indiv_heat_ht"), {
    d <- event_data("plotly_click", source = "all_indiv_heat_ht")
    if(!is.null(d)){
      dx <- d$x
      dy <- d$y
      
      # get the heatmap df
      clean_m <- tab_heat_df(cleaned_df$sub,
                             "HEIGHTCM",
                             methods_chosen = methods_chosen$m,
                             sort_col = input$heat_sort_col,
                             hide_agree = input$heat_hide_agree,
                             sort_dec = input$heat_sort_dec,
                             interactive = F,
                             show_y_lab = input$heat_show_y_lab,
                             show_answers = input$heat_show_answers,
                             hl_incorr = input$heat_hl_incorr,
                             reduce_lines = input$heat_reduce_lines,
                             reduce_amount = input$heat_reduce_amount,
                             offset_amount = input$heat_offset_amount)
      # get the info we need
      m <- unique(clean_m$Method)[dx]
      subjid <- clean_m$subjid[dy]
      id <- clean_m$id[dy]
      
      # for the regular tab
      # add ones that were already there (added through other means)
      sub_add <- strsplit(input$subj_focus, "\n")[[1]]
      if (length(sub_add) > 0){
        subj_focus$subj <- sub_add
      } else {
        subj_focus$subj <- c()
      }
      
      # now, automatically add the subject to the focus on list
      subj_focus$subj[length(subj_focus$subj)+1] <- subjid
      subj_focus$subj <- unique(subj_focus$subj)
      
      updateTextAreaInput(session,
                          "subj_focus",
                          value = paste(subj_focus$subj, collapse = "\n"))
      
      # add to the individual subject text area
      
      # add ones that were already there (added through other means)
      sub_add <- strsplit(input$subj_inter_focus, "\n")[[1]]
      if (length(sub_add) > 0){
        subj_focus$subj_inter <- sub_add
      } else {
        subj_focus$subj_inter <- c()
      }
      
      # now, automatically add the subject to the focus on list
      subj_focus$subj_inter[length(subj_focus$subj_inter)+1] <-
        paste0(subjid, "/", id)
      subj_focus$subj_inter <- unique(subj_focus$subj_inter)
      
      updateTextAreaInput(
        session,
        "subj_inter_focus",
        value = paste(subj_focus$subj_inter, collapse = "\n")
      )
    }
  })
  
  # click on the weight heat map to add subjects and ids to focus
  observeEvent(event_data("plotly_click", source = "all_indiv_heat_wt"), {
    d <- event_data("plotly_click", source = "all_indiv_heat_wt")
    if(!is.null(d)){
      dx <- d$x
      dy <- d$y
      
      # get the heatmap df
      clean_m <- tab_heat_df(cleaned_df$sub,
                             "WEIGHTKG",
                             methods_chosen = methods_chosen$m,
                             sort_col = input$heat_sort_col,
                             hide_agree = input$heat_hide_agree,
                             sort_dec = input$heat_sort_dec,
                             interactive = F,
                             show_y_lab = input$heat_show_y_lab,
                             show_answers = input$heat_show_answers,
                             hl_incorr = input$heat_hl_incorr,
                             reduce_lines = input$heat_reduce_lines,
                             reduce_amount = input$heat_reduce_amount,
                             offset_amount = input$heat_offset_amount)
      
      # get the info we need
      m <- unique(clean_m$Method)[dx]
      subjid <- clean_m$subjid[dy]
      id <- clean_m$id[dy]
      
      # replace ones that were already there (added through other means)
      sub_add <- strsplit(input$subj_focus, "\n")[[1]]
      if (length(sub_add) > 0){
        subj_focus$subj <- sub_add
      } else {
        subj_focus$subj <- c()
      }
      
      # now, automatically add the subject to the focus on list
      subj_focus$subj[length(subj_focus$subj)+1] <- subjid
      subj_focus$subj <- unique(subj_focus$subj)
      
      updateTextAreaInput(session,
                          "subj_focus",
                          value = paste(subj_focus$subj, collapse = "\n"))
      
      # add to the individual subject text area
      
      # add ones that were already there (added through other means)
      sub_add <- strsplit(input$subj_inter_focus, "\n")[[1]]
      if (length(sub_add) > 0){
        subj_focus$subj_inter <- sub_add
      } else {
        subj_focus$subj_inter <- c()
      }
      
      # now, automatically add the subject to the focus on list
      subj_focus$subj_inter[length(subj_focus$subj_inter)+1] <-
        paste0(subjid, "/", id)
      subj_focus$subj_inter <- unique(subj_focus$subj_inter)
      
      updateTextAreaInput(
        session,
        "subj_inter_focus",
        value = paste(subj_focus$subj_inter, collapse = "\n")
      )
    }
  })
  
  # update output to only focus on specified methods
  observeEvent(input$update_methods, {
    methods_chosen$m <- input$togg_methods
  })
  
  observeEvent(input$add_subj_focus, {
    # add ones that were already there (added through other means)
    sub_add <- strsplit(input$subj_focus, "\n")[[1]]
    if (length(sub_add) > 0){
      subj_focus$subj <- sub_add
    } else {
      subj_focus$subj <- c()
    }
    
    subj_focus$subj[length(subj_focus$subj)+1] <- input$subj
    subj_focus$subj <- unique(subj_focus$subj)
    
    updateTextAreaInput(session,
                        "subj_focus",
                        value = paste(subj_focus$subj, collapse = "\n"))
  })
  
  output$download_focus <- downloadHandler(
    filename = function() {
      if ((is.null(input$dat_file) & is.null(input$dat_inter_file)) |
          input$run_ex){
        paste0(tools::toTitleCase(comp_title), 
               "_EHR_Cleaning_Subject_Focus_List_Example_Data.csv")
      } else {
        fn <- ifelse(is.null(input$dat_file),
                     input$dat_inter_file$name,
                     input$dat_file$name
        )
        paste0(tools::toTitleCase(comp_title),
               "_EHR_Cleaning_Subject_Focus_List_", fn)
      }
    },
    content = function(file) {
      write.csv(data.frame("Focus.Subjects" = subj_focus$subj),
                file, row.names = FALSE, na = "")
    }
  )
  
  output$download_inter_focus <- downloadHandler(
    filename = function() {
      if ((is.null(input$dat_file) & is.null(input$dat_inter_file)) |
          input$run_inter_ex){
        paste0(tools::toTitleCase(comp_title), 
               "_EHR_Cleaning_Subject_ID_Focus_List_Example_Data.csv")
      } else {
        fn <- ifelse(is.null(input$dat_file),
                     input$dat_inter_file$name,
                     input$dat_file$name
        )
        paste0(tools::toTitleCase(comp_title),
               "_EHR_Cleaning_Subject_ID_Focus_List_", fn)
      }
    },
    content = function(file) {
      write.csv(data.frame("Focus.Subjects.IDs" = subj_focus$subj_inter),
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
    ht_tab <- tab_clean_res(cleaned_df$sub, "HEIGHTCM", methods_chosen$m,
                            input$show_perc_bar)
    plot_bar(ht_tab, input$togg_res_count)
  })
  
  output$overall_wt <- renderPlotly({
    wt_tab <- tab_clean_res(cleaned_df$sub, "WEIGHTKG", methods_chosen$m,
                            input$show_perc_bar)
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
                      methods_chosen = methods_chosen$m,
                      show_heat_corr = input$show_heat_corr)
  })
  
  output$overall_corr_wt <- renderPlotly({
    plot_methods_corr(cleaned_df$sub, "WEIGHTKG",
                      methods_chosen = methods_chosen$m,
                      show_heat_corr = input$show_heat_corr)
  })
  
  # plot individual results ----
  
  output$indiv_choose <- renderUI({
    subj_list <- if (nrow(cleaned_df$sub) == 0){
      c()
    } else {
      if (!input$show_indiv_impl){
        unique(cleaned_df$sub$subjid)
      } else {
        unique(cleaned_df$sub$subjid[rowSums(cleaned_df$sub == "Implausible",
                                             na.rm = T) > 1])
      }
    }
    
    selectInput(
      "subj",
      label = HTML("<p style = 'font-weight: normal'><b>Which subject's cleaned data would you like to visualize?</b> Search for subjects by pressing backspace and typing.</p>"),
      choices = subj_list
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
  
  output$all_indiv_legn <- renderPlot({
    plot_result_heat_map(cleaned_df$sub,
                         "HEIGHTCM",
                         methods_chosen = methods_chosen$m,
                         sort_col = input$heat_sort_col,
                         hide_agree = input$heat_hide_agree,
                         sort_dec = input$heat_sort_dec,
                         interactive = F,
                         show_y_lab = input$heat_show_y_lab,
                         show_answers = input$heat_show_answers,
                         hl_incorr = input$heat_hl_incorr,
                         reduce_lines = input$heat_reduce_lines,
                         reduce_amount = input$heat_reduce_amount,
                         offset_amount = input$heat_offset_amount,
                         legn = T)
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
                         hl_incorr = input$heat_hl_incorr,
                         reduce_lines = input$heat_reduce_lines,
                         reduce_amount = input$heat_reduce_amount,
                         offset_amount = input$heat_offset_amount)
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
                         hl_incorr = input$heat_hl_incorr,
                         reduce_lines = input$heat_reduce_lines,
                         reduce_amount = input$heat_reduce_amount,
                         offset_amount = input$heat_offset_amount)
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
                         hl_incorr = input$heat_hl_incorr,
                         reduce_lines = input$heat_reduce_lines,
                         reduce_amount = input$heat_reduce_amount,
                         offset_amount = input$heat_offset_amount)
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
                         hl_incorr = input$heat_hl_incorr,
                         reduce_lines = input$heat_reduce_lines,
                         reduce_amount = input$heat_reduce_amount,
                         offset_amount = input$heat_offset_amount)
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
                         hl_incorr = input$heat_hl_incorr,
                         reduce_lines = input$heat_reduce_lines,
                         reduce_amount = input$heat_reduce_amount,
                         offset_amount = input$heat_offset_amount)
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
                         hl_incorr = input$heat_hl_incorr,
                         reduce_lines = input$heat_reduce_lines,
                         reduce_amount = input$heat_reduce_amount,
                         offset_amount = input$heat_offset_amount)
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
    subj_list <- if (nrow(cleaned_df$sub) == 0){
      c()
    } else {
      if (!input$show_indiv_inter_impl){
        unique(cleaned_df$sub$subjid)
      } else {
        unique(cleaned_df$sub$subjid[rowSums(cleaned_df$sub == "Implausible",
                                             na.rm = T) > 1])
      }
    }
    
    selectInput(
      "inter_subj",
      label = HTML("<p style = 'font-weight: normal'><b>Which subject's intermediate steps would you like to examine?</b> Search for subjects by pressing backspace and typing. Focus IDs, if added to focus list and updated, will be circled points in plot.</p>"),
      choices = subj_list
    )
  })
  
  # we're only going to render the iteration steps for growthcleanr naive
  # DO NOT DELETE YET
  output[["growthcleanr-naive_iter_step_ui"]] <- renderUI({
    sliderInput(
      "growthcleanr-naive_iter_step",
      "Choose Iteration:",
      min = 1,
      max = 1,
      value = 1
    )
  })
  
  # update the iteration count
  observeEvent(input[["growthcleanr-naive_method_step"]], {
    if (input[["growthcleanr-naive_method_step"]] %in% c("Before", "After")){
      updateSliderInput(
        session,
        "growthcleanr-naive_iter_step",
        "Choose Iteration:",
        min = 1,
        max = 1,
        value = 1
      )
    } else {
      # get the maximum iterations
      step <- as.character(input[["growthcleanr-naive_method_step"]])
      # values we want to focus on in the table (second clause counts alpha char)
      step_focus <- 
        if (grepl("h", step) & nchar(gsub("\\d", "",  step)) == 1){
          "HEIGHTCM"
        } else if (grepl("w", step) & nchar(gsub("\\d", "",  step)) == 1){
          "WEIGHTKG"
        } else {
          c("HEIGHTCM", "WEIGHTKG")
        }
      
      # subset the data to the subject, type, and methods we care about
      clean_df <- sub_subj_type(cleaned_inter_df$sub,
                                step_focus, input$inter_subj,
                                "growthcleanr-naive",
                                m_types = m_inter_types)
      tab_out <- clean_df[,colnames(clean_df)[
        grepl("_Iter_", colnames(clean_df))]]
      # remove all NA columns
      tab_out <- tab_out[, sapply(tab_out, function(x){ !all(is.na(x)) })]
      # get all possible iterations
      iters <- as.numeric(substring(
        gsub(paste0("growthcleanr-naive_Step_", step,"_Iter_"),
             "", colnames(tab_out)),
        1, 1))
      
      updateSliderInput(
        session,
        "growthcleanr-naive_iter_step",
        "Choose Iteration:",
        min = 1,
        max = max(iters),
        step = 1,
        value = 1
      )
    }
  })
  
  lapply(paste0(methods_inter_avail, "_step_title"), function(x){
    output[[x]] <- renderUI({
      ms <-  as.character(
        input[[paste0(tolower(input$inter_tabset),"_method_step")]]
      )
      
      HTML(paste0(
        "<h3><center>",
        if (ms == "Before"){
          "Before Method"
        } else if (ms == "After"){
          "After Method"
        } else {
          paste(
            "Step",
            m_inter_steps_full_title[[tolower(input$inter_tabset)]][ms]
          )
        },
        "</h3></center>"
      ))
    })
  })
  
  lapply(paste0(methods_inter_avail, "_step_subtitle"), function(x){
    output[[x]] <- renderUI({
      if (!input[[paste0(tolower(input$inter_tabset), "_method_step")]] %in%
          c("Before", "After")){
        ms <- as.character(input[[paste0(tolower(input$inter_tabset),
                                         "_method_step")]])
        
        HTML(paste0(
          "<h4><center>",
          m_inter_steps_full_subtitle[[tolower(input$inter_tabset)]][ms],
          "</h4></center>"
          
        ))
      }
    })
  })
  
  lapply(paste0(methods_inter_avail, "_inter_plot"), function(x){
    output[[x]] <- renderPlotly({
      # get possible ids to focus on
      subj_ids <- subj_focus$subj_inter
      if (length(subj_ids) > 0){
        subjids <- sapply(strsplit(subj_ids, "/"), `[[`, 1)
        # take caution if only a subject is entered
        ids <- sapply(strsplit(subj_ids, "/"), function(x){
          if (length(x) < 2){ "" } else { x[[2]] }
        })
        # get ids to focus on
        focus_ids <- ids[subjids == input$inter_subj]
        if (all(focus_ids == "")){
          focus_ids <- c()
        }
      } else {
        focus_ids <- c()
      }
      
      plot_inter_cleaned(cleaned_inter_df$sub, input$inter_subj,
                         step = as.character(
                           input[[paste0(tolower(input$inter_tabset),
                                         "_method_step")]]
                         ),
                         methods_chosen = tolower(input$inter_tabset),
                         focus_ids = focus_ids,
                         color_ans = input$inter_color_ans,
                         iter_step =
                           if (tolower(input$inter_tabset) ==
                               "growthcleanr-naive"){
                             input[["growthcleanr-naive_iter_step"]]
                           } else if (tolower(input$inter_tabset) ==
                                      "muthalagu") {
                             input$muthalagu_bucket_step
                           } else {1})
    })
  })
  
  lapply(paste0(methods_inter_avail, "_inter_plot_legn"), function(x){
    output[[x]] <- renderPlot({
      plot_inter_cleaned(cleaned_inter_df$sub, input$inter_subj,
                         step = as.character(
                           input[[paste0(tolower(input$inter_tabset),
                                         "_method_step")]]
                         ),
                         methods_chosen = tolower(input$inter_tabset),
                         focus_ids = c(),
                         color_ans = input$inter_color_ans,
                         iter_step =
                           if (tolower(input$inter_tabset) ==
                               "growthcleanr-naive"){
                             input[["growthcleanr-naive_iter_step"]]
                           } else {1},
                         legn = T)
    })
  })
  
  lapply(paste0(methods_inter_avail, "_inter_table"), function(x){
    output[[x]] <- renderTable({
      d <- suppressWarnings(event_data("plotly_hover", source = "inter_plot"))
      hover_id <- if (!is.null(d)){ d$customdata } else { "none" }
      
      tab_inter_vals(cleaned_inter_df$sub, input$inter_subj,
                     step = as.character(
                       input[[paste0(tolower(input$inter_tabset),
                                     "_method_step")]]
                     ),
                     methods_chosen = tolower(input$inter_tabset),
                     highlt = hover_id,
                     color_ans = input$inter_color_ans,
                     iter_step =
                       if (tolower(input$inter_tabset) ==
                           "growthcleanr-naive"){
                         input[["growthcleanr-naive_iter_step"]]
                       } else if (tolower(input$inter_tabset) ==
                                  "muthalagu") {
                         input$muthalagu_bucket_step
                       } else {1}
      )
    },
    striped = TRUE,
    bordered = TRUE,
    sanitize.text.function = function(x){x},
    width = '100%',
    colnames = FALSE)
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
