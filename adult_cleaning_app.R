# Adult growthcleanr Explorer
# By Hannah De los Santos
# Originated on: 10/7/2020

# This implements a prototype application to explore adult EHR cleaning 
# implementations.

vers_adult_ehr <- "0.1.0"

# load libraries, scripts, and data ----

library(shiny)
library(ggplot2)
library(rstudioapi)
library(colorspace)
library(plotly)
library(viridisLite)
library(ggplotify)

#https://stackoverflow.com/questions/3452086/getting-path-of-an-r-script/35842176#35842176
# set working directory - only works in RStudio (with rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load default data - fake 
# dat <- read.csv(file.path("Data", "adult_synthetic_data_seed_8.csv"))
dat <- read.csv(file.path("Data", "synthea-adults-sub-100subj.csv"))

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

methods_avail <- c("muthalagu", "cheng", "chan")

# types cleaned for each method
m_types <- list(
  "HEIGHTCM" = methods_avail,
  "WEIGHTKG" = methods_avail[-1]
)

methods_func <- list(muthalagu_clean_ht,
                     cheng_clean_both,
                     chan_clean_both)
names(methods_func) <- methods_avail

# capitalize first letter of words, from ?toupper, edited to handle vector
simpleCap <- function(y) {
  sapply(y, function(x){
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
  }, USE.NAMES = F)
}

# method colors
m_colors <- viridisLite::viridis(length(methods_avail))
names(m_colors) <- simpleCap(methods_avail)

# supporting functions ----

# Function to extract legend
# https://stackoverflow.com/questions/12041042/how-to-plot-just-the-legends-in-ggplot2
g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  legend
} 

# function to tabulate results of a given height or weight
tab_clean_res <- function(cleaned_df, type){
  m_for_type <- m_types[[type]]
  
  # preallocate final data frame
  t_tab <- data.frame(matrix(0, 
                             nrow = length(m_for_type),
                             ncol = 3))
  colnames(t_tab) <- c("Method","Implausible","Include")
  t_tab$Method <- simpleCap(m_for_type)
  
  # tabulate results
  rownames(t_tab) <- m_for_type
  for (m in m_for_type){
    tab <- table(cleaned_df[cleaned_df$param == type, paste0(m, "_result")])
    t_tab[m,names(tab)] <- tab
  }
  
  return(t_tab)
}

# function to plot overall hist
plot_hist <- function(t_tab, yval = "Implausible"){
  ggplotly(
    ggplot(t_tab, aes_string("Method", yval, fill = "Method"))+
      geom_bar(stat = "identity")+
      theme_bw()+
      # scale_fill_discrete_qualitative(palette = "Dark 3")+
      scale_fill_manual(values = m_colors)+
      theme(legend.position = "none")+
      scale_y_continuous(expand = expansion(mult = c(0,.05))) +
      NULL,
    tooltip = c("x","y")
  )
}

# function to generate shiny tab title
gen_title <- function(criteria, tab_titl){
  return(
    if (criteria){
      HTML(paste0("<center><h3>",
                  tab_titl, 
                  " Results: Full Data</center></h3>"))
    } else {
      HTML(paste0("<center><h3>", 
                  tab_titl, 
                  " Results: Subset Data</center></h3>"))
    }
  )
}

# function to tabulate reasons for a given height or weight
tab_clean_reason <- function(cleaned_df, type, show_count = F){
  if (nrow(cleaned_df) == 0 | all(cleaned_df[cleaned_df$param == type,] != "Implausible")){
    return(data.frame())
  }
  
  m_for_type <- m_types[[type]]
  
  tot_tab <- data.frame()
  for (m in m_for_type){
    # subsetting the result reasons
    criteria <- cleaned_df[paste0(m, "_reason")] != "" & 
      cleaned_df$param == type
    
    tmp_tab <- as.data.frame(
      table(cleaned_df[criteria, paste0(m, "_reason")])
      )
    tmp_tab <- tmp_tab[order(tmp_tab$Freq, decreasing = T),]
    colnames(tmp_tab) <- paste0(simpleCap(m),
                                c("_Reason", "_Count"))
    tmp_tab$row <- 1:nrow(tmp_tab)
    
    # merge the reasons together, with padding
    tot_tab <- 
      if (nrow(tot_tab) == 0){
        tmp_tab
      } else {
        merge(tot_tab, tmp_tab, by = "row", all = T)
      }
  }
  # remove the row column
  colnames(tot_tab)[1] <- "Rank"
  
  if (!show_count){
    tot_tab <- tot_tab[,!grepl("Count", colnames(tot_tab))]
  }
  return(tot_tab)
}

# function to subset based on subject and type
sub_subj_type <- function(cleaned_df, type, subj){
  result_map <- c(
    "TRUE" = "Include",
    "FALSE" = "Implausible"
  )
  
  # subset the data to the things we care about
  clean_df <- cleaned_df[cleaned_df$subjid == subj & 
                           cleaned_df$param == type,]
  # subset to only the methods included
  clean_df <- clean_df[
    ,
    (!grepl("_result", colnames(clean_df)) &
       !grepl("_reason", colnames(clean_df))) |
      (colnames(clean_df) %in% paste0(m_types[[type]], "_reason") |
         colnames(clean_df) %in% paste0(m_types[[type]], "_result"))
  ]
  
  # create counts for plotting
  clean_df$num_implausible <- clean_df$sum_implausible <-
    rowSums((clean_df[,paste0(m_types[[type]], "_result")] != "Include"))
  clean_df$sum_include <-
    rowSums((clean_df[,paste0(m_types[[type]], "_result")] != "Implausible"))
  # for all include, make 0 -> 1 for plotting
  clean_df$num_implausible[clean_df$num_implausible == 0] <- 1
  # aggregate all the results
  clean_df$all_result <- 
    result_map[as.character(
      apply(clean_df[,paste0(m_types[[type]], "_result")] == "Include", 1, all)
    )]
  # aggregate all the methods
  clean_df$all_include <- apply(
    clean_df[,paste0(m_types[[type]], "_result")],
    1,
    function(x){
      paste(simpleCap(m_types[[type]][x == "Include"]), 
            collapse = ", ")
    })
  
  clean_df$all_implausible <- apply(
    clean_df[,paste0(m_types[[type]], "_result")],
    1,
    function(x){
      paste(simpleCap(m_types[[type]][x == "Implausible"]), 
            collapse = ", ")
    })
  
  # gather all the reasons for implausibility
  clean_df$all_reason <-
    apply(clean_df[,paste0(m_types[[type]], "_reason")], 1, function(x){
      nam <- simpleCap(m_types[[type]])[x != ""]
      x_wo <- paste0(nam,": ", x[x != ""])
      x_wo <- paste(x_wo, collapse = "\n")
      return(if (x_wo == ": "){""} else {x_wo})
    })
    
  
  return(clean_df)
}

# function to plot individual heights and weights
plot_cleaned <- function(cleaned_df, type, subj, 
                         show_fit_line = T, show_sd_shade = T, 
                         calc_fit_w_impl = T, legn = F){
  color_map <- c(
    "Include" = "#000000",
    "Implausible" = "#e62315"
  )
  
  type_map <- c(
    "HEIGHTCM" = "Height (cm)",
    "WEIGHTKG" = "Weight (kg)"
  )
  
  # subset the data to the subject, type, and methods we care about
  # also create necessary counts for plotting and such
  clean_df <- sub_subj_type(cleaned_df, type, subj)
  
  if (nrow(clean_df) == 0){
    if (legn){
      return(ggplot()+theme_bw())
    } else {
      return(ggplotly(ggplot()+theme_bw()))
    }
  }
  
  # if user wants to show the line fit
  bf_df <- data.frame()
  if (show_fit_line | show_sd_shade){
    # add best fit line (padded slightly for plotting prettiness)
    bf_df <- data.frame(
      "age_years" = c(
        clean_df$age_years,
        min(clean_df$age_years)-(diff(range(clean_df$measurement))*.05),
        max(clean_df$age_years)+(diff(range(clean_df$measurement))*.05)
      ),
      "measurement_orig" = c(
        clean_df$measurement,
        rep(NA,2)
      )
    )
    
    if (show_fit_line){
      bf_df$best_fit <- 
        if (calc_fit_w_impl){
          predict(lm(measurement ~ age_years, clean_df), bf_df)
        } else {
          predict(lm(measurement ~ age_years, clean_df, 
                     subset = clean_df$all_result == "Include"), 
                  bf_df)
        }
    }
    
    if (show_sd_shade){
      st_dev <- 
      if (calc_fit_w_impl){
        sd(clean_df$measurement)
      } else {
        sd(clean_df$measurement[clean_df$all_result == "Include"])
      }
     
      
      if (show_fit_line){
        bf_df$min_sd1 <- bf_df$best_fit-st_dev
        bf_df$max_sd1 <- bf_df$best_fit+st_dev
        bf_df$min_sd2 <- bf_df$best_fit-(2*st_dev)
        bf_df$max_sd2 <- bf_df$best_fit+(2*st_dev)
      } else {
        bf_df$min_sd1 <- bf_df$measurement_orig-st_dev
        bf_df$max_sd1 <- bf_df$measurement_orig+st_dev
        bf_df$min_sd2 <- bf_df$measurement_orig-(2*st_dev)
        bf_df$max_sd2 <- bf_df$measurement_orig+(2*st_dev)
        
        bf_df <- bf_df[complete.cases(bf_df),]
      }
    }
  }
  
  # make the scatter plot (applies in all situations)
  p <- suppressWarnings(
    ggplot()+
      geom_point(
        data = clean_df, 
        aes(
          age_years, measurement,
          color = all_result, size = num_implausible,
          text = paste0(
            "Subject: ", subjid, "\n",
            "Result: ", all_result,"\n",
            "Include Methods (", sum_include, "): ", all_include, "\n",
            "Implausible Methods (", sum_implausible, "): ", all_implausible, "\n",
            paste0("If Implausible, Reasons:\n", all_reason)
          )
        )
      )+
      theme_bw()+
      scale_color_manual("Result", values = color_map, breaks = names(color_map))+
      scale_size(
        "Count Implausible", 
        range = c(1,3), limits = c(1,3), breaks = c(1:3)
      )+
      # theme(legend.position = "bottom",
      # legend.direction = "horizontal")+
      theme(plot.title = element_text(hjust = .5))+
      xlab("Age (Years)")+
      ylab(type_map[type])+
      ggtitle(paste0("Subject: ", subj))+
      NULL
  )
  
  if (show_fit_line){
    p <- p +
      geom_line(data = bf_df, 
                aes(x = age_years, y = best_fit), 
                size = 1, linetype = "longdash")+
      scale_x_continuous(expand = expansion(mult = c(0,0)))
      
  }
  
  if (show_sd_shade){
    p <- p +
      geom_ribbon(
        data = bf_df, 
        aes(x = age_years, ymin = min_sd1, ymax = max_sd1),
        fill = "grey70", alpha = .5)+
      geom_ribbon(
        data = bf_df, 
        aes(x = age_years,  ymin = min_sd2, ymax = max_sd2), 
        fill = "grey70", alpha = .2)
  }
  
  if (legn){
    p <- p +
      theme(legend.position = "bottom",
            text = element_text(size = 15))
    
    return(as.ggplot(g_legend(p)))
  } else {
    p <- p +
      theme(legend.position = "none")
    
    return(ggplotly(p, tooltip = c("text")))
  }
}

gen_subj_text <- function(cleaned_df, type, subj){
  # subset the data to the subject, type, and methods we care about
  clean_df <- sub_subj_type(cleaned_df, type, subj)
  
  impl_by_method <- sapply(m_types[[type]], function(x){
    paste0("<li><b>Total Implausible by ", simpleCap(x),": </b>",
           sum(clean_df[,paste0(x,"_result")] == "Implausible"), "</li>")
  })
  impl_by_method <- paste0(
    "<ul>",
    paste(impl_by_method, collapse = ""),
    "</ul>"
  )
  
  incl_by_method <- sapply(m_types[[type]], function(x){
    paste0("<li><b>Total Include by ", simpleCap(x),": </b>",
           sum(clean_df[,paste0(x,"_result")] == "Include"), "</li>")
  })
  incl_by_method <- paste0(
    "<ul>",
    paste(incl_by_method, collapse = ""),
    "</ul>"
  )
  
  # compile all the reasons for implausibility
  count_reasons <- table(unlist(strsplit(clean_df$all_reason, "\n")))
  reason_text <- ""
  if (length(count_reasons) > 0){
    reason_text <- "<ul>"
    for (i in 1:length(count_reasons)){
      reason_text <- paste0(
        reason_text, 
        "<li>", names(count_reasons)[i], " (", count_reasons[i], ")</li>"
      )
    }
    reason_text <- paste0(reason_text,"</ul>")
  }
  
  return(
    HTML(paste0(
      "<b>Subject: </b>", subj,"<br>",
      "<b>Number of Records: </b>", nrow(clean_df),"<br>",
      "<b>Total Include (by all methods): </b>",
      sum(clean_df$all_result == "Include"),"<br>",
      incl_by_method,
      "<b>Total Implausible (by any method): </b>",
      sum(clean_df$all_result == "Implausible"),"<br>",
      impl_by_method,
      "<b>Reasons for Implausibility: </b><br>",
      reason_text
    ))
  )
}

# UI ----

# TODO: LOOK INTO MODULES

ui <- navbarPage(
  
  # UI: compute and compare results ----
  
  "Adult EHR Cleaning",
  tabPanel(
    "Compare",
    sidebarLayout(
      sidebarPanel(
        HTML("<b>Upload adult EHR data and click the button below to get started!</b> If no data is input, default synthetic data will be used. More information on data format can be found in the \"About\" tab.<p>"),
        fileInput("dat_file", "Upload Data CSV",
                  accept = c(".csv", ".CSV")),
        div(style="display:inline-block",
            actionButton("run_data", "Run data!"),
            downloadButton("download_results", label = "Download Results")
        ),
        hr(),
        HTML("<b>Settings for all plots:</b><p>"),
        textAreaInput("subj_focus", 
                      "Enter subjects to focus on (line separated):",
                      width = "300px",
                      height = "100px"),
        div(style="display:inline-block",
          actionButton("update_subj", "Update Subjects"),
          actionButton("reset_subj", "Reset")
        ),
        hr(),
        HTML("<b>Settings for overall plots:</b><p>"),
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
        hr(),
        HTML("<b>Settings for individual plots:</b><p>"),
        uiOutput("indiv_choose"),
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
        )
      ),
      mainPanel(tabsetPanel(
        tabPanel(
          "Overall",
          fluidRow(
            width = 12,
            uiOutput("overall_subj_title")
          ),
          fluidRow(
            column(width = 6, style='border-right: 1px solid black', 
              HTML("<h3><center>Height Results</center></h3>"),
              plotlyOutput("overall_ht"),
              hr(),
              HTML("<h4><center><b>Top Reasons for Implausible Values</center></b></h4>"),
              dataTableOutput("overall_ht_top_reasons")
            ),
            column(width = 6, 
              HTML("<h3><center>Weight Results</center></h3>"),
              plotlyOutput("overall_wt"),
              hr(),
              HTML("<h4><center><b>Top Reasons for Implausible Values</center></b></h4>"),
              dataTableOutput("overall_wt_top_reasons")
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
            column(width = 6, style='padding-right: 20px; border-right: 1px solid black',
                   HTML("<h3><center>Height Results</center></h3>"),
                   plotlyOutput("subj_ht"),
                   plotOutput("subj_legn_ht", height = "20px"),
                   hr(),
                   fluidRow(
                     style = "border: 1px #e3e3e3; border-style: solid; border-radius: 10px; background: #f5f5f5; padding: 10px;",
                     uiOutput("about_subj_ht")
                   )
            ),
            column(width = 6, style = "padding-left: 20px;",
                   HTML("<h3><center>Weight Results</center></h3>"),
                   plotlyOutput("subj_wt"),
                   plotOutput("subj_legn_wt", height = "20px"),
                   hr(),
                   fluidRow(
                     style = "border: 1px #e3e3e3; border-style: solid; border-radius: 10px; background: #f5f5f5; padding: 10px;",
                     uiOutput("about_subj_wt")
                   )
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
      ))
    )
  ),
  
  # UI: documentation ----
  
  tabPanel(
    "About",
    mainPanel(
      width = 12,
      tabsetPanel(
        tabPanel(
          "About Adult EHR Cleaning and Data Format",
          fluidRow(
            column(width = 3),
            column(
              width = 6,
              HTML(
                "<center><h3>Welcome to the Adult EHR Cleaning Application!</h3></center><p>",
                "This application seeks to compare different methods of cleaning adult EHR data, implementing a variety of methods. This currently includes Muthalagu, et al., Cheng, et al., and Chan, et al. To find out more about these methods, please click on their respective tabs. More to come soon!<p>",
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
                "</ul><p>",
                "If no data is input, the app will use synthetic data (to find out more about this example data, click on the 'Synthetic Data' tab). Then click run to get started!"
              ),
              column(width = 3)
            )
          )
        ),
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
                "<ul><li>Remove biologically implausible weight records. Heights are biologically implausible if less than 24.9 kg or greater than 453.6 kg.</li></ul>",
                "<b>Step 2w, W compare difference from average to range or SD</b><br>",
                "<ul><li>Weight was determined to be inaccurate if: a) the range was > 22.7 kg AND absolute difference between recorded weight and avg weight was > 70% of range OR b) SD was >20% of the average weight AND absolute difference between that weight and average weight > the SD.</li></ul>",
                "<b>Step 3, BMI BIV</b><br>",
                "<ul><li>Remove biologically implausible BMI records. If BMI for a given set of height/weights is < 12 or > 70, deem implausible.</li></ul>"
              )
            ),
            column(width = 3)
          )
        ),
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
                "<ul><li>Remove biologically implausible weight records. Heights are biologically implausible if less than 22.7 kg or greater than 340.2 kg.</li></ul>",
                "<b>Step 2w, W BMI BIV</b><br>",
                "<ul><li>Calculate BMI based on average height for all weight records, then remove biologically implausible weights. BMIs are biologically implausible if less than 10 or greater than 100.</li></ul>",
                "<b>Step 3w, W check SD away from mean</b><br>",
                "<ul><li>Exclude weights that were greater than 3 standard deviations from the mean.</li></ul>"
              )
            ),
            column(width = 3)
          )
        ),
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
  
  cleaned_df <- reactiveValues(
    "full" = data.frame(),
    "sub" = data.frame()
  )
  
  observeEvent(input$run_data, {
    withProgress(message = "Cleaning data!", value = 0, {
      tot_increments <- 1+1+length(methods_avail)
      
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
      for (m in methods_avail){
        incProgress(1/tot_increments, 
                    message = paste("Running", simpleCap(m)),
                    detail = Sys.time())
        
        # clean data
        clean_df <- methods_func[[m]](df)
        
        # add the results to the overall dataframe
        c_df[,paste0(m, "_result")] <- clean_df$result
        c_df[,paste0(m, "_reason")] <- clean_df$reason
      }
      
      # initialize subset (cleaned_df holds all subjects)
      cleaned_df$full <- cleaned_df$sub <- c_df
    })
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
  })
  
  # plot overall results ----
  
  output$overall_subj_title <- renderUI({
    gen_title(nrow(cleaned_df$full) == nrow(cleaned_df$sub), "Overall")
  })
  
  output$overall_ht <- renderPlotly({
    ht_tab <- tab_clean_res(cleaned_df$sub, "HEIGHTCM")
    plot_hist(ht_tab, input$togg_res_count)
  })
  
  output$overall_wt <- renderPlotly({
    wt_tab <- tab_clean_res(cleaned_df$sub, "WEIGHTKG")
    plot_hist(wt_tab, input$togg_res_count)
  })
  
  output$overall_ht_top_reasons <- renderDataTable({
    tab_clean_reason(cleaned_df$sub, "HEIGHTCM", input$show_reason_count) 
  }, 
  options = list(scrollX = TRUE,
                 pageLength = 5)
  )
  
  output$overall_wt_top_reasons <- renderDataTable({
    tab_clean_reason(cleaned_df$sub, "WEIGHTKG", input$show_reason_count) 
  }, 
  options = list(scrollX = TRUE,
                 pageLength = 5)
  )
  
  # plot individual results ----
  
  output$indiv_choose <- renderUI({
    selectInput(
      "subj",
      label = "Which subject's cleaned data would you like to visualize?",
      choices = 
        if (nrow(cleaned_df$sub) == 0){c()} else {unique(cleaned_df$sub$subjid)}
    )
  })
  
  output$indiv_subj_title <- renderUI({
    gen_title(nrow(cleaned_df$full) == nrow(cleaned_df$sub), "Individual")
  })
  
  output$subj_legn_ht <- renderPlot({
    plot_cleaned(cleaned_df$sub, "HEIGHTCM", input$subj, legn = T)
  })
  
  output$subj_legn_wt <- renderPlot({
    plot_cleaned(cleaned_df$sub, "WEIGHTKG", input$subj, legn = T)
  })
  
  output$subj_ht <- renderPlotly({
    plot_cleaned(cleaned_df$sub, "HEIGHTCM", input$subj, 
                 input$show_fit_line, input$show_sd_shade, 
                 input$calc_fit_w_impl)
  })
  
  output$subj_wt <- renderPlotly({
    plot_cleaned(cleaned_df$sub, "WEIGHTKG", input$subj, 
                 input$show_fit_line, input$show_sd_shade, 
                 input$calc_fit_w_impl)
  })
  
  output$about_subj_ht <- renderUI({
    gen_subj_text(cleaned_df$sub, "HEIGHTCM", input$subj)
  })
  
  output$about_subj_wt <- renderUI({
    gen_subj_text(cleaned_df$sub, "WEIGHTKG", input$subj)
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
      "Synthetic data was generated by <a href='https://synthetichealth.github.io/synthea/' target = 'blank'>Synthea</a> for ", length(unique(dat$subjid)), " subjects and ", nrow(dat), " records, with ages ranging from ", min(dat$age_years), " to ", max(dat$age_years), ". Descriptive data plots are below.<p>"
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
    )
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
    )
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
    )
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
    )
  })
}

# RUN ----

shinyApp(ui, server)