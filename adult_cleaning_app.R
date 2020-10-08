# Adult growthcleanr Explorer
# By Hannah De los Santos
# Originated on: 10/7/2020

# This implements a prototype application to explore adult EHR cleaning 
# implementations.

# load libraries, scripts, and data ----

library(shiny)
library(ggplot2)
library(rstudioapi)

#https://stackoverflow.com/questions/3452086/getting-path-of-an-r-script/35842176#35842176
# set working directory - only works in RStudio (with rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load data - fake for now, will get a better
dat <- read.csv(file.path("Data", "adult_synthetic_data_seed_8.csv"))

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

methods_avail <- c("muthalagu", "cheng", "chan")
methods_func <- list(muthalagu_clean_ht,
                     cheng_clean_both,
                     chan_clean_both)
names(methods_func) <- methods_avail

# supporting functions ----

# capitalize first letter of words, from ?toupper
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

# UI ----

ui <- navbarPage(
  "Adult EHR Cleaning",
  tabPanel(
    "Compare",
    sidebarLayout(
      sidebarPanel(
        HTML("<b>Upload adult EHR data and click the button below to get started!</b> If no data is input, default synthetic data will be used. More information on data format can be found in the \"About\" tab.<p>"),
        fileInput("dat_file", "Upload Data CSV",
                  accept = c(".csv", ".CSV")),
        actionButton("go", "Run data!"),
        hr()
      ),
      mainPanel()
    )
  ),
  tabPanel(
    "About",
    "put some stuff about data here"
  )
)

# SERVER ----

server <- function(input, output, session) {
  
  cleaned_df <- data.frame()
  observeEvent(input$go, {
    withProgress(message = "Cleaning data!", value = 0, {
      tot_increments <- 1+1+length(methods_avail)
      
      incProgress(1/tot_increments, message = "Uploading data!")
      
      df <-
        if (is.null(input$dat_file)){
          # use example data
          dat
        } else {
          read.csv(input$dat_file$datapath)
        }
      
      # run each method
      cleaned_df <- df
      for (m in methods_avail){
        incProgress(1/tot_increments, message = paste("Running", simpleCap(m)))
        
        # clean data
        clean_df <- methods_func[[m]](df)
        
        # add the results to the overall dataframe
        cleaned_df[,paste0(m, "_result")] <- clean_df$result
        cleaned_df[,paste0(m, "_reason")] <- clean_df$reason
      }
    })
  })
}

# RUN ----

shinyApp(ui, server)