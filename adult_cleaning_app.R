# Adult growthcleanr Explorer
# By Hannah De los Santos
# Originated on: 10/7/2020

# This implements a prototype application to explore adult EHR cleaning 
# implementations.

# load libraries, scripts, and data ----

library(shiny)
library(ggplot2)
library(rstudioapi)
library(colorspace)
library(plotly)

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

# supporting functions ----

# capitalize first letter of words, from ?toupper, edited to handle vector
simpleCap <- function(y) {
  sapply(y, function(x){
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
  }, USE.NAMES = F)
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
plot_hist <- function(t_tab){
  ggplotly(
    ggplot(t_tab, aes(Method, Implausible, fill = Method))+
      geom_bar(stat = "identity")+
      theme_bw()+
      # scale_fill_discrete_qualitative(palette = "Dark 3")+
      scale_fill_viridis_d()+
      theme(legend.position = "none")+
      scale_y_continuous(expand = expansion(mult = c(0,.05))) +
      NULL,
    tooltip = c("x","y")
  )
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
        actionButton("run_data", "Run data!"),
        hr(),
        HTML("<b>Settings for all plots:</b><p>"),
        textAreaInput("subj_focus", 
                      "Enter subjects to focus on (line separated):",
                      width = "200px",
                      height = "100px"),
        div(style="display:inline-block",
          actionButton("update_subj", "Update Subjects"),
          actionButton("reset_subj", "Reset")
        )
      ),
      mainPanel(tabsetPanel(
        tabPanel(
          "Overall",
          fluidRow(
            width = 12,
            uiOutput("subj_title")
          ),
          fluidRow(
            column(width = 6, {
              plotlyOutput("overall_ht")
            }),
            column(width = 6, {
              plotlyOutput("overall_wt")
            })
          )
        )
      ))
    )
  ),
  tabPanel(
    "About",
    "put some stuff about data here"
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
      
      incProgress(1/tot_increments, message = "Uploading data!")
      
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
        incProgress(1/tot_increments, message = paste("Running", simpleCap(m)))
        
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
  
  # plot results ----
  
  output$subj_title <- renderUI({
    if (nrow(cleaned_df$full) == nrow(cleaned_df$sub)){
      HTML("<center><h3>Overall Results: Full Data</center></h3>")
    } else {
      HTML("<center><h3>Overall Results: Subset Data</center></h3>")
    }
  })
  
  output$overall_ht <- renderPlotly({
    ht_tab <- tab_clean_res(cleaned_df$sub, "HEIGHTCM")
    plot_hist(ht_tab)
  })
  
  output$overall_wt <- renderPlotly({
    wt_tab <- tab_clean_res(cleaned_df$sub, "WEIGHTKG")
    plot_hist(wt_tab)
  })
}

# RUN ----

shinyApp(ui, server)