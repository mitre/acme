# Adult growthcleanr Explorer
# By Hannah De los Santos
# Originated on: 10/7/2020

# This implements a prototype application to explore adult EHR cleaning 
# implementations.

vers_adult_ehr <- "0.3.1"

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

# capitalize first letter of words, from ?toupper, edited to handle vector
simpleCap <- function(y) {
  sapply(y, function(x){
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
  }, USE.NAMES = F)
}

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

methods_inter_avail <- c("cheng", "chan")

# types cleaned for each method
m_inter_types <- list(
  "HEIGHTCM" = c("cheng", "chan"),
  "WEIGHTKG" = c("cheng", "chan")
)

methods_inter_func <- list(cheng_clean_both,
                           chan_clean_both)
names(methods_inter_func) <- methods_inter_avail

# list of steps for each method
m_inter_steps <- list(
  "cheng" = c("1h", "2h", "1w", "2w", "3"),
  "chan" = c("1h", "2h", "1w", "2w", "3w")
)

m_inter_steps_full_title <- list(
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
  )
)

m_inter_steps_full_subtitle <- list(
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
  )
)

# processing functions ----

# function to tabulate results of a given height or weight
tab_clean_res <- function(cleaned_df, type, methods_chosen = methods_avail){
  m_for_type <- m_types[[type]][m_types[[type]] %in% methods_chosen]
  
  if (length(m_for_type) == 0){
    return(data.frame())
  }
  
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

# function to calculate total possible answers
tot_poss_answers <- function(cleaned_df, type){
  return(
    HTML(paste0(
      "<center><h4>",
      "Total Possible Correct: ", sum(cleaned_df$param == type), "<br>",
      "Total Possible Correct, Include: ", 
      sum(cleaned_df$param == type & cleaned_df$answers == "Include"), "<br>",
      "Total Possible Correct, Implausible: ",
      sum(cleaned_df$param == type & cleaned_df$answers == "Implausible"), "<br>",
      "<h4></center>"
    ))
  )
}

# function to tabulate results as compared to answers for a given height or weight
tab_answers <- function(cleaned_df, type, 
                        methods_chosen = methods_avail, 
                        group = F){
  m_for_type <- m_types[[type]][m_types[[type]] %in% methods_chosen]
  
  if (length(m_for_type) == 0 ||
      nrow(cleaned_df) == 0 || 
      !any(grepl("answers", colnames(cleaned_df)))){
    return(data.frame())
  }
  
  # preallocate final data frame
  t_tab <- data.frame(matrix(
    0, 
    nrow = length(m_for_type)*if(group){2} else {1},
    ncol = if (group){ 4 }else{ 3 }))
  
  res_type <- c("Include", "Implausible")
  
  if (group){
    colnames(t_tab) <- c("Method","Answer","Count","Percent")
    t_tab$Method <- rep(simpleCap(m_for_type), each = 2)
    t_tab$Answer <- rep(res_type, length(m_for_type))
    rownames(t_tab) <- paste0(rep(m_for_type, each = 2), "_", t_tab$Answer)
  } else {
    colnames(t_tab) <- c("Method","Count","Percent")
    t_tab$Method <- simpleCap(m_for_type)
    rownames(t_tab) <- m_for_type
  }
  
  # tabulate results
  param_log <- cleaned_df$param == type
  for (m in m_for_type){
    if (group){
      for (r in res_type){
        all_log <- param_log & cleaned_df$answers == r
        
        t_tab[paste0(m, "_", r),"Count"] <- sum(
          cleaned_df[all_log, "answers"] == 
            cleaned_df[all_log, paste0(m, "_result")]
        )
        t_tab[paste0(m, "_", r),"Percent"] <- 
          t_tab[paste0(m, "_", r),"Count"]/sum(all_log)*100
      }
    } else {
      t_tab[m,"Count"] <- sum(
        cleaned_df[param_log, "answers"] == 
          cleaned_df[param_log, paste0(m, "_result")]
      )
      t_tab[m,"Percent"] <- t_tab[m,"Count"]/sum(param_log)*100
    }
  }
  
  return(t_tab)
}

# function to tabulate reasons for implausibility for a given height or weight
tab_clean_reason <- function(cleaned_df, type, 
                             show_count = F, 
                             methods_chosen = methods_avail){
  m_for_type <- m_types[[type]][m_types[[type]] %in% methods_chosen]
  
  if (nrow(cleaned_df) == 0 | 
      all(cleaned_df[cleaned_df$param == type,] != "Implausible") |
      length(m_for_type) == 0){
    return(data.frame())
  }
  
  tot_tab <- data.frame()
  for (m in m_for_type){
    # subsetting the result reasons
    criteria <- cleaned_df[paste0(m, "_reason")] != "" & 
      cleaned_df$param == type
    
    tmp_tab <- as.data.frame(
      table(cleaned_df[criteria, paste0(m, "_reason")])
    )
    tmp_tab <- tmp_tab[order(tmp_tab$Freq, decreasing = T),]
    
    if (length(tmp_tab) == 0){
      tmp_tab <- data.frame(
        "Var1" = "",
        "Freq" = 0
      )
    }
    
    colnames(tmp_tab) <- paste0(simpleCap(m),
                                c("_Reason", "_Count"))
    tmp_tab$row <- 1:nrow(tmp_tab)
    
    # merge the reasons together, with padding
    tot_tab <- 
      if (nrow(tot_tab) == 0){
        # rearrange by default
        tmp_tab[,c(3, 1, 2)]
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

# function to subset cleaned data based on subject and type
sub_subj_type <- function(cleaned_df, type, subj,
                          methods_chosen = methods_avail,
                          m_types = m_types){
  result_map <- c(
    "TRUE" = "Include",
    "FALSE" = "Implausible"
  )
  
  m_for_type <- unique(
    unlist(m_types[type])[unlist(m_types[type]) %in% methods_chosen]
  )
  
  if (length(m_for_type) == 0 | nrow(cleaned_df) == 0){
    return(data.frame())
  }
  
  # subset the data to the things we care about
  clean_df <- cleaned_df[cleaned_df$subjid == subj & 
                           cleaned_df$param %in% type,]
  # subset to only the methods included
  clean_df <- clean_df[
    ,
    (!grepl("_result", colnames(clean_df)) &
       !grepl("_reason", colnames(clean_df)) &
       !grepl("_Step", colnames(clean_df))) |
      (colnames(clean_df) %in% paste0(m_for_type, "_reason") |
         colnames(clean_df) %in% paste0(m_for_type, "_result") |
         # when using _step, we only ever need the first one
         grepl(paste0(m_for_type, "_Step")[1], colnames(clean_df)))
  ]
  
  # create counts for plotting
  clean_df$num_implausible <- clean_df$sum_implausible <-
    rowSums((clean_df[,paste0(m_for_type, "_result"), drop = F] != "Include"))
  clean_df$sum_include <-
    rowSums((clean_df[,paste0(m_for_type, "_result"), drop = F] != "Implausible"))
  # for all include, make 0 -> 1 for plotting
  clean_df$num_implausible[clean_df$num_implausible == 0] <- 1
  # aggregate all the results
  clean_df$all_result <- 
    result_map[as.character(
      apply(clean_df[,paste0(m_for_type, "_result"), drop = F] == "Include", 1, all)
    )]
  # aggregate all the methods
  clean_df$all_include <- apply(
    clean_df[,paste0(m_for_type, "_result"), drop = F],
    1,
    function(x){
      paste(simpleCap(m_for_type[x == "Include"]), 
            collapse = ", ")
    })
  
  clean_df$all_implausible <- apply(
    clean_df[,paste0(m_for_type, "_result"), drop = F],
    1,
    function(x){
      paste(simpleCap(m_for_type[x == "Implausible"]), 
            collapse = ", ")
    })
  
  # gather all the reasons for implausibility
  clean_df$all_reason <-
    apply(clean_df[,paste0(m_for_type, "_reason"), drop = F], 1, function(x){
      nam <- simpleCap(m_for_type)[x != ""]
      x_wo <- paste0(nam,": ", x[x != ""])
      x_wo <- paste(x_wo, collapse = "\n")
      return(if (x_wo == ": "){""} else {x_wo})
    })
  
  
  return(clean_df)
}

# function to create the dataframe for the heatmap
tab_heat_df <- function(cleaned_df, type,
                        methods_chosen = methods_avail,
                        sort_col = "none",
                        sort_dec = F,
                        hide_agree = F, 
                        interactive = F,
                        show_y_lab = F,
                        show_answers = T,
                        hl_incorr = T,
                        reduce_lines = F,
                        reduce_amount = nrow(cleaned_df),
                        offset_amount = 0){
  # if show answers is true and there are no answers, fix that
  if (show_answers & !"answers" %in% colnames(cleaned_df)){
    show_answers <- F
  }
  
  number_map <- 
    if (show_answers){
      c(
        "Incorrect: Include (False Negative)" = 0,
        "Correct: Include (True Negative)" = 1,
        "Incorrect: Implausible (False Positive)" = 2,
        "Correct: Implausible (True Positive)" = 3
      )
    } else {
      c(
        "Include" = 0,
        "Implausible" = 1
      )
    }
  
  # get the possible methods for this type
  m_for_type <- m_types[[type]][m_types[[type]] %in% methods_chosen]
  
  # subset the data to the things we care about
  clean_df <- cleaned_df[cleaned_df$param == type,]
  # subset to only the methods included
  clean_df <- clean_df[
    ,
    (!grepl("_result", colnames(clean_df)) &
       !grepl("_reason", colnames(clean_df))) |
      (colnames(clean_df) %in% paste0(m_for_type, "_reason") |
         colnames(clean_df) %in% paste0(m_for_type, "_result"))
  ]
  
  if (nrow(clean_df) == 0 | reduce_amount <= 0){
    return(data.frame())
  }
  
  # only keep the results and necessary sorting
  clean_df <- 
    clean_df[,
             !(grepl("_reason", colnames(clean_df)) | 
                 grepl("param", colnames(clean_df)))
    ]
  
  # if we're going to show answers, we want to change the names  
  if (show_answers){
    # get the results as compared to the answers
    ans_res <- 
      clean_df[, grepl("_result", colnames(clean_df))] == clean_df$answers
    ans_incl <- clean_df$answers == "Include"
    ans_impl <- clean_df$answers == "Implausible"
    # add correct/incorrect + fp/tp/fn/tn
    clean_df[, grepl("_result", colnames(clean_df))][ans_res] <- 
      paste0(
        "Correct: ", clean_df[, grepl("_result", colnames(clean_df))][ans_res]
      )
    clean_df[, grepl("_result", colnames(clean_df))][ans_res & ans_incl] <-
      paste0(
        clean_df[, grepl("_result", colnames(clean_df))][ans_res & ans_incl],
        " (True Negative)"
      )
    clean_df[, grepl("_result", colnames(clean_df))][ans_res & ans_impl] <-
      paste0(
        clean_df[, grepl("_result", colnames(clean_df))][ans_res & ans_impl],
        " (True Positive)"
      )
    
    clean_df[, grepl("_result", colnames(clean_df))][!ans_res] <- 
      paste0(
        "Incorrect: ", clean_df[, grepl("_result", colnames(clean_df))][!ans_res]
      )
    clean_df[, grepl("_result", colnames(clean_df))][(!ans_res) & ans_impl] <-
      paste0(
        clean_df[, grepl("_result", colnames(clean_df))][(!ans_res) & ans_impl],
        " (False Negative)"
      )
    clean_df[, grepl("_result", colnames(clean_df))][(!ans_res) & ans_incl] <-
      paste0(
        clean_df[, grepl("_result", colnames(clean_df))][(!ans_res) & ans_incl],
        " (False Positive)"
      )
  }
  
  # if we choose to remove where they all agree, do so!
  if (hide_agree){
    # result columns
    res_col <- grepl("_result", colnames(clean_df))
    
    if (!show_answers){
      clean_df <- clean_df[rowSums(clean_df[, res_col] != "Include") > 0,]
    } else {
      clean_df <- 
        clean_df[rowSums(
          clean_df[, res_col] != "Correct: Include" & 
            clean_df[, res_col] != "Correct: Implausible"
        ) > 0,]
    }
  }
  
  # sort for visualizing
  if (!"none" %in% sort_col){
    sort_col <- sort_col[sort_col != "none"]
    
    clean_df <- 
      clean_df[do.call('order', 
                       c(clean_df[sort_col], list(decreasing = sort_dec))),]
  }
  
  # create label column (combining all the non result columns)
  clean_df$Label <- trimws(apply(
    clean_df[, !grepl("_result", colnames(clean_df))],
    1,
    paste,
    collapse = " / "
  ))
  lab <- paste(
    colnames(clean_df[, !(grepl("_result", colnames(clean_df)) | 
                            grepl("Label", colnames(clean_df)))]),
    collapse = " / "
  )
  # remove the sort columns (keep subject and id)
  clean_df <- clean_df[, (grepl("_result", colnames(clean_df)) | 
                            grepl("Label", colnames(clean_df)) |
                            grepl("subjid", colnames(clean_df)) |
                            grepl("id", colnames(clean_df)))]
  # add label name
  clean_df$Label_Name <- lab
  # rename result columns
  colnames(clean_df)[grepl("_result", colnames(clean_df))] <-
    simpleCap(
      gsub("_result", "", 
           colnames(clean_df)[grepl("_result", colnames(clean_df))])
    )
  
  # reduce lines, if specified
  if (reduce_lines){
    if (reduce_amount > nrow(clean_df)){
      reduce_amount <- nrow(clean_df)
    }
    if (offset_amount > nrow(clean_df)){
      offset_amount <- nrow(clean_df) - 1
      reduce_amount <- nrow(clean_df)
    }
    
    clean_df <- clean_df[(offset_amount+1):(offset_amount+1 + reduce_amount), ]
  }
  
  if (nrow(clean_df) > 0){
    clean_m <- melt(clean_df, id.vars = c("Label", "subjid", "id", "Label_Name"), variable.name = "Method")
    clean_m$Label <- factor(clean_m$Label, levels = unique(clean_m$Label))
    
    # convert the text to numbers
    clean_m[, "value"] <- number_map[clean_m$value]
  } else {
    clean_m <- clean_df
  }
  
  return(clean_m)
}

# function to build a table of intermediate values depending on a step and 
# method chosen
tab_inter_vals <- function(cleaned_df, subj, step,
                           methods_chosen = methods_inter_avail[1],
                           highlt = "none"){
  type_map <- c(
    "HEIGHTCM" = "Height (cm)",
    "WEIGHTKG" = "Weight (kg)"
  )
  
  result_map <- c(
    "TRUE" = "Implausible",
    "FALSE" = "Include",
    "Implausible" = "Implausible",
    "Include" = "Include",
    "Not Calculated" = "Not Calculated"
  )
  
  # values we want to focus on in the table
  step_focus <- 
    if (grepl("h", step)){
      "HEIGHTCM"
    } else if (grepl("w", step)){
      "WEIGHTKG"
    } else {
      c("HEIGHTCM", "WEIGHTKG")
    }
  
  # subset the data to the subject, type, and methods we care about
  clean_df <- sub_subj_type(cleaned_df, step_focus, subj, methods_chosen, 
                            m_types = m_inter_types)
  
  if (nrow(clean_df) == 0){
    return(data.frame())
  }
  
  tab_out <- clean_df[,c("id", "param", "age_years", "measurement")]
  tab_out <- cbind(
    tab_out, 
    if (step == "Before"){
      rep("Include", nrow(tab_out))
    } else if (step == "After"){
      clean_df[, paste0(methods_chosen, "_result")]
    } else {
      clean_df[,colnames(clean_df)[
        grepl(paste0("_Step_", step), colnames(clean_df))]]
    }
  )
  colnames(tab_out)[ncol(tab_out)] <- 
    paste0(methods_chosen, "_Step_", step, "_Result")
  
  # prettify certain columns
  tab_out[, grepl("_Result", colnames(tab_out))] <- 
    result_map[as.character(tab_out[, grepl("_Result", colnames(tab_out))])]
  tab_out$param <- type_map[tab_out$param]
  tab_out[,sapply(tab_out, class) == "numeric"] <- 
    round(tab_out[,sapply(tab_out, class) == "numeric"], 3)
  
  # transpose for output
  tab_out <- as.data.frame(t(tab_out))
  # add a column for names
  step_names <- gsub(
    paste0(methods_chosen, " Step ", step, " "), 
    "",
    gsub("_", " ", rownames(tab_out)[-c(1:4)])
  )
  
  tab_out <- cbind(
    "names" = paste0(
      "<strong><p align = 'right'>",
      c("ID", "Parameter", "Age (years)", "Measurement", step_names),
      "</strong></p>"
    ),
    tab_out
  )
  
  # need to remove leading/trailing whitespace
  tab_out <- as.data.frame(
    apply(tab_out, 2, function(x){trimws(x, which = "both")})
  )
  
  # highlight a specific column
  if (highlt != "none" & highlt %in% clean_df$id){
    highlt <- as.character(highlt)
    tab_out[, tab_out["id",] == highlt] <- 
      paste0("<strong>", tab_out[, tab_out["id",] == highlt], "</strong>")
  }
  
  return(tab_out)
}

# plotting/output functions ----

# Function to extract legend
# https://stackoverflow.com/questions/12041042/how-to-plot-just-the-legends-in-ggplot2
g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  legend
} 

# function to plot overall bar plot
plot_bar <- function(t_tab, yval = "Implausible"){
  if (nrow(t_tab) > 0){
    t_tab$Method <- factor(t_tab$Method, levels = unique(t_tab$Method))
    
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
    ) %>% config(displayModeBar = F)
  } else {
    ggplotly(ggplot()+theme_bw()) %>% config(displayModeBar = F)
  }
}


# function to plot check answers bar plot
plot_answer_bar <- function(t_tab, yval = "Count", 
                            group = F, ontop = F, legn = F){
  if (nrow(t_tab) > 0){
    t_tab$Method <- factor(t_tab$Method, levels = unique(t_tab$Method))
    
    fill_color <-
      if (group){
        c("Include" = "#b2abd2","Implausible" = "#fdb863")
      } else {
        m_colors
      }
    
    p <- 
      if (group){
        ggplot(t_tab, aes_string("Method", yval, fill = "Answer", group = "Answer"))+
          theme_bw()+
          if (ontop){
            geom_bar(stat = "identity")
          } else {
            geom_bar(stat = "identity", position = position_dodge(.9))
          }
      } else {
        ggplot(t_tab, aes_string("Method", yval, fill = "Method"))+
          theme_bw()+
          geom_bar(stat = "identity")+
          theme(legend.position = "none")
      }
    
    p <- p +
      scale_fill_manual(values = fill_color)+
      scale_y_continuous(
        expand = expansion(mult = c(0,.05))) +
      ylab(paste(yval, " Correct"))+
      NULL
    
    p <- p + 
      if (!legn){
        theme(legend.position = "none")
      } else {
        theme(legend.position = "bottom",
              legend.direction = "horizontal",
              text = element_text(size = 15))
      }
    
    p <- 
      if (!legn | !group){
        ggplotly(
          p,
          tooltip = c("x","y","fill")
        ) %>% config(displayModeBar = F)
      } else {
        as.ggplot(g_legend(p))
      }
  } else {
    p <- 
      if (!legn){
        ggplotly(ggplot()+theme_bw()) %>% config(displayModeBar = F)
      } else {
        ggplot()+theme_bw()
      }
  }
  
  return(p)
}

# function to generate the overall title for each shiny tab
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

# function to plot individual heights and weights
plot_cleaned <- function(cleaned_df, type, subj, 
                         methods_chosen = methods_avail,
                         show_fit_line = T, show_sd_shade = T, 
                         calc_fit_w_impl = F, legn = F, 
                         single = F){
  # the minimum +/- value around the mean for the y axis to show
  min_range_band <- c(
    "HEIGHTCM" = 3.5,
    "WEIGHTKG" = 5
  )
  
  # maps for configuring ggplot
  color_map <- c(
    "Include" = "#000000",
    "Implausible" = "#fdb863"
  )
  
  shape_map <- c(
    "Include" = 16,
    "Implausible" = 17
  )
  
  type_map <- c(
    "HEIGHTCM" = "Height (cm)",
    "WEIGHTKG" = "Weight (kg)"
  )
  
  # subset the data to the subject, type, and methods we care about
  # also create necessary counts for plotting and such
  clean_df <- sub_subj_type(cleaned_df, type, subj, methods_chosen,
                            m_types = m_types)
  
  # get the possible methods for this type
  m_for_type <- m_types[[type]][m_types[[type]] %in% methods_chosen]
  
  if (nrow(clean_df) == 0){
    if (legn){
      return(ggplot()+theme_bw())
    } else {
      return(ggplotly(ggplot()+theme_bw()) %>% config(displayModeBar = F))
    }
  }
  
  if (nrow(clean_df) == 1){
    show_fit_line <- F
    show_sd_shade <- F
    calc_fit_w_impl <- F
  }
  
  bf_df <- data.frame(
    "age_years" = c(
      clean_df$age_years,
      min(clean_df$age_years)-(diff(range(clean_df$age_years))*.05),
      max(clean_df$age_years)+(diff(range(clean_df$age_years))*.05)
    ),
    "measurement_orig" = c(
      clean_df$measurement,
      rep(NA,2)
    )
  )
  
  if (nrow(clean_df) > 1){
    # if user wants to show the line fit
    # add best fit line (padded slightly for plotting prettiness)
    bf_df$best_fit <- 
      if (calc_fit_w_impl){
        predict(lm(measurement ~ age_years, clean_df), bf_df)
      } else {
        predict(lm(measurement ~ age_years, clean_df, 
                   subset = clean_df$all_result == "Include"), 
                bf_df)
      }
    
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
  
  # consider the y range to be, at a minimum, a certain amount around the mean
  min_rg <- min(bf_df[,-1], na.rm = T)
  max_rg <- max(bf_df[,-1], na.rm = T)
  yaxis_lim <- 
    if (diff(c(min_rg, max_rg)) < (min_range_band[type]*2)){
      c(
        mean(c(min_rg, max_rg))-min_range_band[type],
        mean(c(min_rg, max_rg))+min_range_band[type]
      )
    } else {
      c(
        min_rg-(.01*diff(c(min_rg,max_rg))), 
        max_rg+(.01*diff(c(min_rg,max_rg)))
      )
    }
  
  p <- ggplot()
  
  p <- p +
    if (single){
      ggtitle(paste0("Method: ", simpleCap(methods_chosen)))
    } else {
      ggtitle(paste0("Subject: ", subj))
    }
  
  if (show_fit_line){
    p <- p +
      geom_line(data = bf_df, 
                aes(x = age_years, y = best_fit), 
                size = 1, linetype = "longdash", 
                color = "#3F8FB4")+
      scale_x_continuous(expand = expansion(mult = c(0,0)))
    
  }
  
  if (show_sd_shade){
    p <- p +
      geom_ribbon(
        data = bf_df, 
        aes(x = age_years, ymin = min_sd1, ymax = max_sd1),
        fill = "#80C4EA", alpha = .5)+ # grey70 old
      geom_ribbon(
        data = bf_df, 
        aes(x = age_years,  ymin = min_sd2, ymax = max_sd2), 
        fill = "#80C4EA", alpha = .2)
  }
  
  # make the scatter plot (applies in all situations)
  p <- suppressWarnings(
    p +
      geom_point(
        data = clean_df, 
        aes(
          age_years, measurement,
          color = all_result, shape = all_result,
          size = num_implausible,
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
      scale_shape_manual("Result", values = shape_map, breaks = names(shape_map))+
      scale_size(
        "Count Implausible", 
        range = c(1,length(m_for_type)), 
        limits = c(1,length(m_for_type)), 
        breaks = c(1:length(m_for_type))
      )+
      ylim(yaxis_lim)+
      theme(plot.title = element_text(hjust = .5))+
      xlab("Age (Years)")+
      ylab(type_map[type])+
      NULL
  )
  
  if (legn){
    p <- p +
      theme(legend.position = "bottom",
            text = element_text(size = 15))
    
    return(as.ggplot(g_legend(p)))
  } else {
    p <- p +
      theme(legend.position = "none")
    
    return(ggplotly(p, tooltip = c("text")) %>% config(displayModeBar = F))
  }
  
}

# function to generate the summary that appears below individual subject plots
gen_subj_text <- function(cleaned_df, type, subj,
                          methods_chosen = methods_avail,
                          single = F){
  # subset the data to the subject, type, and methods we care about
  clean_df <- sub_subj_type(cleaned_df, type, subj, methods_chosen,
                            m_types = m_types)
  
  if (nrow(clean_df) == 0){
    return(HTML(""))
  }
  
  m_for_type <- m_types[[type]][m_types[[type]] %in% methods_chosen]
  
  impl_by_method <- sapply(m_for_type, function(x){
    paste0("<li><b>Total Implausible by ", simpleCap(x),": </b>",
           sum(clean_df[,paste0(x,"_result")] == "Implausible"), "</li>")
  })
  impl_by_method <- paste0(
    "<ul>",
    paste(impl_by_method, collapse = ""),
    "</ul>"
  )
  
  incl_by_method <- sapply(m_for_type, function(x){
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
  
  if (!single){
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
  } else {
    return(
      HTML(paste0(
        "<b>Subject: </b>", subj,"<br>",
        "<b>Number of Records: </b>", nrow(clean_df),"<br>",
        incl_by_method,
        impl_by_method,
        "<b>Reasons for Implausibility: </b><br>",
        reason_text
      ))
    )
  }
}

# function to generate correlation plots for methods (overall plot)
plot_methods_corr <- function(cleaned_df, type,
                              methods_chosen = methods_avail){
  
  type_n <- c(
    "HEIGHTCM" = "Height",
    "WEIGHTKG" = "Weight"
  )
  
  # get the possible methods for this type
  m_for_type <- m_types[[type]][m_types[[type]] %in% methods_chosen]
  
  # subset the data to the things we care about
  clean_df <- cleaned_df[cleaned_df$param == type,]
  # subset to only the methods included
  clean_df <- clean_df[
    ,
    (!grepl("_result", colnames(clean_df)) &
       !grepl("_reason", colnames(clean_df))) |
      (colnames(clean_df) %in% paste0(m_for_type, "_reason") |
         colnames(clean_df) %in% paste0(m_for_type, "_result"))
  ]
  
  if (nrow(clean_df) == 0){
    return(ggplotly(ggplot()+theme_bw()))
  }
  
  # get columns for correlation
  corr_df <- clean_df[, grepl("_result", colnames(clean_df))]
  colnames(corr_df) <- simpleCap(gsub("_result","",colnames(corr_df)))
  corr_df[corr_df == "Include"] <- 0
  corr_df[corr_df == "Implausible"] <- 1
  
  # compute correlation
  corr_df <- sapply(corr_df, as.numeric)
  corr_df <- cor(corr_df)
  corr_df[lower.tri(corr_df)] <- NA
  
  # melt into long form for ggplot
  corr_df <- melt(corr_df)
  colnames(corr_df) <- c("Method.1", "Method.2", "Correlation")
  corr_df$Correlation <- signif(corr_df$Correlation, 3)
  
  # create correlation heat map
  p <- ggplotly(
    ggplot(corr_df, aes(Method.1, Method.2,
                        fill = Correlation,
                        label = Correlation))+
      geom_tile()+
      geom_text(size = 3)+
      scale_fill_gradient2(
        breaks = c(-1,0,1),
        limits = c(-1,1),
        low = "#0571b0", mid = "#f7f7f7", high = "#ca0020")+
      theme_bw()+
      scale_x_discrete(expand = c(0,0))+
      scale_y_discrete(expand = c(0,0))+
      theme(axis.title = element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            plot.title = element_text(hjust = .5))+
      ggtitle(paste0("Correlation of Implausible Values for ", type_n[type]))+
      NULL
  ) %>% config(displayModeBar = F)
  
  return(p)
}

# function to create a heat map of results for methods (specific tab)
# y labels will not be shown if there are more than 100 records
# interactivity will not be shown if number of records and methods chosen exceeds
# 1500
# CONSIDER AN OVERRIDE?
plot_result_heat_map <- function(cleaned_df, type,
                                 methods_chosen = methods_avail,
                                 sort_col = "none",
                                 sort_dec = F,
                                 hide_agree = F, 
                                 interactive = F,
                                 show_y_lab = F,
                                 show_answers = T,
                                 hl_incorr = T,
                                 reduce_lines = F,
                                 reduce_amount = nrow(cleaned_df),
                                 offset_amount = 0,
                                 legn = F){
  type_n <- c(
    "HEIGHTCM" = "Height",
    "WEIGHTKG" = "Weight"
  )
  
  # if show answers is true and there are no answers, fix that
  if (show_answers & !"answers" %in% colnames(cleaned_df)){
    show_answers <- F
  }
  
  number_map <- 
    if (show_answers){
      c(
        "Incorrect: Include (False Negative)" = 0,
        "Correct: Include (True Negative)" = 1,
        "Incorrect: Implausible (False Positive)" = 2,
        "Correct: Implausible (True Positive)" = 3
      )
    } else {
      c(
        "Include" = 0,
        "Implausible" = 1
      )
    }
  num_to_lab <- names(number_map)
  names(num_to_lab) <- number_map
  
  color_map <- 
    if (show_answers){
      # if we're highlighting incorrect answers, make those brighter
      if (hl_incorr){
        c(
          "Incorrect: Include (False Negative)" = "#5e3c99",
          "Correct: Include (True Negative)" = "#b2abd2",
          "Incorrect: Implausible (False Positive)" = "#e66101",
          "Correct: Implausible (True Positive)" = "#fdb863"
        )
      } else {
        c(
          "Correct: Include (True Negative)" = "#5e3c99",
          "Incorrect: Include (False Negative)" = "#b2abd2",
          "Correct: Implausible (True Positive)" = "#e66101",
          "Incorrect: Implausible (False Positive)" = "#fdb863"
        )
      }
    } else {
      c(
        "Include" = "#b2abd2",
        "Implausible" = "#fdb863"
      )
    }
  if (!legn){
    names(color_map) <- number_map[names(color_map)]
  }
  
  # we're going to make a fake dataset for legend purposes
  if (legn){
    df <- data.frame(
      "fill" = names(color_map),
      "y" = names(color_map),
      "x" = rep("a", length(color_map))
    )
    
    p <- ggplot(df, 
                aes(x, y, fill = fill))+
      theme_bw()+
      scale_x_discrete(expand = c(0,0))+
      scale_y_discrete(expand = c(0,0))+
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.title = element_blank())+
      scale_fill_discrete(type = color_map)+ 
      geom_tile(color = "black") +
      theme(legend.position = "bottom",
            legend.direction = "horizontal",
            text = element_text(size = 15))
    
    return(as.ggplot(g_legend(p)))
  }
  
  clean_m <- tab_heat_df(cleaned_df = cleaned_df, 
                         type = type,
                         methods_chosen = methods_chosen,
                         sort_col = sort_col,
                         sort_dec = sort_dec,
                         hide_agree = hide_agree, 
                         interactive = interactive,
                         show_y_lab = show_y_lab,
                         show_answers = show_answers,
                         hl_incorr = hl_incorr,
                         reduce_lines = reduce_lines,
                         reduce_amount = reduce_amount,
                         offset_amount = offset_amount)

  if (nrow(clean_m) == 0){
    if (interactive){
      return(ggplotly(ggplot()+theme_bw()+ggtitle("No entries.")))
    } else {
      return(ggplot()+theme_bw()+ggtitle("No entries."))
    }
  }
  
  # get the possible methods for this type
  m_for_type <- m_types[[type]][m_types[[type]] %in% methods_chosen]
  
  p <- ggplot(clean_m, 
              aes(Method, Label, fill = value,
                  text = paste0(
                    "Method: ", Method,"\n",
                    "Label: ", Label,"\n",
                    "Result: ", num_to_lab[as.character(value)]
                  )))+
    theme_bw()+
    scale_fill_gradientn(
      colors = color_map, 
      breaks = as.numeric(names(color_map)),
      limits = c(min(as.numeric(names(color_map))), 
                 max(as.numeric(names(color_map))))
    )+
    scale_x_discrete(expand = c(0,0))+
    scale_y_discrete(expand = c(0,0))+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.title = element_blank())+
    ylab(paste("Record:", unique(clean_m$Label_Name)))+
    theme(legend.position = "none")+
    NULL
  
  if (!show_y_lab | length(unique(clean_m$Label)) > 100){
    p <- p + 
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  }
  
  # if there aren't a ton of entries, you can add grids
  if (length(unique(clean_m$Label))*length(m_for_type) < 250*6){
    p <- p + 
      geom_tile(color = "white")
  } else {
    p <- p + 
      geom_tile()
  }
  
  if (interactive){
    # if there are too many entries, the plotly won't render
    if (length(unique(clean_m$Label))*length(m_for_type) > 250*6){
      p <- ggplotly(
        ggplot()+
          theme_bw()+
          ggtitle(
            "Too many entries. Reduce the amount of subjects by focusing\non a subset, reduce number of entries in sidebar, or remove\ninteractivity in the sidebar."
          )
      ) %>%
        layout(margin = list(t = 100)) %>% 
        config(displayModeBar = F)
    } else {
      p <- suppressWarnings({
        ggplotly(
          p,
          source = paste0(
            "all_indiv_heat",
            ifelse(type == "HEIGHTCM", "_ht", "_wt")
            ),
          tooltip = c("text")
        ) %>%
          layout(
            legend = list(orientation = "h",   # show entries horizontally
                          xanchor = "center",  
                          x = 0.5,
                          y = 1.1)) %>% 
          style(xgap = 1, ygap = 1) %>%
          event_register("plotly_click") %>%
          config(displayModeBar = F)
      })
    }
  } else {
    p <- p + theme(text = element_text(size = 15))
  }
  
  return(p)
}

# function to plot individual heights and weights for intermediate values at
# a specific step for a specific method
plot_inter_cleaned <- function(cleaned_df, subj, step,
                               methods_chosen = methods_inter_avail[1],
                               focus_ids = c(),
                               legn = F){
  # the minimum +/- value around the mean for the y axis to show
  min_range_band <- c(
    "HEIGHTCM" = 3.5,
    "WEIGHTKG" = 5
  )
  
  # maps for configuring ggplot
  color_map <- c(
    "Include" = "#000000",
    "Not Calculated" = "#000000",
    "Implausible" = "#fdb863"
  )
  
  shape_map <- c(
    "Include" = 16,
    "Not Calculated" = 16,
    "Implausible" = 17
  )
  
  size_map <- c(
    "Include" = 2,
    "Not Calculated" = 2,
    "Implausible" = 3
  )
  
  type_map <- c(
    "HEIGHTCM" = "Height (cm)",
    "WEIGHTKG" = "Weight (kg)"
  )
  
  result_map <- c(
    "TRUE" = "Implausible",
    "FALSE" = "Include",
    "Not Calculated" = "Not Calculated"
  )
  
  opposite_focus_map <- c(
    "Height (cm)" = "w",
    "Weight (kg)" = "h"
  )
  
  type <- c("HEIGHTCM", "WEIGHTKG")
  step_focus <- type_map[
    if (grepl("h", step)){
      "HEIGHTCM"
    } else if (grepl("w", step)){
      "WEIGHTKG"
    } else {
      c("HEIGHTCM", "WEIGHTKG")
    }
  ]
  
  # subset the data to the subject, type, and methods we care about
  # also create necessary counts for plotting and such
  clean_df <- sub_subj_type(cleaned_df, type, subj, methods_chosen, 
                            m_types = m_inter_types)
  
  # get the possible methods for this type
  m_for_type <- unique(
    unlist(m_types[type])[unlist(m_types[type]) %in% methods_chosen]
  )
  
  if (nrow(clean_df) == 0){
    if (legn){
      return(ggplot()+theme_bw())
    } else {
      return(ggplotly(ggplot()+theme_bw()) %>% config(displayModeBar = F))
    }
  }
  
  bf_df <- data.frame(
    "id" = clean_df$id,
    "age_years" = clean_df$age_years,
    "param" = type_map[clean_df$param],
    "measurement" = clean_df$measurement,
    "step_result" = 
      if (step == "Before"){
        rep("Include", nrow(clean_df))
      } else if (step == "After"){
        clean_df$all_result
      } else {
        result_map[
          as.character(
            clean_df[, paste0(methods_chosen, "_Step_", step, "_Result")]
          )
        ]
      }
  )
  # if it's the end, we want to make all the implausible NA
  if (step == "After"){
    bf_df$step_result[bf_df$step_result == "Implausible"] <- NA
  }
  bf_df[is.na(bf_df$step_result) & bf_df$param %in% step_focus,
        "step_result"] <- "Implausible"
  # for the parameter not in focus, we want the last result
  if (length(step_focus) == 1){
    # get all the steps for the method
    all_steps <- m_inter_steps[[methods_chosen]]
    all_op_steps <- which(
      grepl(opposite_focus_map[step_focus], 
            all_steps[c(1:(which(all_steps == step))-1)]
      )
    )
    last_op_step <- 
      if (length(all_op_steps) == 0) {
        "Before"
      } else {
        all_steps[all_op_steps[length(all_op_steps)]]
      }
    
    # add the values for the last step
    foc_log <- !bf_df$param %in% step_focus
    
    bf_df[foc_log, "step_result"] <- 
      if (last_op_step == "Before"){
        rep("Include", sum(foc_log))
      } else if (last_op_step == "After"){
        clean_df$all_result[!type_map[clean_df$param] %in% step_focus]
      } else {
        result_map[
          as.character(
            clean_df[!type_map[clean_df$param] %in% step_focus, 
                     paste0(methods_chosen, "_Step_", last_op_step, "_Result")]
          )
        ]
      }
    
    bf_df$step_result_orig[foc_log] <- bf_df$step_result[foc_log]
    bf_df$step_result_orig[foc_log][
      bf_df$step_result[foc_log] == "Implausible"] <- 
      NA
    bf_df[is.na(bf_df$step_result) & foc_log,
          "step_result"] <- "Implausible"
  }
  
  # consider the y range to be, at a minimum, a certain amount around the mean
  scales_y <- list()
  for (t in type){
    foc_log <- bf_df$param == type_map[t]
    if (sum(foc_log) > 0){
      min_rg <- min(bf_df$measurement[foc_log], na.rm = T)
      max_rg <- max(bf_df$measurement[foc_log], na.rm = T)
      yaxis_lim <- 
        if (diff(c(min_rg, max_rg)) < (min_range_band[t]*2)){
          c(
            mean(c(min_rg, max_rg))-min_range_band[t],
            mean(c(min_rg, max_rg))+min_range_band[t]
          )
        } else {
          c(
            min_rg-(.01*diff(c(min_rg,max_rg))), 
            max_rg+(.01*diff(c(min_rg,max_rg)))
          )
        }
      
      scales_y[[type_map[t]]] <- scale_y_continuous(limits = yaxis_lim)
    }
  }
  
  p <- suppressWarnings(
    ggplot(bf_df, aes(customdata = id))+
      geom_line(
        data = bf_df[bf_df$step_result %in% c("Include", "Not Calculated"),], 
        aes(age_years, measurement), color = "grey")+
      geom_point(
        aes(
          age_years, measurement,
          color = step_result, shape = step_result,
          size = step_result,
          text = paste0(
            "ID: ", id,"\n",
            "Step ", step, " Result: ", step_result,"\n"
          )
        )
      )+
      theme_bw()+
      scale_color_manual("Result", values = color_map, breaks = names(color_map))+
      scale_shape_manual("Result", values = shape_map, breaks = names(shape_map))+
      scale_size_manual("Result", values = size_map, breaks = names(shape_map))+
      theme(plot.title = element_text(hjust = .5))+
      xlab("Age (Years)")+
      ylab("Measurement")+
      facet_grid_sc(rows = vars(param), scales = list(y = scales_y))+
      NULL
  )
  
  if (sum(bf_df$id %in% focus_ids) > 0){
    p <- p +
      geom_point(
        data = bf_df[bf_df$id %in% focus_ids,],
        aes(
          age_years, measurement,
          color = step_result
        ), 
        fill = NA, shape = 21, size = 6#, color = "pink"
      )
  }
  
  if (legn){
    p <- p +
      theme(legend.position = "bottom",
            text = element_text(size = 15))
    
    return(as.ggplot(g_legend(p)))
  } else {
    p <- p +
      theme(legend.position = "none")
    
    return(
      ggplotly(p, tooltip = c("text"), source = "inter_plot") %>%
        config(displayModeBar = F) %>%
        event_register("plotly_hover")
    )
  }
}

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
            HTML("<b>Upload adult EHR data or results and click the corresponding button below to get started!</b> If no data is input, default synthetic data/results will be used. More information on data format can be found in the \"About\" tab. Note that this will update data in \"Examine Methods\" tab.<p>"),
            fileInput("dat_file", "Upload Data/Results CSV",
                      accept = c(".csv", ".CSV")),
            checkboxInput(
              "run_ex", 
              HTML("<b>Run example data?</b>"),
              value = F
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
        HTML("<b>Upload adult EHR data or results and click the corresponding button below to understand intermediate values!</b> If no data is input, default synthetic data/results will be used. More information on data format can be found in the \"About\" tab. Note that this will update data in \"Compare Results\" tab.<p>"),
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
        uiOutput("indiv_inter_choose")
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
                HTML("</center>"),
                uiOutput(paste0(m_name, "_step_title")),
                uiOutput(paste0(m_name, "_step_subtitle")),
                plotlyOutput(paste0(m_name, "_inter_plot")),
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
                "<li><b>age_years:</b> age in years (ages < 18 will be filtered out) (can also be <b>agedays</b>, as in the original growthcleanr format, and will be automatically converted to years)</li>",
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
      # check that df has age_years or age days, preferring age_years
      if ("agedays" %in% colnames(df) & !"age_years" %in% colnames(df)){
        df$age_years <- df$agedays /365.25
      }
      # fix id if not unique or if it doesn't exist
      if (is.null(df$id) || length(unique(df$id)) != nrow(df)){
        df$id <- 1:nrow(df)
      }
      # filter out data less than 18
      df <- df[df$age_years >= 18, ]
      
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
      if ((is.null(input$dat_file) & is.null(input$dat_inter_file)) |
          input$run_inter_ex){
        "Adult_EHR_Cleaning_Results_data_example.csv"
      } else {
        fn <- ifelse(is.null(input$dat_file), 
                     input$dat_inter_file$name,
                     input$dat_file$name
        )
        
        paste0("Adult_EHR_Cleaning_Results_", fn)
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
        "Adult_EHR_Cleaning_Results_w_Intermediate_Values_data_example.csv"
      } else {
        fn <- ifelse(is.null(input$dat_file), 
                     input$dat_inter_file$name,
                     input$dat_file$name
        )
        paste0("Adult_EHR_Cleaning_Results_w_Intermediate_Values_", fn)
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
        "Adult_EHR_Cleaning_Subject_Focus_List_Example_Data.csv"
      } else {
        fn <- ifelse(is.null(input$dat_file), 
                     input$dat_inter_file$name,
                     input$dat_file$name
                     )
        paste0("Adult_EHR_Cleaning_Subject_Focus_List_", fn)
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
        "Adult_EHR_Cleaning_Subject_ID_Focus_List_Example_Data.csv"
      } else {
        fn <- ifelse(is.null(input$dat_file), 
                     input$dat_inter_file$name,
                     input$dat_file$name
        )
        paste0("Adult_EHR_Cleaning_Subject_ID_Focus_List_", fn)
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
    selectInput(
      "inter_subj",
      label = HTML("<p style = 'font-weight: normal'><b>Which subject's intermediate steps would you like to examine?</b> Search for subjects by pressing backspace and typing. Focus IDs, if added to focus list and updated, will circled points in plot.</p>"),
      choices = 
        if (nrow(cleaned_inter_df$sub) == 0){
          c()
        } else {
          unique(cleaned_inter_df$sub$subjid)
        }
    )
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
                         focus_ids = focus_ids)
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
                     highlt = hover_id)
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
