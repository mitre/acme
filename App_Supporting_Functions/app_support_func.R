# Supporting Functions for Adult growthcleanr Explorer
# By Hannah De los Santos
# Originated on: 1/6/2021

# About: this lists supporting functions for the adult growthcleanr explorer app,
# moved from the main app.R for ease of use.

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
                          methods_chosen = methods_avail){
  result_map <- c(
    "TRUE" = "Include",
    "FALSE" = "Implausible"
  )
  
  m_for_type <- m_types[[type]][m_types[[type]] %in% methods_chosen]
  
  if (length(m_for_type) == 0 | nrow(cleaned_df) == 0){
    return(data.frame())
  }
  
  # subset the data to the things we care about
  clean_df <- cleaned_df[cleaned_df$subjid == subj & 
                           cleaned_df$param == type,]
  # subset to only the methods included
  clean_df <- clean_df[
    ,
    (!grepl("_result", colnames(clean_df)) &
       !grepl("_reason", colnames(clean_df))) |
      (colnames(clean_df) %in% paste0(m_for_type, "_reason") |
         colnames(clean_df) %in% paste0(m_for_type, "_result"))
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
          theme(legend.position = "bottom",
                legend.direction = "horizontal",
                text = element_text(size = 15))+
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
  clean_df <- sub_subj_type(cleaned_df, type, subj, methods_chosen)
  
  # get the possible methods for this type
  m_for_type <- m_types[[type]][m_types[[type]] %in% methods_chosen]
  
  if (nrow(clean_df) == 0){
    if (legn){
      return(ggplot()+theme_bw())
    } else {
      return(ggplotly(ggplot()+theme_bw()) %>% config(displayModeBar = F))
    }
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
  clean_df <- sub_subj_type(cleaned_df, type, subj, methods_chosen)
  
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
      geom_text()+
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
                                 hl_incorr = T){
  type_n <- c(
    "HEIGHTCM" = "Height",
    "WEIGHTKG" = "Weight"
  )
  
  # if show answers is true and there are no answers, fix that
  if (show_answers & !"answers" %in% colnames(cleaned_df)){
    show_answers <- F
  }
  
  color_map <- 
    if (show_answers){
      # if we're highlighting incorrect answers, make those brighter
      if (hl_incorr){
        c(
          "Include (Incorrect)" = "#5e3c99",
          "Include (Correct)" = "#b2abd2",
          "Implausible (Incorrect)" = "#e66101",
          "Implausible (Correct)" = "#fdb863"
        )
      } else {
        c(
          "Include (Correct)" = "#5e3c99",
          "Include (Incorrect)" = "#b2abd2",
          "Implausible (Correct)" = "#e66101",
          "Implausible (Incorrect)" = "#fdb863"
        )
      }
    } else {
      c(
        "Include" = "#b2abd2",
        "Implausible" = "#fdb863"
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
  
  if (nrow(clean_df) == 0){
    return(ggplotly(ggplot()+theme_bw()))
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
    # add correct/incorrect
    clean_df[, grepl("_result", colnames(clean_df))][ans_res] <- 
      paste0(
        clean_df[, grepl("_result", colnames(clean_df))][ans_res], " (Correct)"
      )
    clean_df[, grepl("_result", colnames(clean_df))][!ans_res] <- 
      paste0(
        clean_df[, grepl("_result", colnames(clean_df))][!ans_res], " (Incorrect)"
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
          clean_df[, res_col] != "Include (Correct)" & 
            clean_df[, res_col] != "Implausible (Correct)"
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
  # remove the sort columns
  clean_df <- clean_df[, (grepl("_result", colnames(clean_df)) | 
                            grepl("Label", colnames(clean_df)))]
  # rename result columns
  colnames(clean_df)[grepl("_result", colnames(clean_df))] <-
    simpleCap(
      gsub("_result", "", 
           colnames(clean_df)[grepl("_result", colnames(clean_df))])
    )
  
  if (nrow(clean_df) == 0){
    if (interactive){
      return(ggplotly(ggplot()+theme_bw()+ggtitle("No entries.")))
    } else {
      return(ggplot()+theme_bw()+ggtitle("No entries."))
    }
  }
  
  clean_m <- melt(clean_df, id.vars = "Label", variable.name = "Method")
  clean_m$Label <- factor(clean_m$Label, levels = unique(clean_m$Label))
  
  p <- ggplot(clean_m, 
              aes(Method, Label, fill = value))+
    theme_bw()+
    scale_fill_discrete(type = color_map)+
    scale_x_discrete(expand = c(0,0))+
    scale_y_discrete(expand = c(0,0))+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.title = element_blank())+
    ylab(paste("Record:", lab))+
    NULL
  
  if (!show_y_lab | length(unique(clean_m$Label)) > 100){
    p <- p + 
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  }
  
  # if there aren't a ton of entries, you can add grids
  if (length(unique(clean_m$Label))*length(m_for_type) < 250*6){
    p <- p + 
      geom_tile(color = "black")
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
            "Too many entries. Reduce the amount of subjects or remove interactivity in the sidebar."
          )
      )
    } else {
      p <- suppressWarnings({
        ggplotly(
          p
        ) %>%
          layout(
            legend = list(orientation = "h",   # show entries horizontally
                          xanchor = "center",  
                          x = 0.5,
                          y = 1.1)) %>% 
          config(displayModeBar = F)
      })
    }
  } else {
    p <- p + theme(text = element_text(size = 15))
  }
  
  return(p)
}

