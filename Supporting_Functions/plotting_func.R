# Plotting Functions
# By Hannah De los Santos
# Originated on: 12/21/22

# Helper functions to plot data for ACME.

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
            "Age (days): ", age_days, "\n",
            "Age (months): ", signif(age_days/30.4,4), "\n",
            "Age (years): ", signif(age_years,4), "\n",
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

# function to generate correlation or count plots for methods (overall plot)
plot_methods_corr <- function(cleaned_df, type,
                              methods_chosen = methods_avail,
                              show_heat_corr = F){
  
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
  
  # if there's no data or there's no methods
  if (nrow(clean_df) == 0 | all(!grepl("_result", colnames(clean_df)))){
    return(ggplotly(ggplot()+theme_bw()))
  }
  
  # get columns for correlation
  corr_df <- clean_df[, grepl("_result", colnames(clean_df)), drop = F]
  colnames(corr_df) <- simpleCap(gsub("_result","",colnames(corr_df)))
  corr_df[corr_df == "Include"] <- 0
  corr_df[corr_df == "Implausible"] <- 1
  
  if (show_heat_corr){
    # compute correlation
    corr_df <- sapply(corr_df, as.numeric)
    corr_df <- cor(corr_df)
    corr_df[lower.tri(corr_df)] <- NA
    
    out_name <- "Correlation"
  } else {
    # otherwise compute counts
    corr_df <- sapply(corr_df, as.numeric)
    corr_df <- crossprod(corr_df)
    corr_df[lower.tri(corr_df)] <- NA
    
    out_name <- "Count"
  }
  
  # melt into long form for ggplot
  corr_df <- reshape2::melt(corr_df)
  colnames(corr_df) <- c("Method.1", "Method.2", out_name)
  corr_df[, out_name] <- signif(corr_df[, out_name], 4)
  
  # create correlation heat map
  p <- ggplot(corr_df, aes_string("Method.1", "Method.2",
                                  fill = out_name,
                                  label = out_name))+
    geom_tile()+
    geom_text(size = 3)+
    theme_bw()+
    scale_x_discrete(expand = c(0,0))+
    scale_y_discrete(expand = c(0,0))+
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          plot.title = element_text(hjust = .5))+
    ggtitle(paste0(out_name, " of Implausible Values for ", type_n[type]))+
    NULL
  
  p <- p +
    if (show_heat_corr){
      scale_fill_gradient2(
        breaks = c(-1,0,1),
        limits = c(-1,1),
        low = "#0571b0", mid = "#f7f7f7", high = "#ca0020")
    } else {
      scale_fill_gradient2(
        low = "#0571b0", mid = "#f7f7f7", high = "#ca0020")
    }
  
  p <- ggplotly(
    p
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
                               color_ans = T,
                               iter_step = 1,
                               legn = F){
  # if color answers is true and there are no answers, fix that
  if (nrow(cleaned_df) > 0 &&
      color_ans && !"answers" %in% colnames(cleaned_df)){
    color_ans <- F
  }
  
  # the minimum +/- value around the mean for the y axis to show
  min_range_band <- c(
    "HEIGHTCM" = 3.5,
    "WEIGHTKG" = 5
  )
  
  # maps for configuring ggplot
  color_map <-
    if (color_ans){
      # if we're highlighting incorrect answers, make those brighter
      if (T){#(hl_incorr){ FIX LATER
        c(
          "Incorrect: Include (False Negative)" = "#5e3c99",
          "Correct: Include (True Negative)" = "#b2abd2",
          "Incorrect: Implausible (False Positive)" = "#e66101",
          "Correct: Implausible (True Positive)" = "#fdb863",
          "Not Calculated" = "#000000",
          "Unknown" = "#000000"
        )
      } else {
        c(
          "Correct: Include (True Negative)" = "#5e3c99",
          "Incorrect: Include (False Negative)" = "#b2abd2",
          "Correct: Implausible (True Positive)" = "#e66101",
          "Incorrect: Implausible (False Positive)" = "#fdb863",
          "Not Calculated" = "#000000",
          "Unknown" = "#000000"
        )
      }
    } else {
      c(
        "Include" = "#000000",
        "Not Calculated" = "#000000",
        "Unknown" = "#000000",
        "Implausible" = "#fdb863"
      )
    }
  
  shape_map <- c(
    "Include" = 16,
    "Not Calculated" = 16,
    "Unknown" = 16,
    "Implausible" = 17
  )
  
  size_map <- c(
    "Include" = 2,
    "Not Calculated" = 2,
    "Unknown" = 2,
    "Implausible" = 3
  )
  
  type_map <- c(
    "HEIGHTCM" = "Height (cm)",
    "WEIGHTKG" = "Weight (kg)"
  )
  
  result_map <- c(
    "TRUE" = "Implausible",
    "FALSE" = "Include",
    "Not Calculated" = "Not Calculated",
    "Unknown" = "Unknown"
  )
  
  opposite_focus_map <- c(
    "Height (cm)" = "w",
    "Weight (kg)" = "h"
  )
  
  type <- c("HEIGHTCM", "WEIGHTKG")
  step_focus <- type_map[
    if (grepl("h", step) & nchar(gsub("\\d", "",  step)) == 1){
      "HEIGHTCM"
    } else if (grepl("w", step) & nchar(gsub("\\d", "",  step)) == 1){
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
        if (!methods_chosen %in% c("growthcleanr-naive", "muthalagu")){
          result_map[
            as.character(
              clean_df[, paste0(methods_chosen, "_Step_", step, "_Result")]
            )
          ]
        } else if (methods_chosen == "growthcleanr-naive") {
          result_map[
            as.character(
              clean_df[, paste0(methods_chosen, "_Step_", step,
                                "_Iter_", iter_step, "_Result")]
            )
          ]
        } else {
          if (grepl("2", step)){ # we only need the bucket for step 2
            result_map[
              as.character(
                clean_df[, paste0(methods_chosen, "_Step_", step,
                                  "_Bucket_", iter_step, "_Result")]
              )
            ]
          } else {
            result_map[
              as.character(
                clean_df[, paste0(methods_chosen, "_Step_", step, "_Result")]
              )
            ]
          }
        }
      }
  )
  add_title <- ""
  # if it's muthalagu, we want to remove all the ages not in the bucket
  if (methods_chosen == "muthalagu" & grepl("2", step)){
    ages <- as.numeric(unlist(strsplit(iter_step, "-")))
    age_low <- ages[1]
    age_high <- ages[2]
    
    bf_df <- bf_df[bf_df$age_years >= age_low & bf_df$age_years < age_high,]
    
    if (nrow(bf_df) == 0){
      if (legn){
        return(ggplot()+theme_bw())
      } else {
        return(ggplotly(ggplot()+
                          theme_bw()+
                          ggtitle("No ages in selected bucket.")) %>%
                 config(displayModeBar = F))
      }
    }
    
    # if the step is 2b+, we need to check if they're all NA (which means they
    # weren't processed for that step)
    if (step %in% c("2hb", "2hc")){
      if (all(is.na(bf_df$step_result))){
        bf_df$step_result <- "Include"
        add_title <- "All records included in step 2ha."
      }
    }
  }
  # for muthalagu, we also want to remove all the weights
  # if (methods_chosen == "muthalagu"){
  #   # filter out all the weights
  #   bf_df <- bf_df[bf_df$param == "Height (cm)",]
  # }
  
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
  
  # for ease of coloring
  bf_df$step_result_color <- bf_df$step_result
  # if we're going to show answers, we want to change the names
  if (color_ans){
    # get the results as compared to the answers
    ans_res <- bf_df$step_result_color ==
      clean_df$answers[clean_df$id %in% bf_df$id]
    # do not get the not calculated
    ans_nnc <- !bf_df$step_result_color %in% c("Not Calculated", "Unknown")
    ans_incl <- clean_df$answers[clean_df$id %in% bf_df$id] == "Include"
    ans_impl <- clean_df$answers[clean_df$id %in% bf_df$id] == "Implausible"
    # add correct/incorrect + fp/tp/fn/tn
    bf_df$step_result_color[ans_res & ans_nnc] <-
      paste0(
        "Correct: ", bf_df$step_result_color[ans_res & ans_nnc]
      )
    bf_df$step_result_color[ans_res & ans_nnc & ans_incl] <-
      paste0(
        bf_df$step_result_color[ans_res & ans_nnc & ans_incl],
        " (True Negative)"
      )
    bf_df$step_result_color[ans_res & ans_nnc & ans_impl] <-
      paste0(
        bf_df$step_result_color[ans_res & ans_nnc & ans_impl],
        " (True Positive)"
      )
    
    bf_df$step_result_color[!ans_res & ans_nnc] <-
      paste0(
        "Incorrect: ", bf_df$step_result_color[!ans_res & ans_nnc]
      )
    bf_df$step_result_color[(!ans_res) & ans_nnc & ans_impl] <-
      paste0(
        bf_df$step_result_color[(!ans_res) & ans_nnc & ans_impl],
        " (False Negative)"
      )
    bf_df$step_result_color[(!ans_res) & ans_nnc & ans_incl] <-
      paste0(
        bf_df$step_result_color[(!ans_res) & ans_nnc & ans_incl],
        " (False Positive)"
      )
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
        data = bf_df[bf_df$step_result %in%
                       c("Include", "Not Calculated", "Unknown"),],
        aes(age_years, measurement), color = "grey")+
      geom_point(
        aes(
          age_years, measurement,
          color = step_result_color, shape = step_result,
          size = step_result,
          text = paste0(
            "ID: ", id,"\n",
            "Step ", step,
            if (methods_chosen == "growthcleanr-naive"){
              paste0(" Iter ", iter_step) } else {""},
            " Result: ", step_result,"\n",
            if(color_ans){paste0("Answer: ", step_result_color)} else {""}
          )
        )
      )+
      theme_bw()+
      scale_color_manual("Result Color", values = color_map, breaks = names(color_map))+
      scale_shape_manual("Result Shape", values = shape_map, breaks = names(shape_map))+
      scale_size_manual("Result Shape", values = size_map, breaks = names(shape_map))+
      theme(plot.title = element_text(hjust = .5))+
      xlab("Age (Years)")+
      ylab("Measurement")+
      facet_grid_sc(rows = vars(param), scales = list(y = scales_y))+
      if (methods_chosen == "muthalagu"){
        ggtitle(add_title)
      }
  )
  
  # # keep if necessary
  # if (length(unique(bf_df$param)) > 1){
  #   p <- p +
  #     facet_grid_sc(rows = vars(param), scales = list(y = scales_y))
  # } else {
  #   p <- p +
  #     ylim(yaxis_lim)+
  #     ylab(unique(bf_df$param))
  # }
  
  if (sum(bf_df$id %in% focus_ids) > 0){
    p <- p +
      geom_point(
        data = bf_df[bf_df$id %in% focus_ids,],
        aes(
          age_years, measurement,
          color = step_result_color
        ),
        fill = NA, shape = 21, size = 6
      )
  }
  
  if (legn){
    p <- p +
      theme(legend.position = "bottom",
            legend.direction = "horizontal",
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

