# Processing Functions
# By Hannah De los Santos
# Originated on: 12/21/22

# Helper functions to process data for ACME.

# processing functions ----

# function to tabulate results of a given height or weight
tab_clean_res <- function(cleaned_df, type, methods_chosen = methods_avail, 
                          show_perc_bar = F){
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
    if (show_perc_bar){
      tab <- round(tab/sum(cleaned_df$param == type)*100, 2)
    }
    t_tab[m, names(tab)] <- tab
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
    clean_m <- reshape2::melt(clean_df, id.vars = c("Label", "subjid", "id", "Label_Name"), variable.name = "Method")
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
                           highlt = "none",
                           iter_step = 1,
                           color_ans = T){
  # if color answers is true and there are no answers, fix that
  if (nrow(cleaned_df) > 0 &&
      color_ans && !"answers" %in% colnames(cleaned_df)){
    color_ans <- F
  }
  
  type_map <- c(
    "HEIGHTCM" = "Height (cm)",
    "WEIGHTKG" = "Weight (kg)"
  )
  
  result_map <- c(
    "TRUE" = "Implausible",
    "FALSE" = "Include",
    "Implausible" = "Implausible",
    "Include" = "Include",
    "Not Calculated" = "Not Calculated",
    "Unknown" = "Unknown"
  )
  
  # values we want to focus on in the table
  step_focus <-
    if (grepl("h", step) & nchar(gsub("\\d", "",  step)) == 1){
      "HEIGHTCM"
    } else if (grepl("w", step) & nchar(gsub("\\d", "",  step)) == 1){
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
  
  tab_out <- clean_df[,c("id", "param", "age_years", "measurement",
                         if (color_ans) {"answers"} else {c()})]
  tab_out <- cbind(
    tab_out,
    if (step == "Before"){
      rep("Include", nrow(tab_out))
    } else if (step == "After"){
      clean_df[, paste0(methods_chosen, "_result")]
    } else {
      if (!methods_chosen %in% c("growthcleanr-naive", "muthalagu")){
        clean_df[,colnames(clean_df)[
          grepl(paste0("_Step_", step), colnames(clean_df))]]
      } else if (methods_chosen == "growthcleanr-naive") {
        clean_df[,colnames(clean_df)[
          grepl(paste0("_Step_", step, "_Iter_", iter_step),
                colnames(clean_df))]]
      } else {
        if (grepl("2", step)){ # we only need the bucket for step 2
          clean_df[, colnames(clean_df)[
            grepl(paste0("_Step_", step, "_Bucket_", iter_step),
                  colnames(clean_df))]]
        } else {
          clean_df[, colnames(clean_df)[
            grepl(paste0("_Step_", step),
                  colnames(clean_df))]]
        }
      }
    }
  )
  if (step == "Before" | step == "After"){
    colnames(tab_out)[ncol(tab_out)] <-
      paste0(methods_chosen, "_Step_", step, "_Result")
  }
  # if it's muthalagu, we want to remove all the ages not in the bucket
  if (methods_chosen == "muthalagu" & grepl("2", step)){
    ages <- as.numeric(unlist(strsplit(iter_step, "-")))
    age_low <- ages[1]
    age_high <- ages[2]
    
    tab_out <- tab_out[tab_out$age_years >= age_low &
                         tab_out$age_years < age_high,]
    
    if (nrow(tab_out) == 0){
      return(data.frame())
    }
    
    # if the step is 2b+, we need to check if they're all NA (which means they
    # weren't processed for that step)
    if (step %in% c("2hb", "2hc")){
      if (all(is.na(tab_out[, paste0(methods_chosen, "_Step_", step,
                                     "_Bucket_", iter_step, "_Result")]))){
        tab_out[, paste0(methods_chosen, "_Step_", step,
                         "_Bucket_", iter_step, "_Result")] <- "Include"
      }
    }
  }
  # for muthalagu, we also want to remove all the weights
  if (methods_chosen == "muthalagu"){
    # filter out all the weights
    tab_out <- tab_out[tab_out$param == "HEIGHTCM",]
  }
  
  # if there are only NAs in a column, remove it
  # (this will happen in growthcleanr)
  tab_out <- tab_out[, sapply(tab_out, function(x){ !all(is.na(x)) })]
  
  # prettify certain columns
  tab_out[, grepl("_Result", colnames(tab_out))] <-
    as.data.frame(
      lapply(tab_out[, grepl("_Result", colnames(tab_out)), drop = F],
             function(x){result_map[as.character(x)]})
    )
  tab_out$param <- type_map[tab_out$param]
  tab_out[,sapply(tab_out, class) == "numeric"] <-
    round(tab_out[,sapply(tab_out, class) == "numeric"], 3)
  
  # transpose for output
  tab_out <- as.data.frame(t(tab_out))
  # add a column for names
  step_names <- gsub(
    paste0(methods_chosen, " Step ", step, " "),
    "",
    gsub("_", " ", rownames(tab_out)[-c(1:ifelse(color_ans, 5, 4))])
  )
  
  tab_out <- cbind(
    "names" = paste0(
      "<strong><p align = 'right'>",
      c("ID", "Parameter", "Age (years)", "Measurement",
        if (color_ans) {"Answers"} else {c()},
        step_names),
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


