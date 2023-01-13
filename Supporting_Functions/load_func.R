# Supporting Load Functions
# By Hannah De los Santos
# Originated on: 12/21/22

# Helper functions to load/process functions.

# overall constants ----

# define commonly used colors
# colors chosen to be colorblind-friendly
col_impl_med <- "#fdb863" 
col_impl_high <- "#e66101" 
col_inc_med <- "#b2abd2" 
col_inc_high <- "#5e3c99"
col_default <- "#000000" 

# support functions ----

# load requisite functions
# function below from ?source
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    # if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    # if(trace) cat("\n")
  }
}

# capitalize first letter of words, from ?toupper, edited to handle vector
simpleCap <- function(y) {
  sapply(y, function(x){
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
  }, USE.NAMES = F)
}
