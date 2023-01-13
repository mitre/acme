# Specifications to run ACME with the Infants Framework
# By Hannah De los Santos
# Originated on: 12/21/22

# Read README section "Using the ACME Framework for Other Anthropometric Algorithms" to update this configuration file.

# comparison title (same case as file directories) ----

# TODO: change "group" to your comparison title
comp_title <- "group"

# load method functions ----

sourceDir(file.path("EHR_Cleaning_Implementations", comp_title))

# data specification ----

# TODO: change "EXAMPLE.csv" to the names of the data files for both your base data, and with your data results.
dat <- read.csv(file.path("Data", comp_title, "EXAMPLE.csv"))
dat_res <- read.csv(
  file.path("Data", comp_title, "EXAMPLE.csv")
)

# default age range to focus on ----

# TODO: change to your default age range to focus on
age_low <- -Inf
age_high <- Inf

# method specification ----

# regular methods

# TODO: replace with a character vector of algorithm titles
methods_avail <- c("algo_1", "algo_2")

# TODO: if not all algorithms clean both height and weight, specify with character vectors which are height or weight algorithms
# types cleaned for each method
m_types <- list(
  "HEIGHTCM" = methods_avail,
  "WEIGHTKG" = methods_avail
)

# TODO: list algorithm functions (only the base name, do NOT include passed in variables, i.e. "algo" not "algo()")
methods_func <- list(algo_1,
                     algo_1
)
names(methods_func) <- methods_avail

# method colors
m_colors <- viridisLite::viridis(length(methods_avail))
names(m_colors) <- simpleCap(methods_avail)

# intermediate methods
# TODO: replace with a character vector of algorithm titles that have "intermediate values" (i.e. inter_vals = TRUE adds additional intermediate value columns)
methods_inter_avail <- c("algo_1", "algo_2")

# types cleaned for each method
# TODO: if not all algorithms clean both height and weight, specify with character vectors which are height or weight algorithms
# types cleaned for each method
m_inter_types <- list(
  "HEIGHTCM" = methods_avail,
  "WEIGHTKG" = methods_avail
)

# TODO: list algorithm functions (only the base name, do NOT include passed in variables, i.e. "algo" not "algo()")
methods_inter_func <- list(algo_1,
                           algo_1)
names(methods_inter_func) <- methods_inter_avail

# list of steps for each method
# TODO: add steps for intermediate values for each method as a character vector
m_inter_steps <- list(
  "algo_1" = c("1w", "1h"),
  "algo_2" = c("1h", "1w", "2h", "2w")
)

# TODO: add short titles for each step for intermediate values for each method as a named character vector within a list
m_inter_steps_full_title <- list(
  "algo_1" = c(
    "1w" = "1w: W short title",
    "1h" = "1h: H short title"
  ),
  "algo_1" = c(
    "1h" = "1h: H short title",
    "1w" = "1w: W short title",
    "2h" = "2h: H short title",
    "2w" = "2w: W short title"
  )
)

# TODO: add descriptions for each step for intermediate values for each method as a named character vector within a list
m_inter_steps_full_subtitle <- list(
  "algo_1" = c(
    "1w" = "1w: Long description of what happens within this step, at a high level.",
    "1h" = "1h: Long description of what happens within this step, at a high level."
  ),
  "algo_2" = c(
    "1h" = "1h: Long description of what happens within this step, at a high level.",
    "1w" = "1w: Long description of what happens within this step, at a high level.",
    "2h" = "2h: Long description of what happens within this step, at a high level.",
    "2w" = "2w: Long description of what happens within this step, at a high level."
  )
)

# method documentation (module) ----

# TODO: update documentation with a tab for each included method for the "about page" of ACME
methods_docs_UI <- function(id){
  list(
    # UI: algorithm 1 ----
    tabPanel(
      "Algorithm 1 (2022)", # title of the tab
      fluidRow(
        column(width = 3),
        column(
          width = 6,
          HTML(
            "<h3>Algorithm 1 (2022)</h3>",
            "<h4>Cleans: Weight and Height Records</h4><p>",
            "Algorithm 1 seeks identify implausible values and outliers in longitudinal childhood anthropometric data. More information on this method can be found <a href='' target = 'blank'>here</a>. Steps for this method, along with their titles (used in output) and descriptions, are below.<p>"
          ),
          hr(),
          HTML(
            "<h4>Steps:</h4>",
            "<b>Step 1w, W short title</b><br>",
            "<ul><li>Long description of what happens within this step, in detail.</li></ul>",
            "<b>Step 1h, H short title</b><br>",
            "<ul><li>Long description of what happens within this step, in detail.</li></ul>"
          )
        ),
        column(width = 3)
      )
    ),
    
    # UI: algorithm 2 ----
    
    tabPanel(
      "Algorithm 2 (2022)", # title of the tab
      fluidRow(
        column(width = 3),
        column(
          width = 6,
          HTML(
            "<h3>Algorithm 2 (2022)</h3>",
            "<h4>Cleans: Weight and Height Records</h4><p>",
            "Algorithm 2 seeks identify implausible values and outliers in longitudinal childhood anthropometric data. More information on this method can be found <a href='' target = 'blank'>here</a>. Steps for this method, along with their titles (used in output) and descriptions, are below.<p>"
          ),
          hr(),
          HTML(
            "<h4>Steps:</h4>",
            "<b>Step 1w, W short title</b><br>",
            "<ul><li>Long description of what happens within this step, in detail.</li></ul>",
            "<b>Step 1h, H short title</b><br>",
            "<ul><li>Long description of what happens within this step, in detail.</li></ul>",
            "<b>Step 2w, W short title</b><br>",
            "<ul><li>Long description of what happens within this step, in detail.</li></ul>",
            "<b>Step 2h, H short title</b><br>",
            "<ul><li>Long description of what happens within this step, in detail.</li></ul>"
          )
        ),
        column(width = 3)
      )
    )
  )
}