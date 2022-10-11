# EHR Data Explorer

This application seeks to compare different methods of cleaning adult and infants EHR data,
implementing a variety of methods. For adults, this currently includes Muthalagu, et al., Cheng, et al., Chan, et al., Littman, et al., Breland, et al., and Daymont, et al. For infants, this includes Yang, et al., Carsley, et al., Shi, et al., and Massara, et al.

## README Outline

- Installation and First-Time Set-Up

## Installation and First-Time Set-Up

To run this application, please follow the following steps (assuming you have already
downloaded/cloned this repository):

1. Download R and RStudio.
2. Open RStudio.
3. In the console window (bottom-left corner by default), enter the following to download necessary packages:

```{r}
install.packages(c("shiny", "ggplot2", "rstudioapi", "colorspace", "plotly", "viridisLite", "ggplotify", "reshape2", "shinyBS", "shinyWidgets", "data.table", "growthcleanr", "lme4", "anthro"))
```

A package from GitHub is needed as well. To download this package, enter the
following in the console:

```{r}
install.packages("devtools")
devtools::install_github("zeehio/facetscales")
```

In downloading `facetscales` on Windows, it may request that you need `Rtools`. You can download that following instructions listed [here](https://cran.r-project.org/bin/windows/Rtools/).

If you are on a Mac, R also requires the `Cairo` package, which itself requires that
the [XQuartz](https://www.xquartz.org/) package is also installed. You may need to
download and install XQuartz first, as it does not come with MacOS by default. After
XQuartz is installed on your system, install `Cairo` in R:

```{r}
install.packages("Cairo")
```

Note that this may ask for some input about updating related packages -- "no" is a fine
answer. This may take some time.

4. Open `adult_cleaning_app.R` or `infants_cleaning_app.R` (depending on which you want to run), which should be included in the downloaded files from
this repository. It should open in the top-left corner pane of RStudio by default.
5. In the top right corner of the pane with the `adult_cleaning_app.R` or `infants_cleaning_app.R` script, you should see the
button, "Run App". Click on the small downwards arrow next to it and choose "Run
External".
6. Now click "Run App". This should open the application in your default browser window.
7. Have fun! More information on running the application and methods involved can be
found within the app.
