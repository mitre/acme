# Adult growthcleanr Explorer

This application seeks to compare different methods of cleaning adult EHR data, implementing a variety of methods. This currently includes Muthalagu, et al., Cheng, et al., Chan, et al., Littman, et al., Breland, et al., and Daymont, et al.

## README Outline

- Installation and First-Time Set-Up

## Installation and First-Time Set-Up

To run this application, please follow the following steps (assuming you have already downloaded/cloned this repository): 

1. Download R and RStudio.
2. Open R.
3. In the console window (bottom-left corner by default), enter the following to download necessary packages:

```{r}
install.packages(c("shiny", "ggplot2", "rstudioapi", "colorspace", "plotly", "viridisLite", "ggplotify"))
```

If you are on a Mac, additionally enter:

```{r}
install.packages("Cairo")
```

Note that this may ask for some input about updating related packages -- "no" is a fine answer. This may take some time.

4. Open adult_cleaning_app.R, which should be included in the downloaded files from this repository. It should open in the top-left corner RStudio by default.
5. In the top right corner of the adult_cleaning_app.R script, you should see the button, "Run App". Click on the small downwards arrow next to it and choose "Run External".
6. Now click "Run App". This should open the application in your default browser window.
7. Have fun! More information on running the application and methods involved can be found within the app.