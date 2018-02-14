README
================

This software is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The software has not received final approval by the U.S. Geological Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. The software is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the software.

What this app does
------------------

This app generates a model archival summary (MAS) for a water-quality surrogate model.

Running the app remotely
------------------------

You will need to have the shiny, rmarkdown, scales, labeling, ggplot2, car, dataRetrieval, lubridate, smwrQW, smwrStats, XML, DAAG, and MASS packages installed. This could be done by running the following commands in an R Console (or RStudio):

``` r
install.packages("shiny")
install.packages("rmarkdown")
install.packages("scales")
install.packages("labeling")
install.packages("ggplot2")
install.packages("car")
install.packages("dataRetrieval", repos="https://owi.usgs.gov/R")
install.packages("lubridate")
install.packages("smwrQW", repos="https://owi.usgs.gov/R")
install.packages("smwrStats", repos="https://owi.usgs.gov/R")
install.packages("XML")
install.packages("DAAG")
install.packages("MASS")
```

Once you have the packages installed, you can start the app with the following commands:

``` r
library(shiny)
runGitHub("PatrickEslick/regReport", launch.browser=TRUE)
```

Using the app
-------------

See the app's "Help" tab for details on using the app.
