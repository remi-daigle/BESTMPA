# Bio-Economic Selection Toolbox for Marine Protected Areas - R package
Thank you for your interest in the R package for the bioeconomic evaluation of MPA network design! If you haven't already done so, please refer to the [manuscript](https://f1000research.com/articles/4-1234/v1) describing the model results.

## Usage
If you would like to use this package to your own scenario, please fork this repository and modify as needed, or install the package directly from GitHub.
```{r install from github, eval=FALSE}
install.packages("devtools")
library(devtools)
install_github("remi-daigle/BESTMPA@gh-pages")
```

We kindly ask that you cite both the original manuscript and this repository in any resulting publication. We recognize that there are some aspects of the model the could be closer to reality and we look forward to seeing how you would improve it. To make changes, we recommend starting with the code in this vignette (or the [index.Rmd](https://github.com/remi-daigle/BESTMPA/blob/gh-pages/index.Rmd))

If you would like to replicate our results, download the repository as is and run the simulations by running the code in [index.Rmd](https://github.com/remi-daigle/BESTMPA/blob/gh-pages/index.Rmd). This script will contains all the user defined parameters, and the functions are all contained within the [R/](https://github.com/remi-daigle/BESTMPA/tree/gh-pages/R) directory if you would like to inspect and or modify them individually.

**Please note:** This package requires the installation of R packages before use (tidyverse, data.table, ggplot2 grid, Grid2Polygons, gridExtra, igraph, maptools, Matrix, raster, readr, rgdal*, rgeos, sp, spdep, tidyr)
>*installation of the rgdal package is notoriously difficult, please see [here](http://stackoverflow.com/questions/15248815/rgdal-package-installation) and [here](http://cran.r-project.org/web/packages/rgdal/index.html) for further information should you have any problems.

Please see package [vignette](https://remi-daigle.github.io/BESTMPA/) for further instructions
