
# PRELIMINARIES ---------------------------------------------------------------

# This script uses renv to preserve the R environment specs (e.g., package versions.)
library(renv)
# run this if you want to reproduce results using the R environment we had:
# renv::restore()

to.load = c("dplyr",
            "data.table",
            "purrr",
            "tidyr",
            "stringr",
            "tibble",
            "ggplot2",
            "testthat",
            "plotly",
            "htmlwidgets", # for saving plotly
            "here")

# load within installation if needed
for (pkg in to.load) {
  
  cat( paste("\nAbout to try loading package", pkg) )
  
  tryCatch({
    # eval below needed because library() will otherwise be confused
    # https://www.mitchelloharawild.com/blog/loading-r-packages-in-a-loop/
    eval( bquote( library( .(pkg) ) ) )
  }, error = function(err) {
    install.packages(pkg)
  })
  
}

# run this only if you want to update the R environment specs
# renv::snapshot()


# set working directories
code.dir = here("Code")

# if you need to go up a level in parent directory
data.dir = str_replace_all( string = here(),
                            pattern = "Code",
                            replacement = "Data" ) 

results.dir = str_replace_all( string = here(),
                               pattern = "Results",
                               replacement = "Data" ) 


# get helper fns
setwd(code.dir)
source("helper.R")

# no sci notation
options(scipen=999)

# below are the only absolute paths
# write results directly to directory containing TeX manuscript in Overleaf so stats can be piped directly into text
# this is an absolute path because it must live in Dropbox, outside the project directory, in order to sync with Overleaf
# to reproduce results, just set this to any directory on your local machine
# results will be written to a csv file in that location


# PRELIMINARIES ---------------------------------------------------------------
