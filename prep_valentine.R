
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
            "robumeta",
            "metafor",
            "PublicationBias",
            "MetaUtility",
            "tableone",
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
code.dir = here()

# if you need to go up a level in parent directory
( data.dir = str_replace_all( string = here(),
                              pattern = "Code \\(git\\)",
                              replacement = "Data" ) )

( results.dir = str_replace_all( string = here(),
                                 pattern = "Code \\(git\\)",
                                 replacement = "Data" ) )

# check
setwd(data.dir)
setwd(results.dir)


# get helper fns
setwd(code.dir)
source("helper_valentine.R")

# get data
setwd(data.dir)
do = fread("ds_as_race_analysts_rev_10july24.csv")

# no sci notation
options(scipen=999)




# SANITY CHECKS ---------------------------------------------------------------

# check counts vs. protocol:
# "103 effect sizes from 56 independent samples nested in 36 studies"
expect_equal(103, nrow(do))
expect_equal(36, nuni(do$study_id))
expect_equal(56, nuni(do$sample_id))

CreateTableOne(data = do)

sort(do$n)
sort(do$corr)

plot(do$n, do$corr)

# number of estimates per study
t = do %>% group_by(study_id) %>%
  summarise(count = n()) %>%
  arrange(count)
as.data.frame(t)

# number of estimates by race
t = do %>% group_by(study_id) %>%
  summarise( count = n(),
             count_black = length( corr[`race (0 = Black)` == 0] ),
             count_white = length( corr[`race (0 = Black)` == 1] ) ) %>%
  arrange(count)
as.data.frame(t)


t = do %>% group_by(study_id) %>%
  summarise( count_black = mean(corr) ) %>%
  arrange(count_black)
as.data.frame(t)


# RECODE VARIABLES ---------------------------------------------------------------

summary(do$corr)

# ~ Fisher's z  -------------------------------------------------

# Fisher's z with "o"riginal coding (not reverse-coded)
ES = escalc(measure = "ZCOR", ri = do$corr, ni = do$n)
do$yio = ES$yi
do$vi = ES$vi
do$sei = sqrt(do$vi)


# reverse-coding
# "Any analysis (including any publication bias tests) that involves negative event valence with positive event valence and/or composite event valence must involve flipping the signs of some of the correlations: the correlations for attributional style based on negative events will need to be recoded to the opposite direction"
do$yir = do$yio
do$yir[ do$valence == "negative" ] = -do$yir[ do$valence == "negative" ]

# race indicator
do$white = (do$`race (0 = Black)` == 1)


# basic funnel as sanity check
m = rma(yi = do$yir,
            vi = do$vi,
            slab = do$sample_id)

funnel(m)

# why are 3 SEs exactly 1.00?
sort(do$vi)
do %>% filter(vi == 1)
# okay, this makes sense because they all have n=4, and for Fisher's z, vi <- 1 / (ni-3) approximately
# see https://github.com/cran/metafor/blob/master/R/escalc.r

forest(m)


# ~ Pearson's r (for post hoc analyses)  -------------------------------------------------

# doing this because of the variance estimation issue for the n=4 studies
d2 = do
ES = escalc(measure = "COR", ri = d2$corr, ni = d2$n)
d2$yio = ES$yi
d2$vi = ES$vi
d2$sei = sqrt(d2$vi)

# reverse-coding
d2$yir = d2$yio
d2$yir[ d2$valence == "negative" ] = -d2$yir[ d2$valence == "negative" ]

# compare vi's to those from Fisher's z
# can differ quite a lot for the very large variances
plot(do$vi, d2$vi)
plot(do$vi, do$vi - d2$vi)


# WRITE DATA ------------------------------------------------

setwd(data.dir)
fwrite(do, "valentine_data_prepped.csv")
fwrite(d2, "valentine_data_prepped_rscale.csv")





