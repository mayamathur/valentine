
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
d = fread("valentine_data_prepped.csv")

# no sci notation
options(scipen=999)




# MODEL 1: BOTH VALENCES; REVERSE-CODING ---------------------------------------------------------------

# warmup: regular meta-analysis
m0 = robu(yir ~ 1,
          var.eff.size = vi,
          data = d,
          studynum = study_id,
          modelweights = "HIER",
          small = TRUE)

# ~ Model 1: Both valences with reverse-coding -------------------------------------------------
m1 = robu(yir ~ white,
          var.eff.size = vi,
          data = d,
          studynum = study_id,
          modelweights = "HIER",
          small = TRUE)

( res_rows = report_meta(m1, .mod.type = "robu")$stats )


res_rows = res_rows %>% add_column(.before = 1,
                                   "Est" = c("Intercept", "White") )

res_rows = res_rows %>% add_column(.before = 1,
                                   "k" = rep(sum(m1$k), 2) )

res_rows = res_rows %>% add_column(.before = 1,
                                   "Analysis" = rep("All (rev-code)", 2) )

# initialize results for all analyses
res = res_rows

# ~ Model 2: Positive valences only -------------------------------------------------
m2 = robu(yio ~ white,
          var.eff.size = vi,
          data = d %>% filter(valence == "positive"),
          studynum = study_id,
          modelweights = "HIER",
          small = TRUE)

( res_rows = report_meta(m2, .mod.type = "robu")$stats )


res_rows = res_rows %>% add_column(.before = 1,
                                   "Est" = c("Intercept", "White") )

res_rows = res_rows %>% add_column(.before = 1,
                                   "k" = rep(sum(m2$k), 2) )

res_rows = res_rows %>% add_column(.before = 1,
                                   "Analysis" = rep("Positive val", 2) )

res = rbind(res, res_rows)


# ~ Model 3: Both valences with reverse-coding -------------------------------------------------
m3 = robu(yio ~ white,
          var.eff.size = vi,
          data = d %>% filter(valence == "negative"),
          studynum = study_id,
          modelweights = "HIER",
          small = TRUE)

( res_rows = report_meta(m3, .mod.type = "robu")$stats )


res_rows = res_rows %>% add_column(.before = 1,
                                   "Est" = c("Intercept", "White") )

res_rows = res_rows %>% add_column(.before = 1,
                                   "k" = rep(sum(m3$k), 2) )

res_rows = res_rows %>% add_column(.before = 1,
                                   "Analysis" = rep("Negative val", 2) )

res = rbind(res, res_rows)


# PUBLICATION BIAS  -------------------------------------------------

"For all estimates regardless of valence (after reverse-coding): Mathur & VanderWeele (2020)’s S-value sensitivity analysis and meta-analysis of nonaffirmative studies (Mathur 2024). (In each case, I will again use RVE with study as the clustering variable.) Given your description of this literature, I would expect publication bias to operate on the main effects of attribution on depression, not the moderation, so these will be standard meta-analysis models rather than meta-regressions."

# code nonaffirmatives (assume literature favors negative correlations)
d$affirm = ( -d$yir/sqrt(d$vi) ) > qnorm(0.975)
table(d$affirm)


# ~ MAN estimate -------------------------------------------------
m_MAN = robu(yir ~ 1,
             var.eff.size = vi,
             data = d %>% filter(affirm == FALSE),
             studynum = study_id,
             modelweights = "HIER",
             small = TRUE)

( res_rows = report_meta(m_MAN, .mod.type = "robu")$stats )


res_rows = res_rows %>% add_column(.before = 1,
                                   "Est" = c("Intercept") )

res_rows = res_rows %>% add_column(.before = 1,
                                   "k" = rep(sum(m_MAN$k), 1) )

res_rows = res_rows %>% add_column(.before = 1,
                                   "Analysis" = rep("MAN (worst-case)", 1) )

res = rbind(res, res_rows)



# ~ S-value -------------------------------------------------

# since the MAN estimate is negative, S-value to shift to 0 will obviously be impossible

# consider shifting to a nonnull value instead (r = -0.20)

svalues = PublicationBias::pubbias_svalue(yi = d$yir,
                                vi = d$vi,
                                favor_positive = FALSE,
                                q = r_to_z(-0.20),
                                cluster = d$study_id,
                                model_type = "robust",
                                small = TRUE)

svalues$stats



p = PublicationBias::significance_funnel(yi = d$yir,
                                     vi = d$vi,
                                     est_all = m0$b.r,
                                     est_worst = m_MAN$b.r,
                                     favor_positive = FALSE)









