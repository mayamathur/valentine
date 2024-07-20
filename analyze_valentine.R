
# NOTE ---------------------------------------------------------------

# This script can run either of 2 analyses (Fisher's z scale as planned or Pearson's r as sens analysis).
# Set the global variable below. Written results files names will list the analysis.

# PRELIMINARIES ---------------------------------------------------------------

# set global variable
analysis = "zscale"
#analysis = "rscale"

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

if ( analysis == "zscale" ) {
  ( results.dir = str_replace_all( string = here(),
                                   pattern = "Code \\(git\\)",
                                   replacement = "Results/Fisher z scale (primary)" ) )
}

if ( analysis == "rscale" ) {
  ( results.dir = str_replace_all( string = here(),
                                   pattern = "Code \\(git\\)",
                                   replacement = "Results/Pearson's r scale (post hoc)" ) )
}



# check
setwd(data.dir)
setwd(results.dir)


# get helper fns
setwd(code.dir)
source("helper_valentine.R")

# get data
setwd(data.dir)

if ( analysis == "zscale" ) d = fread("valentine_data_prepped.csv")
if ( analysis == "rscale" ) d = fread("valentine_data_prepped_rscale.csv")

# no sci notation
options(scipen=999)


# WARMUP MODELS -------------------------------------------------

# regular meta-analysis
( m0 = robu(yir ~ 1,
            var.eff.size = vi,
            data = d,
            studynum = study_id,
            modelweights = "HIER",
            small = TRUE) )

# white subset
( mw = robu(yir ~ 1,
            var.eff.size = vi,
            data = d %>% filter(white == 1),
            studynum = study_id,
            modelweights = "HIER",
            small = TRUE) )

# black subset
( mb = robu(yir ~ 1,
            var.eff.size = vi,
            data = d %>% filter(white == 0),
            studynum = study_id,
            modelweights = "HIER",
            small = TRUE) )



# MAIN ANALYSIS MODELS ---------------------------------------------------------------


# ~ Model 1: Both valences with reverse-coding -------------------------------------------------
m1 = robu(yir ~ white,
          var.eff.size = vi,
          data = d,
          studynum = study_id,
          modelweights = "HIER",
          small = TRUE)

( res_rows = report_meta(m1, .mod.type = "robu", .analysis = analysis)$stats )


res_rows = res_rows %>% add_column(.before = 1,
                                   "Est" = c("Intercept", "White") )

res_rows = res_rows %>% add_column(.before = 1,
                                   "k" = rep(sum(m1$k), 2) )

res_rows = res_rows %>% add_column(.before = 1,
                                   "Analysis" = rep("All (rev-coding negative)", 2) )

# initialize results for all analyses
res = res_rows

# ~ Model 2: Positive valences only -------------------------------------------------
m2 = robu(yio ~ white,
          var.eff.size = vi,
          data = d %>% filter(valence == "positive"),
          studynum = study_id,
          modelweights = "HIER",
          small = TRUE)

( res_rows = report_meta(m2, .mod.type = "robu", .analysis = analysis)$stats )


res_rows = res_rows %>% add_column(.before = 1,
                                   "Est" = c("Intercept", "White") )

res_rows = res_rows %>% add_column(.before = 1,
                                   "k" = rep(sum(m2$k), 2) )

res_rows = res_rows %>% add_column(.before = 1,
                                   "Analysis" = rep("Positive val (original coding)", 2) )

res = rbind(res, res_rows)


# ~ Model 3: Negative valences only (original coding) -------------------------------------------------
m3 = robu(yir ~ white,
          var.eff.size = vi,
          data = d %>% filter(valence == "negative"),
          studynum = study_id,
          modelweights = "HIER",
          small = TRUE)

( res_rows = report_meta(m3, .mod.type = "robu", .analysis = analysis)$stats )


res_rows = res_rows %>% add_column(.before = 1,
                                   "Est" = c("Intercept", "White") )

res_rows = res_rows %>% add_column(.before = 1,
                                   "k" = rep(sum(m3$k), 2) )

res_rows = res_rows %>% add_column(.before = 1,
                                   "Analysis" = rep("Negative val (rev-coding negative)", 2) )

res = rbind(res, res_rows)


# PUBLICATION BIAS  -------------------------------------------------

# from my protocol:
"For all estimates regardless of valence (after reverse-coding): Mathur & VanderWeele (2020)’s S-value sensitivity analysis and meta-analysis of nonaffirmative studies (Mathur 2024). (In each case, I will again use RVE with study as the clustering variable.) Given your description of this literature, I would expect publication bias to operate on the main effects of attribution on depression, not the moderation, so these will be standard meta-analysis models rather than meta-regressions."

# code nonaffirmatives (assume literature favors negative correlations)
d$affirm = ( -d$yir/sqrt(d$vi) ) > qnorm(0.975)
mean(d$affirm)  # 68% affirmative


# ~ MAN estimate -------------------------------------------------
m_MAN = robu(yir ~ 1,
             var.eff.size = vi,
             data = d %>% filter(affirm == FALSE),
             studynum = study_id,
             modelweights = "HIER",
             small = TRUE)

( res_rows = report_meta(m_MAN, .mod.type = "robu", .analysis = analysis)$stats )


res_rows = res_rows %>% add_column(.before = 1,
                                   "Est" = c("Intercept") )

res_rows = res_rows %>% add_column(.before = 1,
                                   "k" = rep(sum(m_MAN$k), 1) )

res_rows = res_rows %>% add_column(.before = 1,
                                   "Analysis" = rep("MAN (worst-case regular meta)", 1) )

res = rbind(res, res_rows)



# ~ S-value -------------------------------------------------

# since the MAN estimate is negative, S-value to shift to 0 will obviously be infty

# consider shifting to a nonnull value instead (r = -0.20)
if ( analysis == "zscale" ) q = r_to_z(-0.20)
if ( analysis == "rscale" ) q = -0.20
svalues = PublicationBias::pubbias_svalue(yi = d$yir,
                                vi = d$vi,
                                favor_positive = FALSE,
                                q = q,
                                cluster = d$study_id,
                                model_type = "robust",
                                small = TRUE)

svalues$stats

# add it to results, along with base meta-analysis model
( res_rows = report_meta(m0, .mod.type = "robu", .analysis = analysis)$stats )


res_rows = res_rows %>% add_column(.before = 1,
                                   "Est" = c("Intercept") )

res_rows = res_rows %>% add_column(.before = 1,
                                   "k" = rep(sum(m0$k), 1) )

res_rows = res_rows %>% add_column(.before = 1,
                                   "Analysis" = rep("Basic meta (S-values for r = -0.20)", 1) )

res_rows = bind_cols(res_rows, svalues$stats)

res = bind_rows(res, res_rows)



# ~ Significance funnel plot -------------------------------------------------

if ( analysis == "zscale" ) {
  p = PublicationBias::significance_funnel(yi = d$yir,
                                           vi = d$vi,
                                           est_all = m0$b.r,
                                           est_worst = m_MAN$b.r,
                                           xlab = "Point estimate (Fisher's z)",
                                           favor_positive = FALSE)
  
  
  my_ggsave(name = "significance_funnel.png",
            .plot = p,
            .width = 10,
            .height = 8,
            .results.dir = results.dir,
            .overleaf.dir = NA)
}



# WRITE RESULTS  -------------------------------------------------

setwd(results.dir)
fwrite(res, paste("results_unrounded_", analysis, ".csv", sep = "") )

# rounded
( names_to_round = names(res)[4:ncol(res)] )
res2 = res %>% mutate( across(4:ncol(res), function(x) round(x, 2)) )
fwrite(res2, paste("results_rounded_", analysis, ".csv", sep = "") )


# SANITY CHECKS  -------------------------------------------------

# ~ Influence diagnostics  -------------------------------------------------
# caveat: these require assuming independence across studies
m0_par = rma(yi = d$yir,
             vi = d$vi,
             slab = d$sample_id)

inf = influence(m0_par)
plot(inf)



