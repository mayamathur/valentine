
# INPUT/OUTPUT FNS ----------------------------------------------


# one or both dirs can be NA
my_ggsave = function(name,
                     .plot = last_plot(),
                     .width,
                     .height,
                     .results.dir = results.dir,
                     .overleaf.dir = overleaf.dir) {
  
  dirs = c(.results.dir, .overleaf.dir)
  dirIsNA = sapply(dirs, is.na)
  validDirs = dirs[ !dirIsNA ]
  
  
  for ( dir in validDirs ) {
    setwd(dir)
    ggsave( name,
            plot = .plot,
            width = .width,
            height = .height,
            device = "png" )
  }
}

# for reproducible manuscript-writing
# adds a row to the file "stats_for_paper" with a new statistic or value for the manuscript
# optionally, "section" describes the section of code producing a given result
# expects "study" to be a global var
update_result_csv = function( name,
                              .section = NA,
                              .results.dir = results.dir,
                              .overleaf.dir = overleaf.dir.stats,
                              value = NA,
                              print = FALSE ) {
  
  # if either is NULL, it just won't be included in this vector
  dirs = c(.results.dir, .overleaf.dir)
  
  
  new.rows = data.frame( name,
                         value = as.character(value),
                         section = as.character(.section) )
  
  # to avoid issues with variable types when overwriting
  new.rows$name = as.character(new.rows$name)
  new.rows$value = as.character(new.rows$value)
  new.rows$section = as.character(new.rows$section)
  
  
  for (.dir in dirs) {
    
    setwd(.dir)
    
    if ( "stats_for_paper.csv" %in% list.files() ) {
      res <<- read.csv( "stats_for_paper.csv",
                        stringsAsFactors = FALSE,
                        colClasses = rep("character", 3 ) )
      
      # if this entry is already in the results file, overwrite the
      #  old one
      if ( all(name %in% res$name) ) res[ res$name %in% name, ] <<- new.rows
      else res <<- rbind(res, new.rows)
    }
    
    if ( ! "stats_for_paper.csv" %in% list.files() ) {
      res <<- new.rows
    }
    
    write.csv( res, 
               "stats_for_paper.csv",
               row.names = FALSE,
               quote = FALSE )
    
  }  # end "for (.dir in dirs)"
  
  
  if ( print == TRUE ) {
    View(res)
  }
  
}



# stands for "wipe results"
wr = function(){
  setwd(results.dir)
  if( "stats_for_paper.csv" %in% list.files() ) system("rm stats_for_paper.csv")
  setwd(overleaf.dir)
  if( "stats_for_paper.csv" %in% list.files() ) system("rm stats_for_paper.csv")
}

# stands for "view results"
vr = function(){
  setwd(results.dir)
  View( read.csv("stats_for_paper.csv") )
}


# GENERIC SMALL HELPERS ----------------------------------------------

# nicely report a metafor or robumeta object with optional suffix to denote which model
report_meta = function(.mod,
                       .mod.type = "rma",  # "rma" or "robu"
                       .analysis,  # from global variable ("zscale" or "rscale")
                       .suffix = "") {
  
  if ( !is.null(.mod) ) {
    
    
    if ( .mod.type == "rma" ) {
      # uses Q-profile intervals
      # from here (https://wviechtb.github.io/metafor/reference/confint.rma.html):
      # For objects of class "rma.uni" obtained with the rma.uni function, a confidence interval for the amount of (residual) heterogeneity (i.e., 𝜏2
      # ) can be obtained by setting random=TRUE (which is the default). The interval is obtained iteratively either via the Q-profile method or via the generalized Q-statistic method (Hartung and Knapp, 2005; Viechtbauer, 2007; Jackson, 2013; Jackson et al., 2014). The latter is automatically used when the model was fitted with method="GENQ" or method="GENQM", the former is used in all other cases.
      x = confint(.mod)
      tau.lb = x$random["tau","ci.lb"]
      tau.ub = x$random["tau","ci.ub"]
      
      # alternative Wald-type CIs:
      #tau.CI = tau_CI(.mod)
      
      .res = data.frame( .mod$b,
                         .mod$ci.lb,
                         .mod$ci.ub,
                         
                         sqrt(.mod$tau2) )
    } 
    
    
    if ( .mod.type == "robu" ) {
      
      .res = data.frame( .mod$b.r,
                         .mod$reg_table$CI.L,
                         .mod$reg_table$CI.U,
                         .mod$reg_table$prob,
                         
                         sqrt(.mod$mod_info$tau.sq) )
    } 
    
  } else {
    .res = data.frame( rep(NA, 6) )
  }
  
  #### Specific to Valentine:
  if ( .analysis == "zscale" ) {
    names(.res) = paste( c("EstFish", "LoFish", "HiFish", "EstPval", "TauFish"), .suffix, sep = "" )
    
    # also transform back to Pearson's R
    .res$EstR = MetaUtility::z_to_r(.res$EstFish)
    .res$LoR = MetaUtility::z_to_r(.res$LoFish)
    .res$HiR = MetaUtility::z_to_r(.res$HiFish)
  }
  
  if ( .analysis == "rscale" ) {
    names(.res) = paste( c("EstR", "LoR", "HiR", "EstPval", "TauR"), .suffix, sep = "" )
  }
  #### End of Valentine-specific code
  
  
  row.names(.res) = NULL
  
  return( list(stats = .res) )
}

# quick mean with NAs removed
meanNA = function(x){
  mean(x, na.rm = TRUE)
}

# quick median with NAs removed
medNA = function(x){
  median(x, na.rm = TRUE)
}

# quick length(unique) equivalent
nuni = function(x){
  length(unique(x))
}

expit = function(x) 1 / (1+exp(-x))

logit = function(x) log(x/(1-x))


# ~ Handling strings -----------------

# return strings containing anything in pattern vector
stringsWith = function(pattern, x){
  # make regex expression 
  patterns = paste(pattern, collapse="|")
  x[ grepl(pattern = patterns, x = x)]
}
# stringsWith( pattern = c("dog", "cat"),
#  x = c("dogcat", "horse", "cat", "lion") )


# return indices of strings containing anything in pattern vector
whichStrings = function(pattern, x){
  patterns = paste(pattern, collapse="|")
  grepl(pattern = pattern, x = x)
}

names_with = function(.dat, .pattern) {
  names(.dat)[ grepl(pattern = .pattern, x = names(.dat) ) ]
}



# ~ Calculate simple stats -----------------

quick_ci = function( est, var ) {
  c( est - qnorm(.975) * sqrt(var),
     est + qnorm(.975) * sqrt(var) )
}

quick_pval = function( est, var ) {
  2 * ( 1 - pnorm( abs( est / sqrt(var) ) ) )
}


# ~ Formatting stats as strings -----------------

# round while keeping trailing zeroes
my_round = function(x, digits) {
  formatC( round( x, digits ), format='f', digits=digits )
}

format_CI = function( lo, hi, digits ) {
  paste( "[", my_round( lo, digits ), ", ", my_round( hi, digits ), "]", sep="" )
}

# S-value for meta-analysis publication bias
# round down to nearest integer
format_sval = function( sval, digits ) {
  if ( as.character(sval) == "--" ) return("Already NS")
  else if ( as.character(sval) == "Not possible" ) return("Not possible")
  else return( as.character( round(sval, digits) ) )
  #else return( floor(sval) )
}

format_pval = function(p) {
  if (p >= 0.01) return( my_round( p, 2 ) )
  if (p < 0.01 & p > 10^-5 ) return( formatC( p, format = "e", digits = 0 ) )
  if ( p < 10^-5 ) return("< 1e-05")
}


