
# Description: Functions for ETA correlations with new Xpose
# Date: 05-Jan-2021
# Author: K. Radtke

####################################
## 1. continuous variables       ##
####################################



# TODO:
# 1. call read_nm_tables() internally, unless user passes in a dataframe
# 2. first arg should be "runno" (run number). if runno is a number, then we
# assume that the model is called run[runno].mod. use model_paste0(runno, ".mod")
# 3. Make this work with multiple ETAs
# 4. eta arg should be a numeric vector
# 5. remove vars in favor of ...
# 6. allow ... to consist of unquoted covariate names, e.g. WT, HT, AGE instead
# of "WT", "HT", "AGE", possibly using enquos()





etacorr.cont <- function(s, # nonmem table file
                    eta, # eta in quotations, ie 'ETA1'
                    vars # a vector of variable names in quotes, ie c('WT','HT','AGE')
                    ) {
  eta <- sym(eta) # dplyr compatible format
  ss <- s[!duplicated(s$ID),] # 1 obs per ind

  plots <- list() # create blank
  for(i in 1:length(vars)) {
    var.select <- vars[i]
    var.select <- sym(var.select) # dplyr compatible format
    plots[[i]] <- ggplot(ss, aes(x=!!var.select, y=!!eta))+geom_point(shape=21)+geom_smooth(se=F)+
      geom_smooth(method='lm',se=F,linetype='dashed',color='red')

  }
  do.call(grid.arrange,plots)
}

#   Example of how to execute:
#
#     s <- read_nm_tables(1)
#     vars <- c('AGE','WT')
#     etacorr.cont(s,'ETA1',vars)



####################################
## 2. categorical variables       ##
####################################



etacorr.cat <- function(s, # nonmem table file
                         eta, # eta in quotations, ie 'ETA1'
                         vars # a vector of variable names in quotes, ie c('WT','HT','AGE')
                        ) {
  eta <- sym(eta) # dplyr compatible format
  ss <- s[!duplicated(s$ID), ] # 1 obs per ind

  plots <- list() # create blank
  for(i in 1:length(vars)) {
    var.select <- vars[i]
    var.select <- sym(var.select) # dplyr compatible format
    plots[[i]] <- ggplot(ss, aes(x=as.factor(!!var.select), y=!!eta))+
      geom_boxplot(outlier.colour = NA)

  }
  do.call(grid.arrange,plots)
}


#   Example of how to execute:
#
#     s <- read_nm_tables(1)
#     vars <- c('SEX','HIV')
#     etacorr.cat(s,'ETA1',vars)
