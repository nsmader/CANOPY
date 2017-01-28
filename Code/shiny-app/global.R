#------------------------------------------------------------------------------#
#
# LOAD DATA AND FUNCTIONS FOR OPERATION OF BASKETBALL SIM
#
#------------------------------------------------------------------------------#

### Set Up Workspace -----------------------------------------------------------

rm(list=ls())
setwd("~/GitHub/CANOPY/Code/shiny-app/")
library(plyr)
library(data.table) # To race against join() and other methods of merging tables for speed
library(microbenchmark)
library(magrittr)
require(compiler)
enableJIT(0)

cn <- function(x) colnames(x)
rn <- function(x) rownames(x)
source("./prep-data.R")
source("./declare-canopy-method.R")
source("./declare-proposal-and-temp.R")

