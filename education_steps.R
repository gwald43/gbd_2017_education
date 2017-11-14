rm(list=ls())
library(data.table)
library(plyr)
library(ggplot2)
#source(functions.R)

###########################################
# Analytical steps for GBD 2017 education
###########################################
#
# Code is structured as follows:
#   1. Options and desired outputs
#   2. Directories and datasets
#   3. Version control options
#   4. Analytical steps
#   5. Database formatting
#   6. Graphing tools
#
###########################################

#############
# 1. Options
#############

team        <- "GBD"              #Can be either "GBD", "Forecasting", or "Risk Factors"
debug       <- T                  #If True, then code will only be run for a single location, year, age, sex
steps       <- c(1, 2, 3, etc)    #Which analytical steps to run. If blank, all steps are run.
new_version <- T                  #Create a new version? If not specified, version below must be specified.

version <- NA 

###############################
# 2. Version control & options
###############################

version_dir <- paste0(main_dir, "/version_history/", team, "/")

# If it's a new version of results, create the new version. Otherwise, overwrite a set of pre-existing results. User must manually input the version
if (new_version == T){
  version <- list.files(version_dir) %>% max %>% +1
}else{
  version <- ""
}

main_dir    <- "/snfs1/WORK/01_covariates/02_inputs/education/" 
input_dir   <- paste0(main_dir, team, "/", version, "/inputs/")           
output_dir  <- paste0(main_dir, team, "/", version, "/outputs/")
figure_dir  <- paste0(main_dir, team, "/", version, "/figures/")

##############################
# 3. Directories and datasets 
##############################


date    <- Sys.time() %>% as.character %>% sub(" .*$", "", .) %>% gsub("-", "_", .)

           

# 4. Analytical steps

# 5. Database formatting

# 6. Graphing tools