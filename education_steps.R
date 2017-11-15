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
#   1. General options
#   2. Version control and directory options
#   3. Analytical steps
#   4. Database formatting
#   5. Graphing tools
#
###########################################

#############
# 1. Options
#############

team          <- "GBD"              #Can be either "GBD", "Forecasting", or "Risk Factors"
debug         <- T                  #If True, then code will only be run for a single location, year, age, sex
new_version   <- F                  #Create a new version? If not specified, version below must be specified.
steps         <- c(1, 2, 3, etc)    #Which analytical steps to run. If blank, all steps are run.
gbd_year      <- 2017

if (new_version == F){
  version     <- 1                  
}

#Initial notes detailing version's purpose
version_notes <- "Test run for new education code"

#debug
if debug == T{
	year == 
	age ==
	sex ==
	iso3 ==
}

#########################################
# 2. Version control & directory options
#########################################

main_dir    <- "/snfs1/WORK/01_covariates/02_inputs/education/update_2018/education_results/" 
version_dir <- paste0(main_dir, "/version_history/", team, "/")

#Source functions from funcations script
source('functions.R')

# If it's a new version of results, create the new version. Otherwise, overwrite a set of pre-existing results. User must manually input the version
if (new_version == T){
  version <- list.files(version_dir) %>% max %>% +1
}

sub_dir     <- paste0(main_dir, team, "/v", version)
input_dir   <- paste0(sub_dir, "/inputs/")           
output_dir  <- paste0(sub_dir, "/outputs/")
figure_dir  <- paste0(sub_dir, "/figures/")

date    <- Sys.time() %>% as.character %>% sub(" .*$", "", .) %>% gsub("-", "_", .)

#If directories don't already exist, make them
for (filepath in list(version_dir, input_dir, output_dir, figure_dir)){
  dir.create(filepath, recursive = T, showWarnings = F)
}

#Write initial log, with date stamp and version notes
log      <- paste0("Version: ", team, " v", version, "\n\n", "Date: ", date, "\n\n", "Description: ", version_notes)
log_file <- file(paste0(version_dir, "v", version, "_", date, ".txt"))
cat(log, file = log_file)
close(log_file)

jpath <- ifelse(Sys.info()[1]=="Windows", "J:/", "/home/j/")
cores <- 40
start_year <- 1950
if team == "Forecasting"{
	end_year <- 2100
}
else{
	end_year <- gbd_year
}

#############################
# 3. Templates and datasets
#############################
import_prepped_data()
collapse_surveys(surveys)
collapse_binned()
import_tabulations()
clean_tabulations(edu.data)
age_split_tabulations()

######################
# 4. Analytical steps
######################
load_all_data()
compile_education_distributions()

if team == "Risk factors" {
	stop("Education distributions written to education_distributions.csv")
}
else{
	compile_education_means()
	create_back_and_fore_casts()
	fit_mixed_effects_model()
	add_non_sampling_error()
	delta_transform_variance()
	calculate_regional_MAD()
	save_mixed_effects_model()
	#submitGPR
	load_GPR_draws()
	merge_pop_locs()
	rake_estimates()
}

#########################
# 5. Database formatting
#########################
make_education_covariate()
make_maternal_estimates()
make_agest_estimates()
print("covariates are contained in education_upload.csv, maternal_education_upload.csv, and agestd_education_upload.csv")
####################
# 6. Graphing tools
####################

