# Education in GBD 2017

This is a collection of code for estimating the GBD education covariate, education distributions, &amp; as a risk factor

load_edu_dta - reads .dta files, prints file name
import_prepped_data - lists files to be read, reads them into a data.frame "surveys"
collapse_surveys - collapses surveys by year, location, type, nid, age, bins then cleans data; creates proportion, mean, and sd vars then outputs tabulated data.
collapse_binned
import_tabulations - lists tabulated data then reads it into a data.frame "edu.data"
clean_tabulations - cleans tabulations that may have unstandardized varnames, removes missing data
xwalk_data - crosswalks data using training set which has the same binned age range
age_split_tabulations - age split tabulations when not in ihme age groups
load_all_data - after all data prep is done, loads tabulated data, census data, microdata into a list
compile_education-distributions - combines list produced by load_all_data into one data.frame, outputs as education_distributions.csv
compile_education_means = reduces distributions to summary statistics for covariate use, removes outliers and low-quality data; writes sources_record.csv of counts of data sources
back_cast - produces back_casted data based on cohort model. 
for_cast - produces forcasted data based on cohort model
create_back_and_fore_casts - merges data and back/forecasted data
fit_mixed_effects_model - fit mixed effects model to the data (linear model with fixed effects on year and random effects on place-age, and splines on the age start at 35, 50, 65)
add_non_sampling_error - compares observed mean to prior mean to compute NSE, adds it to mean_se (sampling error)
delta_transform_variance - delta tranforms variance
calculate_regional_MAD - calculate regional MAD
save_mixed_effects_model - outputs model results, cleaned with standardized names and merged on GBD location hierarchy; saves as linear_prior.csv
load_GPR_draws - lists gpr output as "estimates"
collapse_GPR_menas <- collpase estimates of gpr draws