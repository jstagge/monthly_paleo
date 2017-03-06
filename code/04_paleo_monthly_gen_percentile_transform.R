# *------------------------------------------------------------------
# | PROGRAM NAME: Data Music
# | FILE NAME: paleo_monthly_gen_null.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *----------------------------------------------------------------
# | PURPOSE:  This is a code wrapper to generate a midicsv file from data.
# | The resulting file can be processed into a midi file using the program csvmidi.
# | This midi file can then be played using timidity.
# | Check the ToRun.txt file for detailed code             
# |
# *------------------------------------------------------------------
# | COMMENTS:               
# |
# |  1:  
# |  2: 
# |  3: 
# |*------------------------------------------------------------------
# | DATA USED:               
# | This is a test instance using reconstructed climate indices ENSO and PDO
# |
# |*------------------------------------------------------------------
# | CONTENTS:               
# |
# |  PART 1:  
# |  PART 2: 
# |  PART 3: 
# *-----------------------------------------------------------------
# | UPDATES:               
# |
# |
# *------------------------------------------------------------------
 
### Clear any existing data or functions.
rm(list=ls())

###########################################################################
## Set the Paths
###########################################################################
### Path for Data and Output	
data_path <- "../../data"
output_path <- "../../output"
global_path <- "../global_func"
function_path <- "./functions"

### Set global output location
output_path <- file.path(output_path,"paleo_monthly")
output_name <- "percentile_model"

write_output_path <- file.path(file.path(output_path, "paleo_monthly_gen"), output_name)
write_figures_path <- file.path(file.path(output_path,"figures"),output_name)

###########################################################################
###  Load functions
###########################################################################
### Load these functions for all code
require(colorout)
require(assertthat)

### Load these functions for this unique project
require(data.table)
require(fitdistrplus)

### Load project specific functions
#source(file.path(function_path,"gof_calcs.R"))
#source(file.path(function_path,"null_model.R"))
#source(file.path(function_path,"perc_fit.R"))
file.sources = list.files(function_path, pattern="*.R")
sapply(file.path(function_path, file.sources),source)

### Load global functions
#source(file.path(global_path,"theme_classic_correct.R"))
#source(file.path(global_path,"unit_conversions.R"))
#source(file.path(global_path,"usgs_month_dl.R"))
#source(file.path(global_path,"usgs_readin.R"))
#source(file.path(global_path,"read_table_wheaders.R"))
#source(file.path(global_path,"gof_bootstrap.R"))
#source(file.path(global_path,"mid_month.R"))
#source(file.path(global_path,"usgs_wateryear.R"))
file.sources = list.files(global_path, pattern="*.R")
sapply(file.path(global_path, file.sources),source)

###########################################################################
## Set Initial Values
###########################################################################
### Set site data
site_id_list <- c("10109001", "10011500")
site_name_list <- c("Logan Utah", "Bear River near Utah-Wyo")
recons_file_name_list <- c("logan2013flow.txt", "bear2015flow.txt")

first_month_wy <- 10 ### Water Year starts on Oct 1
param_cd <- "00060"

monthly_distr <- "gamma"
annual_distr <- "logis"

ref_period <- c(1925,2005)



###########################################################################
###  Set up a loop to run through all site_ides and Transform based on the Percentile Model
###########################################################################
for (n in seq(1,length(site_id_list))) {

site_id <- site_id_list[n]
site_name <- site_name_list[n]
recons_file_name <- recons_file_name_list[n]

###########################################################################
###  Read in Flow Data
###########################################################################

### Read in observed flow and fix data type
obs_file_name <- paste0(site_id,"_",param_cd,"_mon_wy.csv")
flow_obs <- read.csv(file.path(output_path,paste0("observed_utah_flow/",obs_file_name)))
flow_obs$date <- as.Date(flow_obs$date)  
#head(flow_obs) # Review data frame

### Read in reconst flows (use fread because of large header)
flow_recon <- read_table_wheaders(file.path(data_path,paste0("paleo_derose_annual_recon/",recons_file_name)), sep="\t", na.string="-9999")

### Verify that monthly mean flows are equal.
### There is a rounding difference, so check the the maximum difference is less than tolerance of 0.002 m3/s (0.001 in either direction)
### Found that Bear River has a lot more disagreement (on the order of 0.03 m3/s
### Relaxing this requirement, but need to keep this in mind
### Delete created table after check
flow_merge <- merge(flow_obs, flow_recon, by.x="water_year", by.y="age_AD", all.x=TRUE)
assert_that(max(abs(flow_merge$annual_mean - flow_merge$flow.obs.m3s), na.rm=TRUE) < 0.04)
rm(flow_merge)

################################################
### Read in monthly and annual parameters to Transform Percentiles
#################################################
monthly_param <- list(param=read.csv(file.path(write_output_path, paste0(site_id,"_param_month_",monthly_distr,".csv"))), distr=monthly_distr)
monthly_param$param <- monthly_param$param[,c(2,3)]

annual_param <- list(param=read.csv(file.path(write_output_path, paste0(site_id,"_param_annual_",annual_distr,".csv"))), distr=annual_distr)
annual_param$param <- annual_param$param[,seq(1,3)]

#################################################
### Apply the Percentile Model to True Observed Annual Flows
#################################################
### Create dataframes for annual flows
annual_data <- data.frame(water_year=flow_obs$water_year, annual_flow=flow_obs$annual_mean)
### Only use one observation per year
annual_data <- unique(annual_data)

### Run Percentile Model
data_name <- "observ_annual"
month_ts <- perc_model(annual_rec=annual_data, monthly_param, annual_param,  		first_month_wy=first_month_wy, data_name=data_name)

### Save Results
save_reconstruction(month_ts, site_id, site_name, output_name, data_name="observ_annual", method="Percentile Model using True Annual Mean Flow")


#################################################
### Specific Catch for the Logan reconstruction, which has 2 reconstruction models
#################################################

if (site_id == "10109001") {
	#################################################
	### Apply the Percentile Model to Reconstructed "Local Model"
	#################################################
	### Create dataframes for annual flows
	annual_data <- data.frame(water_year=flow_recon$age_AD, annual_flow=flow_recon	$flow.rec.local.m3s)
	### Only use one observation per year
	annual_data <- unique(annual_data)

	### Run Percentile Model
	data_name <- "annual_flow_rec_local_m3s"
	month_ts <- perc_model(annual_rec=annual_data, monthly_param, annual_param,  		first_month_wy=first_month_wy, data_name=data_name)

	### Save Results
	save_reconstruction(month_ts, site_id, site_name, output_name, data_name="rec_local", method="Percentile Model using Local Reconstructed MAF")

	################################################
	### Apply the Null Model to Reconstructed "Regional Model"
	#################################################
	### Create dataframes for annual flows
	annual_data <- data.frame(water_year=flow_recon$age_AD, annual_flow=flow_recon	$flow.rec.region.m3s)
	### Only use one observation per year
	annual_data <- unique(annual_data)

	### Run Percentile Model
	data_name <- "annual_flow_rec_region_m3s"
	month_ts <- perc_model(annual_rec=annual_data, monthly_param, annual_param,  		first_month_wy=first_month_wy, data_name=data_name)

	### Save Results
	save_reconstruction(month_ts, site_id, site_name, output_name, data_name="rec_region", method="Percentile Model using Regional Reconstructed MAF")
	

} else {
	#################################################
	### Apply the Null Model to Generic Reconstructed Model
	#################################################
	### Create dataframes for annual flows
	annual_data <- data.frame(water_year=flow_recon$age_AD, annual_flow=flow_recon	$flow.rec.m3s)
	### Only use one observation per year
	annual_data <- unique(annual_data)

	### Run Percentile Model
	data_name <- "annual_flow_rec_m3s"
	month_ts <- perc_model(annual_rec=annual_data, monthly_param, annual_param,  		first_month_wy=first_month_wy, data_name=data_name)

	### Save Results
	save_reconstruction(month_ts, site_id, site_name, output_name, data_name="rec", method="Percentile Model using Reconstructed MAF")

}

}



