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
source(file.path(function_path,"gof_calcs.R"))
source(file.path(function_path,"null_model.R"))
source(file.path(function_path,"perc_fit.R"))

### Load global functions
source(file.path(global_path,"theme_classic_correct.R"))
source(file.path(global_path,"unit_conversions.R"))
source(file.path(global_path,"usgs_month_dl.R"))
source(file.path(global_path,"usgs_readin.R"))
source(file.path(global_path,"read_table_wheaders.R"))
source(file.path(global_path,"gof_bootstrap.R"))

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
###  Set up a loop to run through all site_ides and process the Null Model
###########################################################################
for (n in seq(1,length(site_id_list))) {

site_id <- site_id_list[n]
site_name <- site_name_list[n]
recons_file_name <- recons_file_name_list[n]

###########################################################################
###  Read in Data
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
### Calculate Monthly Percentile Fit
#################################################
for (j in 1:12) {
	### Create a test for the month and extract flows from observed for this month
	month_test <- flow_obs$month == j & flow_obs$water_year >= ref_period[1] & flow_obs$water_year <= ref_period[2]
	month_flows <- flow_obs$monthly_mean[month_test]
	
	### Run the distribution fit
	plot_name <- paste0("month_",j)
	month_fit <- perc_fit(flows=month_flows, distr=monthly_distr, plot_name=plot_name)
	
	if (j ==1) {
		fit_param <- month_fit
	} else {
		fit_param <- rbind(fit_param, month_fit)	
	}
}

### Write fit results to CSV file
write_location <- file.path(write_output_path, paste0(site_id,"_param_month_",monthly_distr,".csv"))
write.csv(fit_param, file = write_location,row.names=FALSE)


################################################
### Calculate Annual Percentile Fit
#################################################
### Create test for years in the reference period
year_test <- flow_recon$age_AD >= ref_period[1] & flow_recon$age_AD <= ref_period[2]

### Process observed annual flows
annual_flows <- flow_recon$flow.obs.m3s[year_test]
annual_fit <- perc_fit(flows=annual_flows, distr=annual_distr, plot_name="observ_annual")


### Process reconstructed flows
if (site_id == "10109001") {
### For this site there are two reconstructed time series (flow.rec.local.m3s and flow.rec.region.m3s)

### create column name and row name for analyis and results
col_name <- "flow.rec.local.m3s"
plot_name <- paste0("annual_",gsub("\\.", "_", col_name))
### Create a vector with annual flows from column col_name within reference years
annual_flows <- flow_recon[,c(col_name)][year_test]
### Perform fit on annual flows
annual_fit <- rbind(annual_fit, perc_fit(flows=annual_flows, distr=annual_distr, plot_name=plot_name))

### create column name and row name for analyis and results
col_name <- "flow.rec.region.m3s"
plot_name <- paste0("annual_",gsub("\\.", "_", col_name))
### Create a vector with annual flows from column col_name within reference years
annual_flows <- flow_recon[,c(col_name)][year_test]
### Perform fit on annual flows
annual_fit <- rbind(annual_fit, perc_fit(flows=annual_flows, distr=annual_distr, plot_name=plot_name))


} else {
### For other sites do reconstructed analysis
### create column name and row name for analyis and results
col_name <- "flow.rec.m3s"
plot_name <- paste0("annual_",gsub("\\.", "_", col_name))
### Create a vector with annual flows from column col_name within reference years
annual_flows <- flow_recon[,c(col_name)][year_test]
### Perform fit on annual flows
annual_fit <- rbind(annual_fit, perc_fit(flows=annual_flows, distr=annual_distr, plot_name=plot_name))
}


### Write fit results to CSV file
write_location <- file.path(write_output_path, paste0(site_id,"_param_annual_",annual_distr,".csv"))
write.csv(annual_fit, file = write_location,row.names=FALSE)

}


