# *------------------------------------------------------------------
# | PROGRAM NAME: process_streamflows
# | FILE NAME: process_streamflows.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *----------------------------------------------------------------
# | PURPOSE:  Fill me in         
# |
# *------------------------------------------------------------------
# | COMMENTS:               
# |
# |  1:  Learned that Justin DeRose calculated annual from daily means, rather
# |      than monthly means.  So, adjusted to process daily flows and convert
# |      to monthly and annual.
# |  2:  Done to match Justin's calculations, but could consider gap filling using zoo.
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
data_path <- "../../Data"
output_path <- "../../Output"
global_path <- "../global_func"
function_path <- "./functions"

### Set global output location
output_path <- file.path(output_path,"paleo_monthly")

output_name <- "observed_utah_flow"

data_path <- file.path(data_path,output_name)
write_output_path <- file.path(output_path,output_name)

###########################################################################
###  Load functions
###########################################################################
### Load these functions for all code
require(colorout)
require(assertthat)

### Load these functions for this unique project
require(zoo)

#source(file.path(global_path,"usgs_month_dl.R"))
source(file.path(global_path,"usgs_daily_dl.R"))
source(file.path(global_path,"usgs_readin.R"))
source(file.path(global_path,"usgs_wateryear.R"))
source(file.path(global_path,"unit_conversions.R"))

###########################################################################
## Set Initial Values
###########################################################################
### Set site data
site_id_list <- c("10109001", "10011500")
site_name_list <- c("Logan Utah", "Bear River near Utah-Wyo")

param_cd <- "00060"

wy_first_month <- 10 ### This is the default USGS water year, starting on Oct 1, it also follows Justin DeRose's reconstruction of MAF

###########################################################################
###  Save Daily Data
###########################################################################
### Loop through all site_ids and save time series data
for (n in seq(1,length(site_id_list))) {
	usgs_daily_dl(site_id_list[n], "00060", destination_folder=data_path)
}


###########################################################################
###  Read in Daily Data and Process to Monthly and Annual
###########################################################################
### Loop through all site_ids and save time series data
for (n in seq(1,length(site_id_list))) {

	### Set site id from list
	site_id <- site_id_list[n]
	site_name <- site_name_list[n]
	
	### Read in observed flows
	flow_obs <- usgs_readin(site_id, param_cd="00060", destination_folder=data_path)

	### Rename columns
	colnames(flow_obs)[2] <- "site_id"
	flow_obs$site_id <- as.factor(flow_obs$site_id)
	colnames(flow_obs)[3] <- "date"
	flow_obs$date <- as.Date(flow_obs$date)
	colnames(flow_obs)[4] <- "flow_cfs"
	colnames(flow_obs)[5] <- "cd"

	### Apply a short name
	flow_obs$short_name <- as.factor(site_name)

	### Calculate date, month and year
	flow_obs$month <- month(flow_obs$date)
	flow_obs$year <- year(flow_obs$date)
	
	### Calculate water year
	flow_obs$water_year <- usgs_wateryear(year=flow_obs$year, month=flow_obs$month, first_month=wy_first_month)

	### Convert to m3/s
	flow_obs$flow_m3s <- flow_obs$flow_cfs * ft3_to_m3 

	### Create a datatable with keys to allow for Monthly and Annual mean calculations
	flow_obs <- data.table(flow_obs)
	setkey(flow_obs, site_id, month, year, water_year)

	### Calculate mean monthly flow and then re-sort by year and month
	### Assigns a date as the mean day of each month (only to be used for plotting)
	flow_obs_monthly <- flow_obs[,list(date=mean(date), monthly_sum=sum(flow_m3s), monthly_mean=mean(flow_m3s), site_name=site_name),by=c("year", "month", "site_id", "water_year")]
	flow_obs_monthly <- flow_obs_monthly[order(rank(year), month)]
	setkey(flow_obs_monthly, water_year)

	### Calculate mean annual flow based on water year and set water_year as key for sorting and merging
	flow_obs_annual <- flow_obs[,list(annual_sum=sum(flow_m3s), annual_mean=mean(flow_m3s)),by="water_year"]
	flow_obs_annual$site_id <- site_id
	flow_obs_annual$site_name <- site_name
	setkey(flow_obs_annual, water_year)

	### Calculate monthly proportion of annual flow and mean proportion
	flow_obs <-  merge(flow_obs_monthly,flow_obs_annual, by=c("site_id", "site_name", "water_year"), all.x = T)

	### Write to data frame
	write_location <- file.path(write_output_path, paste0(site_id,"_",param_cd,"_mon_wy.csv"))
	write.csv(flow_obs, file = write_location,row.names=FALSE)
	}





### Found that USGS monthly is slightly different from Justin's monthly values.
### This code would work if we wanted USGS monthly values

###########################################################################
###  Save Daily Data
###########################################################################
### Loop through all site_ids and save time series data
#for (n in seq(1,length(site_id_list))) {
#	usgs_month_dl(site_id_list[n], "00060", destination_folder=data_path)
#}

###########################################################################
###  Read in Monthly Data and process
###########################################################################
### Loop through all site_ids and save time series data
#for (n in seq(1,length(site_id_list))) {
	### Set site id from list
#	site_id <- site_id_list[n]
	
	### Read in observed flows
#	flow_obs <- usgs_readin(site_id, param_cd="00060", destination_folder=data_path)

	### Calculate water year
#	flow_obs$water_year <- usgs_wateryear(year=flow_obs$year_nu, month=flow_obs$month_nu)

	### Calculate date
#	flow_obs$date <- as.Date(as.yearmon(paste(flow_obs$year_nu,"-",flow_obs$month_nu,"-01",sep="")), frac = 1)

	### Write to data frame
#	write_location <- file.path(write_output_path, paste0(site_id,"_",param_cd,"_mon.csv"))
#	write.csv(flow_obs, file = write_location,row.names=FALSE)

#}
