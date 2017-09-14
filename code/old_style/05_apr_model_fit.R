# *------------------------------------------------------------------
# | PROGRAM NAME: 05_apr_model_fit
# | FILE NAME: 05_apr_model_fit.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *----------------------------------------------------------------
# | PURPOSE:  This is a code wrapper to fit the Annual Percentile with
# | regression (APR) model. It fits a regression equation, explaining
# | monthly percentile by lagged (-1,0,+1) annual percentile. 
# |
# |
# *------------------------------------------------------------------
# | COMMENTS:               
# |
# |  1:  
# |  2: 
# |  3: 
# |*------------------------------------------------------------------
# | DATA USED:               
# | USGS gauge flow data
# | Annual reconstructions from:
# | Allen, E.B., Rittenour, T.M., DeRose, R.J., Bekker, M.F., Kjelgren, R., Buckley, B.M., 2013. A tree-ring based reconstruction of Logan River streamflow, northern Utah. Water Resources Research 49, 8579–8588. doi:10.1002/2013WR014273.
# |
# | DeRose, R.J., Bekker, M.F., Wang, S.Y., Buckley, B.M., Kjelgren, R.K., Bardsley, T., Rittenour, T.M., Allen, E.B., 2015. A millennium-length reconstruction of Bear River stream flow, Utah. Journal of Hydrology 529, Part 2, 524–534. doi:10.1016/j.jhydrol.2015.01.014.
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
data_path <- "../data"
output_path <- "../output"
global_path <- "./global_func"
function_path <- "./functions"

### Set specific output location
output_name <- "apr_model"
write_output_base_path <- file.path(file.path(output_path, "paleo_reconst"), output_name)
write_figures_base_path <- file.path(file.path(output_path,"figures"),output_name)

### Create output folders
dir.create(write_output_base_path)
dir.create(write_figures_base_path)

###########################################################################
###  Load functions
###########################################################################
### Load these functions for all code
#require(colorout)
require(assertthat)

### Load these functions for this unique project

### Load project specific functions
file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
sapply(file.path(function_path, file.sources),source)

### Load global functions
file.sources = list.files(global_path, pattern="*.R", recursive=TRUE)
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

### Create output folders
dir.create(file.path(write_output_base_path, site_id))
dir.create(file.path(write_figures_base_path, site_id))
dir.create(file.path(write_figures_base_path, paste0(site_id, "/pdf")))
dir.create(file.path(write_figures_base_path, paste0(site_id, "/svg")))
dir.create(file.path(write_figures_base_path, paste0(site_id, "/png")))

write_figures_path <- file.path(write_figures_base_path, site_id)
write_output_path <- file.path(write_output_base_path, site_id)


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
percentile_path <- file.path(file.path(output_path, "paleo_reconst"), paste0("ap_model/",site_id))

monthly_param <- list(param=read.csv(file.path(percentile_path, paste0(site_id,"_param_month_",monthly_distr,".csv"))), distr=monthly_distr)
monthly_param$param <- monthly_param$param[,c(2,3)]

annual_param <- list(param=read.csv(file.path(percentile_path, paste0(site_id,"_param_annual_",annual_distr,".csv"))), distr=annual_distr)
annual_param$param <- annual_param$param[,seq(1,3)]

#################################################
### Apply the Percentile Model with Lagged years (1 year previous, 1 year following)
#################################################
### Fit model to observed annual and save coefficients
lag_fit <- apr_fit(data=flow_obs, monthly_param, annual_param, data_name="observ_annual")
### Write to csv file
write_location <- file.path(write_output_path, paste0(site_id,"_",output_name, "_coef_observ_annual.csv"))
write.csv(lag_fit, file = write_location,row.names=TRUE)

if (site_id == "10109001") {
### Fit model to Reconstructed "Local Model" and save coefficients
lag_fit <- apr_fit(data=flow_obs, monthly_param, annual_param, data_name="annual_flow_rec_local_m3s")
### Write to csv file
write_location <- file.path(write_output_path, paste0(site_id,"_",output_name, "_coef_rec_local.csv"))
write.csv(lag_fit, file = write_location,row.names=TRUE)

### Fit model to Reconstructed "Regional Model" and save coefficients
lag_fit <- apr_fit(data=flow_obs, monthly_param, annual_param, data_name="annual_flow_rec_region_m3s")
### Write to csv file
write_location <- file.path(write_output_path, paste0(site_id,"_",output_name, "_coef_rec_region.csv"))
write.csv(lag_fit, file = write_location,row.names=TRUE)

} else {

### Fit model to Reconstructed and save coefficients
lag_fit <- apr_fit(data=flow_obs, monthly_param, annual_param, data_name="annual_flow_rec_m3s")
### Write to csv file
write_location <- file.path(write_output_path, paste0(site_id,"_",output_name, "_coef_rec.csv"))
write.csv(lag_fit, file = write_location,row.names=TRUE)

}

}


#################################################
### Plot coefficients against lag
#################################################
predictor_levels <- c("annual_norm", "annual_norm_neg_1year", "annual_norm_pos_1year")
predictor_labels <- c("Concurrent Water Year\nStd Normal", "Previous (-1) Water Year\nStd Normal", "Next (+1) Water Year\nStd Normal")

### Loop through sites
for (n in seq(1,length(site_id_list))) {

### Read in site details
site_id <- site_id_list[n]
site_name <- site_name_list[n]

write_figures_path <- file.path(write_figures_base_path, site_id)
write_output_path <- file.path(write_output_base_path, site_id)

### Read in observed annual coefficients
data_name <- "observ_annual"
model_path <- write_output_path
norm_model <- read.csv(file.path(model_path, paste0(site_id,"_apr_model_coef_",data_name,".csv")),  row.names = 1)

### Save file
p <- norm_coef_plot(norm_model, predictor_levels=predictor_levels, predictor_labels=predictor_labels)

ggsave(paste0(file.path(write_figures_path,"png/"), site_id, "_apr_model_coef_", data_name,".png"), p, width=6, height=4, dpi=600)
ggsave(paste0(file.path(write_figures_path,"svg/"), site_id, "_apr_model_coef_", data_name,".svg"), p, width=6, height=4)
ggsave(paste0(file.path(write_figures_path,"pdf/"), site_id, "_apr_model_coef_", data_name,".pdf"), p, width=6, height=4)



#################################################
### Specific Catch for the Logan reconstruction, which has 2 reconstruction models
#################################################

if (site_id == "10109001") {
	#################################################
	### Apply the APR Model to Reconstructed "Local Model"
	#################################################
	### Read in Reconstructed "Local Model"
	data_name <- "rec_local"
	model_path <- write_output_path
	norm_model <- read.csv(file.path(model_path, paste0(site_id,"_apr_model_coef_",data_name,".csv")),  row.names = 1)

	### Save file
	p <- norm_coef_plot(norm_model, predictor_levels=predictor_levels, predictor_labels=predictor_labels)

	ggsave(paste0(file.path(write_figures_path,"png/"), site_id, "_apr_model_coef_", data_name,".png"), p, width=6, height=4, dpi=600)
	ggsave(paste0(file.path(write_figures_path,"svg/"), site_id, "_apr_model_coef_", data_name,".svg"), p, width=6, height=4)
	ggsave(paste0(file.path(write_figures_path,"pdf/"), site_id, "_apr_model_coef_", data_name,".pdf"), p, width=6, height=4)


	################################################
	### Apply the APR Model to Reconstructed "Regional Model"
	#################################################
	### Read in Reconstructed "Regional Model"
	data_name <- "rec_region"
	model_path <- write_output_path
	norm_model <- read.csv(file.path(model_path, paste0(site_id,"_apr_model_coef_",data_name,".csv")),  row.names = 1)

	### Save file
	p <- norm_coef_plot(norm_model, predictor_levels=predictor_levels, predictor_labels=predictor_labels)

	ggsave(paste0(file.path(write_figures_path,"png/"), site_id, "_apr_model_coef_", data_name,".png"), p, width=6, height=4, dpi=600)
	ggsave(paste0(file.path(write_figures_path,"svg/"), site_id, "_apr_model_coef_", data_name,".svg"), p, width=6, height=4)
	ggsave(paste0(file.path(write_figures_path,"pdf/"), site_id, "_apr_model_coef_", data_name,".pdf"), p, width=6, height=4)


} else {
	#################################################
	### Apply the APR Model to Generic Reconstructed Model
	#################################################
	### Read in Reconstructed 
	data_name <- "rec"
	model_path <- write_output_path
	norm_model <- read.csv(file.path(model_path, paste0(site_id,"_apr_model_coef_",data_name,".csv")),  row.names = 1)

	### Save file
	p <- norm_coef_plot(norm_model, predictor_levels=predictor_levels, predictor_labels=predictor_labels)

	ggsave(paste0(file.path(write_figures_path,"png/"), site_id, "_apr_model_coef_", data_name,".png"), p, width=6, height=4, dpi=600)
	ggsave(paste0(file.path(write_figures_path,"svg/"), site_id, "_apr_model_coef_", data_name,".svg"), p, width=6, height=4)
	ggsave(paste0(file.path(write_figures_path,"pdf/"), site_id, "_apr_model_coef_", data_name,".pdf"), p, width=6, height=4)
	

}


}




