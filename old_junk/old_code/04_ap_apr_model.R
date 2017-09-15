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

### Create output folders
write_output_base_path <- file.path(output_path, "paleo_reconst")
write_figures_base_path <- file.path(output_path,"figures")

### Create output folders
dir.create(write_output_base_path)
dir.create(write_figures_base_path)

###########################################################################
###  Load functions
###########################################################################
### Load these functions for all code
#require(colorout)
require(assertthat)
require(testthat)

### Load these functions for this unique project
#require(monthlypaleo)
require(staggefuncs)
require(paleoAPR)

###########################################################################
## Set Initial Values
###########################################################################
### Set site data
site_id_list <- c("10109001", "10011500")
site_name_list <- c("logan", "bear")
recons_file_name_list <- c("logan2013flow.txt", "bear2015flow.txt")

first_month_wy <- 10 ### Water Year starts on Oct 1
param_cd <- "00060"

monthly_distr <- rep("gamma",12)
annual_distr <- "logis"

ref_period <- c(1925,2005)


################################################
### Set up model scenario matrix
#################################################
### List of predictor scenarios
pred_list <- c("flow_lag", "clim_only","clim_only_lag","clim_pca_impute", "clim_pca_impute_lag")

flow_lag <- "l(flow, seq(-1,1))"
clim_pred <- " + ENSO + ENSO_short"
clim_lag <- paste0(" + l(", c("ENSO", "ENSO_short"), ",seq(-1,1))", collapse="")
pc_pred <- paste0(" + PC", seq(1,9), collapse="")
pc_lag <- paste0(" + l(PC", seq(1,9), ",seq(-1,1))", collapse="")


### Matrix of run scenarios
run_scenarios <- expand.grid(site="10109001", method="ap", data = c("obs", "rec_local", "rec_region"), predictors="NA")
run_scenarios <- rbind(run_scenarios, expand.grid(site="10109001", method="apr", data = c("obs", "rec_local", "rec_region"), predictors=pred_list))	
run_scenarios <- rbind(run_scenarios, expand.grid(site="10011500", method="ap", data = c("obs", "rec"), predictors="NA"))
run_scenarios <- rbind(run_scenarios, expand.grid(site="10011500", method="apr", data = c("obs", "rec"), predictors=pred_list))	

### Insert reconstruction file names
run_scenarios$site_name <- NA
run_scenarios$site_name[run_scenarios$site=="10109001"]  <- "logan"
run_scenarios$site_name[run_scenarios$site=="10011500"]  <- "bear"

### Name scenarios
run_scenarios$model_name <- paste0(run_scenarios$site,"_",run_scenarios$method,"_",run_scenarios$data, "_", run_scenarios$predictors)
ap_test <- run_scenarios$method=="ap"
run_scenarios$model_name[ap_test] <- paste0(run_scenarios$site[ap_test],"_",run_scenarios$method[ap_test],"_",run_scenarios$data[ap_test])

### Insert regression equations in appropriate place
run_scenarios$reg_eq <- NA
run_scenarios$reg_eq[run_scenarios$predictors=="flow_lag"] <- flow_lag
run_scenarios$reg_eq[run_scenarios$predictors=="clim_only"] <- paste0(flow_lag, clim_pred)
run_scenarios$reg_eq[run_scenarios$predictors=="clim_only_lag"] <- paste0(flow_lag, clim_lag)
run_scenarios$reg_eq[run_scenarios$predictors=="clim_pca_impute"] <- paste0(flow_lag, clim_pred, pc_pred)
run_scenarios$reg_eq[run_scenarios$predictors=="clim_pca_impute_lag"] <- paste0(flow_lag, clim_lag, pc_lag)

### Save scenario matrix
write.csv(run_scenarios, file.path(write_output_base_path, "run_scenarios_ap_apr.csv"), row.names=FALSE)

###########################################################################
## Read in reconstructed climate indices
###########################################################################
### Read in climate indices
clim_ind <- read.csv(file.path(file.path(data_path,"paleo_clim_ind"), "clim_ind.csv"))
### Remove ENSO_var, this is simply a running variance calculation - not useful as a predictor
clim_ind <- subset(clim_ind, select=-c(ENSO_var))
names(clim_ind)[1] <- "year"
### Create time series
clim_ind_ts <- paleo.ts(ts=clim_ind, time_scale="annual", site_prefix="clim_ind")

###########################################################################
## Read in tree ring PC scores
###########################################################################
### Read in PC scores
pc_score_impute <- read.csv(file.path(file.path(output_path,"pca_chronol"), "pc_score_impute.csv"))
names(pc_score_impute)[1] <- "water_year"
### Create time series
pc_ts <- paleo.ts(ts=pc_score_impute, time_scale="annual", site_prefix="tree_pc", wy_first_month=10)


################################################
### Read in and create Annual Flow Timeseries
#################################################
### Read in reconst flows (use fread because of large header)
logan_flow_recon <- read_table_wheaders(file.path(data_path,paste0("paleo_derose_annual_recon/logan2013flow.txt")), sep="\t", na.string="-9999")

bear_flow_recon <- read_table_wheaders(file.path(data_path,paste0("paleo_derose_annual_recon/bear2015flow.txt")), sep="\t", na.string="-9999")

### Create Observed and Reconstructed objects for Logan
logan_annual_obs_ts <- paleo.ts(ts=data.frame(water_year=logan_flow_recon$age_AD, flow=logan_flow_recon$flow.obs.m3s), time_scale="annual", site_prefix="10109001_obs", wy_first_month=10)

logan_annual_rec_local_ts <- paleo.ts(ts=data.frame(water_year=logan_flow_recon$age_AD, flow=logan_flow_recon$flow.rec.local.m3s), time_scale="annual", site_prefix="10109001_rec_local", wy_first_month=10)

logan_annual_rec_region_ts <- paleo.ts(ts=data.frame(water_year=logan_flow_recon$age_AD, flow=logan_flow_recon$flow.rec.region.m3s), time_scale="annual", site_prefix="10109001_rec_region", wy_first_month=10)

### Create Observed and Reconstructed objects for Bear
bear_annual_obs_ts <- paleo.ts(ts=data.frame(water_year=bear_flow_recon$age_AD, flow=bear_flow_recon$flow.obs.m3s), time_scale="annual", site_prefix="10011500_obs", wy_first_month=10)

bear_annual_rec_ts <- paleo.ts(ts=data.frame(water_year=bear_flow_recon$age_AD, flow=bear_flow_recon$flow.rec.m3s), time_scale="annual", site_prefix="10011500_rec", wy_first_month=10)


################################################
### Read in and create Monthly Observed Flow Timeseries
#################################################
for (n in seq(1, length(site_id_list))) {
	site_n <- site_id_list[n]

	### Read in monthly observed flow and fix data type
	obs_file_name <- paste0(site_n,"_",param_cd,"_mon_wy.csv")
	flow_obs <- read.csv(file.path(output_path,paste0("observed_utah_flow/",obs_file_name)))
	flow_obs$date <- as.Date(flow_obs$date)  
	
	### Create observed ts
	monthly_obs_ts <- paleo.ts(ts=data.frame(year=flow_obs$water_year, month=flow_obs$month, flow=flow_obs$monthly_mean), time_scale="monthly", site_prefix=paste0(site_n, "_obs"))
	### Assign to the correct name
	assign(paste0(site_name_list[n],"_monthly_obs_ts"), monthly_obs_ts)
	rm(monthly_obs_ts)	
}


###########################################################################
###  Set up a loop to run through all combinations using APR model
###########################################################################
for (n in seq(1,dim(run_scenarios)[1])) {

site_n <- as.character(run_scenarios$site[n])
site_name_n <- as.character(run_scenarios$site_name[n])
method_n <- as.character(run_scenarios$method[n])
model_name_n <- as.character(run_scenarios$model_name[n])
reg_eq_n <- as.character(run_scenarios$reg_eq[n])
data_n <- as.character(run_scenarios$data[n])

### Report progress
cat(paste(Sys.time()),"\n")
cat(paste0("Beginning Run:  ", n, "   of  ",dim(run_scenarios)[1],"\n"))
cat(paste0("Model:  ", model_name_n, "\n\n\n"))
flush(stdout()) 

################################################
### Set data for scenario
#################################################
annual_rec_ts <- get(paste0(site_name_n, "_annual_",data_n,"_ts"))
monthly_obs_ts <- get(paste0(site_name_n, "_monthly_obs_ts"))

################################################
### Check if normalization distribution exists and generate if not
#################################################
monthly_obs_norm_name <- paste0(site_name_n,"_monthly_obs_norm")
annual_rec_norm_name <- paste0(site_name_n,"_annual_",data_n,"_norm")

### Set output folder for normalization and make sure folder exists
write_output_path <- file.path(file.path(write_output_base_path, "norm_fit"), site_n)
dir.create(write_output_path, recursive=TRUE)
	
### If no monthly observed flow normalization, run normalization
if( !exists(monthly_obs_norm_name)){
	monthly_obs_norm <- fit_norm_dist(flow_series=monthly_obs_ts, distr=monthly_distr, ref_period=ref_period, save_dir=write_output_path)
	assign(monthly_obs_norm_name, monthly_obs_norm)
	
	### Save normalization object
	write_location <- file.path(write_output_path, paste0(site_n,"_monthly_obs_norm.rds"))
	saveRDS(monthly_obs_norm, write_location)
	
	rm(monthly_obs_norm)
}

### If no annual reconstructed flow normalization, run normalization
if( !exists(annual_rec_norm_name)){
	### Fit normalization and assign to variable
	annual_rec_norm <- fit_norm_dist(flow_series=annual_rec_ts, distr=annual_distr, ref_period=ref_period, save_dir=write_output_path)
	assign(annual_rec_norm_name, annual_rec_norm)
	
	### Save normalization object
	write_location <- file.path(write_output_path, paste0(site_n,"_annual_",data_n,"_norm.rds"))
	saveRDS(annual_rec_norm, write_location)
	
	rm(annual_rec_norm)
}

################################################
### Set normalization for scenario
#################################################
annual_rec_norm <- get(annual_rec_norm_name)
monthly_obs_norm <- get(monthly_obs_norm_name)


################################################
### Run Model
#################################################
if (method_n == "ap"){
	### Fit AP Model
	rec_model_fit <- fit_model(method="ap", reconst_data=annual_rec_ts, annual_norm=annual_rec_norm , monthly_norm=monthly_obs_norm)

} else if (method_n == "apr"){
	### Fit APR Model
	rec_model_fit <- fit_model(method="apr", regmethod="elastic", reconst_data=annual_rec_ts, annual_norm=annual_rec_norm, monthly_norm=monthly_obs_norm, pred_ts=list(clim_ind_ts, pc_ts), reg_eq = reg_eq_n, monthly_obs=monthly_obs_ts)
}

### Reconstruct flows
rec_model_ts <- flow_reconstr(recon_model=rec_model_fit, post_proc=FALSE)
	
################################################
### Write results to file
#################################################
### Set output folder for model fits and make sure folder exists
write_output_path <- file.path(file.path(write_output_base_path, "model_fit"), site_n)
dir.create(file.path(write_output_path, "coef"), recursive=TRUE)

### Write Coefficients to csv file
write_location <- file.path(write_output_path, paste0("coef/", model_name_n, "_coef.csv"))
write.csv(coef(rec_model_fit), file = write_location,row.names=TRUE)

### Set output folder for time series and make sure folder exists
write_output_path <- file.path(file.path(write_output_base_path, "ts"), site_n)
dir.create(write_output_path, recursive=TRUE)
### Write Reconstruction to csv file
write_location <- file.path(write_output_path, paste0(model_name_n, "_ts.csv"))
write.csv(rec_model_ts$ts$ts, file = write_location,row.names=TRUE)



################################################
### Run postprocessing for APR model to deal with loss of variance
#################################################
if (method_n == "apr"){
	### Run post-processing
	rec_model_ts_postproc <- flow_reconstr(recon_model=rec_model_fit, post_proc=TRUE)

post_proc_mat <- matrix(unlist(rec_model_ts_postproc$ts$post_proc),2,12, byrow=TRUE)
rownames(post_proc_mat) <- c("mean", "sd")

### Write postprocessing to csv file
dir.create(file.path(write_output_path, "postproc"), recursive=TRUE)
write_location <- file.path(file.path(file.path(write_output_base_path, "ts"), site_n), paste0("postproc/", model_name_n, "_postproc.csv"))
write.csv(post_proc_mat, file = write_location,row.names=TRUE)

### Write timseries to csv file
write_location <- file.path(write_output_path, paste0(model_name_n, "_postproc_ts.csv"))
write.csv(rec_model_ts_postproc$ts$ts, file = write_location,row.names=TRUE)
}

}


