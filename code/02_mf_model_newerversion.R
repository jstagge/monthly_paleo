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

#################################################
### Create path for figures
#################################################
### Create folder for all figures
write_path <- file.path(write_figures_base_path,"model_fit")
dir.create(write_path, recursive=TRUE)

### Create folder for Publication Figures
pub_path <- file.path(write_figures_base_path,"publication")

dir.create(file.path(pub_path,"png"), recursive=TRUE)
dir.create(file.path(pub_path,"pdf"), recursive=TRUE)
dir.create(file.path(pub_path,"svg"), recursive=TRUE)

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

### Load project specific functions
file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
sapply(file.path(function_path, file.sources),source)

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
### Matrix of run scenarios
run_scenarios <- expand.grid(site="10109001", method="mf", data = c("obs", "rec_local", "rec_region"), predictors="NA")
run_scenarios <- rbind(run_scenarios, expand.grid(site="10011500", method="mf", data = c("obs", "rec"), predictors="NA"))

### Insert reconstruction file names
run_scenarios$site_name <- NA
run_scenarios$site_name[run_scenarios$site=="10109001"]  <- "logan"
run_scenarios$site_name[run_scenarios$site=="10011500"]  <- "bear"

### Name scenarios
run_scenarios$model_name <- paste0(run_scenarios$site,"_",run_scenarios$method,"_",run_scenarios$data)
run_scenarios$reg_eq <- NA
### Save scenario matrix
write.csv(run_scenarios, file.path(write_output_base_path, "run_scenarios_mf.csv"), row.names=FALSE)


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
###  Set up a loop to run through all combinations using MF model
###########################################################################
for (n in seq(1,dim(run_scenarios)[1])) {

site_n <- as.character(run_scenarios$site[n])
site_name_n <- as.character(run_scenarios$site_name[n])
method_n <- as.character(run_scenarios$method[n])
model_name_n <- as.character(run_scenarios$model_name[n])
data_n <- as.character(run_scenarios$data[n])

################################################
### Set data for scenario
#################################################
annual_rec_ts <- get(paste0(site_name_n, "_annual_",data_n,"_ts"))
monthly_obs_ts <- get(paste0(site_name_n, "_monthly_obs_ts"))

################################################
### Run Model
#################################################

rec_model_fit <- fit_model(method="mf", reconst_data=annual_rec_ts, monthly_obs=monthly_obs_ts)

### Reconstruct flows
rec_model_ts <- flow_reconstr(recon_model=rec_model_fit)

################################################
### Write results to file
#################################################
### Set output folder for model fits and make sure folder exists
write_output_path <- file.path(file.path(write_output_base_path, "model_fit"), site_n)
dir.create(file.path(write_output_path, "mf_frac"), recursive=TRUE)

### Write Coefficients to csv file
write_location <- file.path(write_output_path, paste0("mf_frac/", model_name_n, "_mf_frac.csv"))
write.csv(coef(rec_model_fit), file = write_location,row.names=TRUE)

### Set output folder for time series and make sure folder exists
write_output_path <- file.path(file.path(write_output_base_path, "ts"), site_n)
dir.create(write_output_path, recursive=TRUE)
### Write Reconstruction to csv file
write_location <- file.path(write_output_path, paste0(model_name_n, "_ts.csv"))
write.csv(rec_model_ts$ts$ts, file = write_location,row.names=TRUE)


################################################
### Create plot of monthly fractions
#################################################
### Create plot dataframes
mf_plot_df <- rec_model_fit$mf_prop$mf_ts
mf_plot_df$month <- factor(mf_plot_df$month, levels=c(seq(10,12),seq(1,9)))
mf_mean_df <- rec_model_fit$mf_prop$prop_mean
mf_mean_df$group <- "group"
mf_mean_df$month <- factor(mf_mean_df$month, levels=c(seq(10,12),seq(1,9)))

### Create plot of Monthly Fraction
p <- ggplot(mf_plot_df, aes(x=month))
p <- p + geom_line(aes(y=prop_month, group=water_year), colour="grey", size=0.15)
p <- p + geom_line(data=mf_mean_df, aes(y=prop_mean, group=group), colour="black", size=0.7)
p <- p + scale_x_discrete(expand= c(0,0), name="Month")
#p <- p + scale_y_continuous(name="Monthly Flow Proportion (%)\n", breaks=seq(0,100,5))
p <- p + scale_y_continuous(name="Monthly Flow Proportion\n", labels = scales::percent, breaks=seq(0,1,0.05))
p <- p + coord_cartesian(ylim=c(0,0.52), expand=FALSE)
#p <- p + coord_cartesian(ylim=c(0,52), expand=FALSE)
p <- p + theme_classic_new()
p

### Create location to save plots
write_output_path <- file.path(file.path(write_figures_base_path,"model_fit"), site_n)
suppressWarnings(dir.create(file.path(write_output_path, "png"), recursive=TRUE))
suppressWarnings(dir.create(file.path(write_output_path, "pdf"), recursive=TRUE))
suppressWarnings(dir.create(file.path(write_output_path, "svg"), recursive=TRUE))

### Save plot
ggsave(paste0(write_output_path,"/png/", model_name_n, "_prop.png"), p, width=6, height=4, dpi=600)
ggsave(paste0(write_output_path,"/pdf/", model_name_n, "_prop.pdf"), p, width=6, height=4)
ggsave(paste0(write_output_path,"/svg/", model_name_n, "_prop.svg"), p, width=6, height=4)

### Save publication Figure 2a
if (model_name_n == "10109001_mf_obs"){
ggsave(paste0(pub_path,"/png/fig_2a.png"), p, width=6, height=4, dpi=600)
ggsave(paste0(pub_path,"/pdf/fig_2a.pdf"), p, width=6, height=4)
ggsave(paste0(pub_path,"/svg/fig_2a.svg"), p, width=6, height=4)
}
### Save publication Figure 2b
if (model_name_n == "10011500_mf_obs"){
ggsave(paste0(pub_path,"/png/fig_2b.png"), p, width=6, height=4, dpi=600)
ggsave(paste0(pub_path,"/pdf/fig_2b.pdf"), p, width=6, height=4)
ggsave(paste0(pub_path,"/svg/fig_2b.svg"), p, width=6, height=4)
}

}


