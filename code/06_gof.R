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


##########################################################################
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
require(ggplot2)

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

ref_period <- c(1925,2005)


#################################################
### Read AP APR scenario matrix
#################################################
run_scenarios_mf <- read.csv(file.path(write_output_base_path, "run_scenarios_mf.csv"))
run_scenarios_ap_apr <- read.csv(file.path(write_output_base_path, "run_scenarios_ap_apr.csv"))
run_scenarios <- rbind(run_scenarios_mf, run_scenarios_ap_apr)

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




#################################################
### Loop through all scenarios and calculate GOF
#################################################

for (n in seq(1,dim(run_scenarios)[1])) {

### Read in scenario information
site_n <- as.character(run_scenarios$site[n])
site_name_n <- as.character(run_scenarios$site_name[n])
method_n <- as.character(run_scenarios$method[n])
model_name_n <- as.character(run_scenarios$model_name[n])
reg_eq_n <- as.character(run_scenarios$reg_eq[n])
data_n <- as.character(run_scenarios$data[n])

################################################
### Set data for scenario
#################################################
annual_rec_ts <- get(paste0(site_name_n, "_annual_",data_n,"_ts"))
monthly_obs_ts <- get(paste0(site_name_n, "_monthly_obs_ts"))

### Set output folder for time series and make sure folder exists
read_path <- file.path(file.path(write_output_base_path, "ts"), site_n)
### Read time series
reconst_ts <- read.csv(file.path(read_path, paste0(model_name_n, "_ts.csv")), row.names = 1)
#reconst_ts_postproc <- read.csv(file.path(read_path, paste0(model_name_n, "_postproc_ts.csv")), row.names = 1)
### Remove t column because it fouls merging
reconst_ts <- reconst_ts[,!(colnames(reconst_ts) %in% "t")]

################################################
### Merge data
#################################################
### For water years, go one year before and after to make sure it works
### Then calculate water years
year_range <- c(min( reconst_ts$year, na.rm=TRUE)-1, max( reconst_ts$year, na.rm=TRUE)+1)
monthly_ts <- expand.grid(month = seq(1,12), year = seq(year_range[1], year_range[2]))
monthly_ts$water_year <- usgs_wateryear(monthly_ts$year, monthly_ts$month, first_month_wy)
		
### Set an order column to allow easier resorting at the end
monthly_ts$t <- seq(1,dim(monthly_ts)[1])
		
### Merge annual flows and month
monthly_ts <- merge(monthly_ts, annual_rec_ts$ts, all.x=TRUE)
### Rename flow column to annual
names(monthly_ts)[which(names(monthly_ts) == "flow")] <- "flow_annual"
### Merge observed flows and month
monthly_ts <- merge(monthly_ts, monthly_obs_ts$ts, all.x=TRUE)
### Rename flow column to monthly observed
names(monthly_ts)[which(names(monthly_ts) == "flow")] <- "flow_obs"
### Merge reconstructed flows and month
monthly_ts <- merge(monthly_ts, reconst_ts, all.x=TRUE)

### Re-sort to obtain time series and rename
reconst_ts <- monthly_ts[with(monthly_ts, order(t)), ]

### Create date column
reconst_ts$date <- as.Date(paste0(reconst_ts$year, "-", reconst_ts$month, "-15"))

###########################################################################
### Calculate goodness of fit statistics for annual fit
###########################################################################
### Calculate gof statistics
gof_annual_temp<- gof_ts(pred=reconst_ts$flow_est, obs=reconst_ts$flow_obs)
### Merge with run scenario table
gof_annual_temp <- cbind(run_scenarios[n,], gof_annual_temp)

### Save gof to a combined dataframe
if (n == 1) {
	gof_annual <- gof_annual_temp
} else {
	gof_annual <- rbind(gof_annual, gof_annual_temp)
}

###########################################################################
### Plot time series
###########################################################################
### Set up output folders
write_output_path <- file.path(file.path(write_figures_base_path,"ts"), site_n)
dir.create(file.path(write_output_path,"png"), recursive=TRUE)
dir.create(file.path(write_output_path,"pdf"), recursive=TRUE)
dir.create(file.path(write_output_path,"svg"), recursive=TRUE)

### Plot timeseries against observed
plot_result <- ts_plot(data=reconst_ts, write_folder= write_output_path, write_file=model_name_n)

###########################################################################
### Plot Residuals
###########################################################################
### Create residual plots
write_output_path <- file.path(file.path(write_figures_base_path,"gof"), site_n)
plot_result <- gof_plot_wrapper(x.obs=reconst_ts$flow_obs, x.pred=reconst_ts$flow_est, x.factor=reconst_ts$month, write_folder= write_output_path, write_file=model_name_n)

### Save to csv
write_output_path <- file.path(file.path(write_output_base_path, "gof"), site_n)
suppressWarnings(dir.create(write_output_path, recursive=TRUE))
write.csv(plot_result, file = file.path(write_output_path, paste0(model_name_n,"_month_gof.csv")), row.names=TRUE)

###########################################################################
### Calculate goodness of fit statistics for monthly fit
###########################################################################
### Combine monthly gof results and merge with run scenario table
gof_month_temp <- data.frame(t(plot_result))
gof_month_temp <- data.frame(month=seq(1,12), gof_month_temp)
gof_month_temp <- cbind(run_scenarios[rep(n, each=12),], gof_month_temp)

if (n == 1) {
	gof_month <- gof_month_temp
} else {
	gof_month <- rbind(gof_month, gof_month_temp)
}


}

###########################################################################
### Save Annual and Monthly Results to csv
###########################################################################
for (i in unique(run_scenarios$site)){
### Create folder
write_output_path <- file.path(file.path(write_output_base_path, "gof"), i)
suppressWarnings(dir.create(write_output_path, recursive=TRUE))

### Save to file
write.csv(subset(gof_annual, site == i), file = file.path(write_output_path, paste0(i, "_annual_gof.csv")), row.names=TRUE)
write.csv(subset(gof_month, site == i), file = file.path(write_output_path, paste0(i, "_monthly_gof.csv")), row.names=TRUE)

}



###########################################################################
## Plot Monthly GOF 
###########################################################################
for (n in seq(1,length(site_id_list))) {

site_n <- as.character(site_id_list[n])

### Read in monthly goodness of fit statistics
gof_month_site <- subset(gof_month, site==site_n)
gof_month_site$model <- paste0(gof_month_site$method,"_", gof_month_site$predictors)

### Re-factor months to water year
gof_month_site$month <- factor(gof_month_site$month, levels=c(seq(10,12), seq(1,9)))

### subset to remove lags
include_models <- c("mf_NA", "ap_NA", "apr_flow_lag", "apr_clim_only", "apr_clim_pca_impute")
gof_month_site <- gof_month_site[gof_month_site$model %in% include_models,]

### Re-factor models to rename levels
gof_month_site$model <- factor(gof_month_site$model, levels=include_models, labels=c("MF Model", "AP Model", "APR Model\nwith Lags", "APR Climate\nIndex Model", "APR Full\nModel"))

### Create location to save plots
write_output_path <- file.path(file.path(file.path(write_figures_base_path,"gof"), site_n), "stats_monthly")
suppressWarnings(dir.create(file.path(write_output_path, "png"), recursive=TRUE))
suppressWarnings(dir.create(file.path(write_output_path, "pdf"), recursive=TRUE))
suppressWarnings(dir.create(file.path(write_output_path, "svg"), recursive=TRUE))

### Set up loop over data
data_list <- as.character(unique(gof_month_site$data))

### Set color palette
color_list <- c("#4477AA", "#CCBB44", "#228833", "#EE6677", "#66CCEE")

for (q in seq(1,length(data_list))) {

### Extract the data for plotting
data_q <- data_list[q]
month_gof_q <- subset(gof_month_site, data==data_q)

### Mean Error Plot
param_list <- list(x="month", y = "ME", group="model", color="model")
param_names <- list(x="Month", y = expression(bold(paste("Mean Error  ( ",m^3,"/s )"))), color="Model")
p <- gof_month_plot2(data=month_gof_q, param_list=param_list, param_names=param_names, y_hor_line=c(0), colors=color_list)
### Save plot
ggsave(paste0(write_output_path,"/png/", site_n, "_", data_q, "_ME_by_month.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_output_path,"/pdf/", site_n, "_", data_q, "_ME_by_month.pdf"), p, width=5, height=4)
ggsave(paste0(write_output_path,"/svg/", site_n, "_", data_q, "_ME_by_month.svg"), p, width=5, height=4)

### Mean Absolute Error Plot
param_list$y <- "MAE"
param_names$y <- expression(bold(paste("Mean Absolute Error  ( ",m^3,"/s )")))
p <- gof_month_plot2(data=month_gof_q, param_list=param_list, param_names=param_names, y_hor_line=NULL, colors=color_list)
### Save plot
ggsave(paste0(write_output_path,"/png/", site_n, "_", data_q, "_MAE_by_month.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_output_path,"/pdf/", site_n, "_", data_q, "_MAE_by_month.pdf"), p, width=5, height=4)
ggsave(paste0(write_output_path,"/svg/", site_n, "_", data_q, "_MAE_by_month.svg"), p, width=5, height=4)

### Pearson Correlation Plot
param_list$y <- "R"
param_names$y <- "Pearson Correlation (R)"
p <- gof_month_plot2(data=month_gof_q, param_list=param_list, param_names=param_names, y_hor_line=c(0,1), colors=color_list)
### Save plot
ggsave(paste0(write_output_path,"/png/", site_n, "_", data_q, "_R_by_month.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_output_path,"/pdf/", site_n, "_", data_q, "_R_by_month.pdf"), p, width=5, height=4)
ggsave(paste0(write_output_path,"/svg/", site_n, "_", data_q, "_R_by_month.svg"), p, width=5, height=4)

### Spearman's Rho Plot
param_list$y <- "R.spear"
param_names$y <- expression(bold(paste("Spearman's Rho  ( ",rho," )")))
p <- gof_month_plot2(data=month_gof_q, param_list=param_list, param_names=param_names, y_hor_line=c(1), colors=color_list)
### Save plot
ggsave(paste0(write_output_path,"/png/", site_n, "_", data_q, "_Rspear_by_month.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_output_path,"/pdf/", site_n, "_", data_q, "_Rspear_by_month.pdf"), p, width=5, height=4)
ggsave(paste0(write_output_path,"/svg/", site_n, "_", data_q, "_Rspear_by_month.svg"), p, width=5, height=4)

### RMSE
param_list$y <- "RMSE"
param_names$y <- expression(bold(paste("RMSE  ( ",m^3,"/s )")))
p <- gof_month_plot2(data=month_gof_q, param_list=param_list, param_names=param_names, y_hor_line=NULL, colors=color_list)
### Save plot
ggsave(paste0(write_output_path,"/png/", site_n, "_", data_q, "_RMSE_by_month.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_output_path,"/pdf/", site_n, "_", data_q, "_RMSE_by_month.pdf"), p, width=5, height=4)
ggsave(paste0(write_output_path,"/svg/", site_n, "_", data_q, "_RMSE_by_month.svg"), p, width=5, height=4)

### Nash Sutcliffe Efficiency Plot
param_list$y <- "NSE"
param_names$y <- "Nash-Sutcliffe Efficiency"
p <- gof_month_plot2(data=month_gof_q, param_list=param_list, param_names=param_names, y_hor_line=c(0,1), colors=color_list)
p <- p + coord_cartesian(ylim=c(-1,1))
### Save plot
ggsave(paste0(write_output_path,"/png/", site_n, "_", data_q, "_NSE_by_month.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_output_path,"/pdf/", site_n, "_", data_q, "_NSE_by_month.pdf"), p, width=5, height=4)
ggsave(paste0(write_output_path,"/svg/", site_n, "_", data_q, "_NSE_by_month.svg"), p, width=5, height=4)

### Save publication Figure 6a
if (site_n == "10109001" & data_q == "obs"){
ggsave(paste0(pub_path,"/png/fig_6a.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(pub_path,"/pdf/fig_6a.pdf"), p, width=5, height=4)
ggsave(paste0(pub_path,"/svg/fig_6a.svg"), p, width=5, height=4)
}
### Save publication Figure 6b
if (site_n == "10109001" & data_q == "rec_region"){
ggsave(paste0(pub_path,"/png/fig_6b.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(pub_path,"/pdf/fig_6b.pdf"), p, width=5, height=4)
ggsave(paste0(pub_path,"/svg/fig_6b.svg"), p, width=5, height=4)
}
### Save publication Figure 6c
if (site_n == "10011500" & data_q == "obs"){
ggsave(paste0(pub_path,"/png/fig_6c.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(pub_path,"/pdf/fig_6c.pdf"), p, width=5, height=4)
ggsave(paste0(pub_path,"/svg/fig_6c.svg"), p, width=5, height=4)
}
### Save publication Figure 6d
if (site_n == "10011500" & data_q == "rec"){
ggsave(paste0(pub_path,"/png/fig_6d.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(pub_path,"/pdf/fig_6d.pdf"), p, width=5, height=4)
ggsave(paste0(pub_path,"/svg/fig_6d.svg"), p, width=5, height=4)
}


}
}


###########################################################################
## Generate Annual GOF and plot
###########################################################################
for (n in seq(1,length(site_id_list))) {

site_n <- as.character(site_id_list[n])

### Read in annual goodness of fit statistics
gof_annual_site <- subset(gof_annual, site==site_n)
gof_annual_site$model <- paste0(gof_annual_site$method,"_", gof_annual_site$predictors)

### subset to remove lags
include_models <- c("mf_NA", "ap_NA", "apr_flow_lag", "apr_clim_only", "apr_clim_pca_impute")
gof_annual_site <- gof_annual_site[gof_annual_site$model %in% include_models,]

### Re-factor models to rename levels
gof_annual_site$model <- factor(gof_annual_site$model, levels=include_models, labels=c("MF Model", "AP Model", "APR Model\nwith Lags", "APR Climate\nIndex Model", "APR Full\nModel"))

### Re-factor data inputs to rename levels
gof_annual_site$data <- factor(gof_annual_site$data, levels=c("obs", "rec", "rec_local", "rec_region"), labels=c("True Mean \nAnnual Flow","Reconstructed\n MAF", "Local Reconstructed  \nMAF","Regional Reconstructed  \nMAF"))

### Create location to save plots
write_output_path <- file.path(file.path(file.path(write_figures_base_path,"gof"), site_n), "stats_annual")
suppressWarnings(dir.create(file.path(write_output_path, "png"), recursive=TRUE))
suppressWarnings(dir.create(file.path(write_output_path, "pdf"), recursive=TRUE))
suppressWarnings(dir.create(file.path(write_output_path, "svg"), recursive=TRUE))

### Mean Error Plot
param_list <- list(x="model", y = "ME", group="data", color="data")
param_names <- list(x="\nModel", y = expression(bold(paste("Mean Error  ( ",m^3,"/s )"))), color="Data")
p <- gof_annual_plot(data=gof_annual_site, param_list=param_list, param_names=param_names, y_hor_line=c(0))
### Save plot
ggsave(paste0(write_output_path,"/png/", site_n, "_ME_annual.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_output_path,"/pdf/", site_n, "_ME_annual.pdf"), p, width=5, height=4)
ggsave(paste0(write_output_path,"/svg/", site_n, "_ME_annual.svg"), p, width=5, height=4)

### Mean Absolute Error Plot
param_list$y <- "MAE"
param_names$y <- expression(bold(paste("Mean Absolute Error  ( ",m^3,"/s )")))
p <- gof_annual_plot(data=gof_annual_site, param_list=param_list, param_names=param_names, y_hor_line=NULL)
### Save plot
ggsave(paste0(write_output_path,"/png/", site_n, "_MAE_annual.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_output_path,"/pdf/", site_n, "_MAE_annual.pdf"), p, width=5, height=4)
ggsave(paste0(write_output_path,"/svg/", site_n, "_MAE_annual.svg"), p, width=5, height=4)

### Pearson Correlation Plot
param_list$y <- "R"
param_names$y <- "Pearson Correlation (R)"
p <- gof_annual_plot(data=gof_annual_site, param_list=param_list, param_names=param_names, y_hor_line=c(0,1))
### Save plot
ggsave(paste0(write_output_path,"/png/", site_n, "_R_annual.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_output_path,"/pdf/", site_n, "_R_annual.pdf"), p, width=5, height=4)
ggsave(paste0(write_output_path,"/svg/", site_n, "_R_annual.svg"), p, width=5, height=4)

### Spearman's Rho Plot
param_list$y <- "R.spear"
param_names$y <- expression(bold(paste("Spearman's Rho  ( ",rho," )")))
p <- gof_annual_plot(data=gof_annual_site, param_list=param_list, param_names=param_names, y_hor_line=c(1))
### Save plot
ggsave(paste0(write_output_path,"/png/", site_n, "_Rspear_annual.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_output_path,"/pdf/", site_n, "_Rspear_annual.pdf"), p, width=5, height=4)
ggsave(paste0(write_output_path,"/svg/", site_n, "_Rspear_annual.svg"), p, width=5, height=4)

### RMSE
param_list$y <- "RMSE"
param_names$y <- expression(bold(paste("RMSE  ( ",m^3,"/s )")))
p <- gof_annual_plot(data=gof_annual_site, param_list=param_list, param_names=param_names, y_hor_line=NULL)
### Save plot
ggsave(paste0(write_output_path,"/png/", site_n, "_RMSE_annual.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_output_path,"/pdf/", site_n, "_RMSE_annual.pdf"), p, width=5, height=4)
ggsave(paste0(write_output_path,"/svg/", site_n, "_RMSE_annual.svg"), p, width=5, height=4)

### Nash Sutcliffe Efficiency Plot
param_list$y <- "NSE"
param_names$y <- "Nash-Sutcliffe Efficiency"
p <- gof_annual_plot(data=gof_annual_site, param_list=param_list, param_names=param_names, y_hor_line=c(0,1))
#p <- p + coord_cartesian(ylim=c(-1,1))
### Save plot
ggsave(paste0(write_output_path,"/png/", site_n, "_NSE_annual.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_output_path,"/pdf/", site_n, "_NSE_annual.pdf"), p, width=5, height=4)
ggsave(paste0(write_output_path,"/svg/", site_n, "_NSE_annual.svg"), p, width=5, height=4)

### Save publication Figure 7
if (site_n == "10109001"){
ggsave(paste0(pub_path,"/png/fig_7.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(pub_path,"/pdf/fig_7.pdf"), p, width=5, height=4)
ggsave(paste0(pub_path,"/svg/fig_7.svg"), p, width=5, height=4)
}


}



###########################################################################
## Create 1730s and 1970s figure  Pub Fig 8a and 8b
###########################################################################
site_n <- "10109001"
site_name_n <- "logan"
data_n <- "rec_region"

### Read in annual and observed
monthly_obs_ts <- get(paste0(site_name_n, "_monthly_obs_ts"))
monthly_obs_ts <- monthly_obs_ts$ts
colnames(monthly_obs_ts)[3] <- "flow_est"
monthly_obs_ts$method <- "flow_obs"

annual_rec_ts <- get(paste0(site_name_n, "_annual_",data_n,"_ts"))
annual_rec_ts <- annual_rec_ts$ts
year_range <- c(min(annual_rec_ts["water_year"], na.rm=TRUE)-1, max(annual_rec_ts["water_year"], na.rm=TRUE)+1)
monthly_ts <- expand.grid(month = seq(1,12), year = seq(year_range[1], year_range[2]))
monthly_ts$water_year <- usgs_wateryear(monthly_ts$year, monthly_ts$month, 10)
annual_rec_ts <- merge(monthly_ts, annual_rec_ts, all.x=TRUE)
colnames(annual_rec_ts)[4] <- "flow_est"
annual_rec_ts$method <- "flow_annual"

### Set output folder for time series and make sure folder exists
read_path <- file.path(file.path(write_output_base_path, "ts"), site_n)
### Read time series
reconst_ap <- read.csv(file.path(read_path, "10109001_ap_rec_region_ts.csv"), row.names = 1)
reconst_apr <- read.csv(file.path(read_path, "10109001_apr_rec_region_clim_pca_impute_ts.csv"), row.names = 1)

### Add method columns
reconst_ap$method <- "ap"
reconst_apr$method <- "apr"

### Combine all data
reconst_ts <- rbind(reconst_ap[,c("month", "year", "flow_est", "method")], reconst_apr[,c("month", "year", "flow_est", "method")])
reconst_ts <- rbind(reconst_ts, monthly_obs_ts[,c("month", "year", "flow_est", "method")])
reconst_ts <- rbind(reconst_ts, annual_rec_ts[,c("month", "year", "flow_est", "method")])

### Create date column 
reconst_ts$date <- as.Date(paste0(reconst_ts$year, "-", reconst_ts$month, "-15"))

### Factor Methods column
reconst_ts$method <- factor(reconst_ts$method, levels = c("flow_obs", "flow_annual", "ap", "apr"), labels = c("Observed Flow ", "Annual Reconstruction", "AP Model\nReconstructed Flow", "APR Model\nReconstructed Flow"))

test <- reconst_ts$method == "AP Model\nReconstructed Flow"

### Create plot
p <- ggplot(reconst_ts[!test,], aes(x=date, y=flow_est, color=method, linetype=method))
#p <- p + geom_line(data=monthly_obs_ts, size=0.25)
p <- p + geom_line(size=0.25)
p <- p + geom_line(data=subset(reconst_ts, method=="flow_annual"), size=0.45)
p <- p + scale_x_date(name="Date", breaks=seq(as.Date("800-01-01"), as.Date("2030-01-01"), by="5 years"), date_labels = "%Y")
p <- p + scale_y_continuous(name=expression(bold(paste("Monthly Mean Discharge  ( ",m^3,"/s )"))))
p <- p + theme_classic_new()
p <- p + coord_cartesian(xlim=c(as.Date("1715-01-01"), as.Date("1745-01-01")))
p <- p + scale_colour_manual(name= NULL, values = c("black", "#377eb8", "#e41a1c"))
p <- p + scale_linetype_manual(values=c("solid", "longdash", "solid", "solid"), guide=FALSE)
p <- p +  theme(legend.position="bottom")
p

### Save publication Figure 8a
ggsave(paste0(pub_path,"/png/fig_8a.png"), p, width=6, height=4, dpi=600)
ggsave(paste0(pub_path,"/pdf/fig_8a.pdf"), p, width=6, height=4)
ggsave(paste0(pub_path,"/svg/fig_8a.svg"), p, width=6, height=4)

### Save publication Figure 8b
p <- ggplot(reconst_ts, aes(x=date, y=flow_est, color=method, linetype=method))
#p <- p + geom_line(data=monthly_obs_ts, size=0.25)
p <- p + geom_line(size=0.25)
p <- p + geom_line(data=subset(reconst_ts, method=="flow_annual"), size=0.45)
p <- p + scale_x_date(name="Date", breaks=seq(as.Date("800-01-01"), as.Date("2030-01-01"), by="5 years"), date_labels = "%Y")
p <- p + scale_y_continuous(name=expression(bold(paste("Monthly Mean Discharge  ( ",m^3,"/s )"))))
p <- p + theme_classic_new()
p <- p + coord_cartesian(xlim=c(as.Date("1960-01-01"), as.Date("1975-01-01")))
p <- p + scale_colour_manual(name= NULL, values = c("black", "#377eb8", "#4daf4a", "#e41a1c"))
p <- p + scale_linetype_manual(values=c("solid", "longdash", "solid", "solid"), guide=FALSE)
p <- p +  theme(legend.position="bottom")
p

ggsave(paste0(pub_path,"/png/fig_8b.png"), p, width=6, height=4, dpi=600)
ggsave(paste0(pub_path,"/pdf/fig_8b.pdf"), p, width=6, height=4)
ggsave(paste0(pub_path,"/svg/fig_8b.svg"), p, width=6, height=4)




