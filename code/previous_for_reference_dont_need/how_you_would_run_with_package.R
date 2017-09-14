
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
output_name <- "ap_model"
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
require(testthat)

### Load these functions for this unique project
require(data.table)
require(fitdistrplus)

### Load project specific functions
#file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
#sapply(file.path(function_path, file.sources),source)

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
###  Set up a loop to run through all site_ides and process the Null Model
###########################################################################
#for (n in seq(1,length(site_id_list))) {
n <- 1

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


###########################################################################
## Read in reconstructed flows, tree rings and climate indices
###########################################################################
### Read in PC scores
pc_score_impute <- read.csv("../../../output/paleo_monthly/pca_chronol/PC_Score_impute.csv")
names(pc_score_impute)[1] <- "water_year"

### Read in climate indices
clim_ind <- read.csv(file.path(file.path(data_path,"paleo_clim_ind"), "clim_ind.csv"))
### Remove ENSO_var, this is simply a running variance calculation - not useful as a predictor
clim_ind <- subset(clim_ind, select=-c(ENSO_var))
names(clim_ind)[1] <- "year"

### Maybe make them monthly and merge


################################################
### Using New Function
#################################################
### Create flow timeseries
annual_obs_ts <- paleo.ts(ts=data.frame(water_year=flow_recon$age_AD, flow=flow_recon$flow.obs.m3s), time_scale="annual", site_prefix=paste0(site_id, "_obs"), wy_first_month=10)

annual_rec_region_ts <- paleo.ts(ts=data.frame(water_year=flow_recon$age_AD, flow=flow_recon$flow.rec.region.m3s), time_scale="annual", site_prefix=paste0(site_id, "_rec_regional"), wy_first_month=10)

monthly_obs_ts <- paleo.ts(ts=data.frame(year=flow_obs$water_year, month=flow_obs$month, flow=flow_obs$monthly_mean), time_scale="monthly", site_prefix=paste0(site_id, "_obs"))

clim_ind_ts <- paleo.ts(ts=clim_ind, time_scale="annual", site_prefix=paste0(site_id, "_clim_ind"))

pc_ts <- paleo.ts(ts=pc_score_impute, time_scale="annual", site_prefix=paste0(site_id, "_pc"), wy_first_month=10)

### Fit distribution
annual_obs_norm <- flow_perc_fit(flow_data=annual_obs_ts, distr="logis", ref_period=ref_period)

annual_rec_norm <- flow_perc_fit(flow_data=annual_rec_region_ts, distr="logis", ref_period=ref_period)

monthly_obs_norm <- flow_perc_fit(flow_data=monthly_obs_ts, distr="gamma", ref_period=ref_period)

### Fit the AP model
ap_model_fit <- fit_model(method="ap", reconst_data=annual_rec_region_ts, annual_norm=annual_rec_norm , monthly_norm=monthly_obs_norm)

### Reconstruction step
monthly_recon <- flow_reconstr(recon_model=ap_model_fit)


### This is internal while I figure out function 
method="apr"
reconst_data=annual_rec_region_ts
annual_norm=annual_rec_norm
 monthly_norm=monthly_obs_norm
pred_ts=list(clim_ind_ts, pc_ts)
reg_eq = "PC2 + l(flow,seq(-1,1)) + ENSO + l(PC1, seq(-2,1))"
monthly_obs=monthly_obs_ts

x <- paleo.fit(method="ap", reconst_data=reconst_data,annual_norm=annual_norm, monthly_norm=monthly_norm)
		### Calculate annual percentile and insert into object
		x$reconst_data$ts <- ap_fit(recon_data = x)
		x$reconst_data$time_scale <- "monthly"
		### Switch back to APR
		x$method <- "apr"
		### Add in predictors
		x$pred_ts <- pred_ts
		x$reg_eq <- reg_eq		
		x$monthly_obs <- monthly_obs
		### run the apr fitting
		yup <- apr_fit(recon_data = x)



### Fit the AP model
apr_model_fit <- fit_model(method="apr", reconst_data=annual_rec_region_ts, annual_norm=annual_rec_norm, monthly_norm=monthly_obs_norm, pred_ts=list(clim_ind_ts, pc_ts), reg_eq = "PC2 + l(flow,seq(-1,1)) + ENSO + l(PC1, seq(-2,1))", monthly_obs=monthly_obs_ts)

### Save the AP Model
save.image("working.RData")
load("working.RData")

### Now need reconstruction step
monthly_recon <- flow_reconstr(recon_model=apr_model_fit)



### Now need reconstruction step



### Fit the APR lagged model
reg_eq <- "l(flow,seq(-1,1))"
apr_lagged_fit <- fit_model(method="apr", reconst_data=annual_rec_region_ts, annual_norm=annual_rec_norm, monthly_norm=monthly_obs_norm, pred_ts=list(clim_ind_ts, pc_ts), reg_eq = reg_eq, monthly_obs=monthly_obs_ts)

### Fit the APR lagged model
reg_eq <- paste0(reg_eq, " + ENSO + ENSO_short")
apr_lagged_climind_fit <- fit_model(method="apr", reconst_data=annual_rec_region_ts, annual_norm=annual_rec_norm, monthly_norm=monthly_obs_norm, pred_ts=list(clim_ind_ts, pc_ts), reg_eq = reg_eq, monthly_obs=monthly_obs_ts)

### Fit the APR lagged model
reg_eq <- paste0(reg_eq , paste(" + PC", seq(1,8), sep="", collapse=""))
apr_all_fit <- fit_model(method="apr", reconst_data=annual_rec_region_ts, annual_norm=annual_rec_norm, monthly_norm=monthly_obs_norm, pred_ts=list(clim_ind_ts, pc_ts), reg_eq = reg_eq, monthly_obs=monthly_obs_ts)


### Fit the APR lagged model
reg_eq <- "l(flow,seq(-1,1))"
reg_eq <- paste0(reg_eq, " + ENSO ")
reg_eq <- paste0(reg_eq , paste(" + PC", seq(1,8), sep="", collapse=""))
apr_all_noshort_fit <- fit_model(method="apr", reconst_data=annual_rec_region_ts, annual_norm=annual_rec_norm, monthly_norm=monthly_obs_norm, pred_ts=list(clim_ind_ts, pc_ts), reg_eq = reg_eq, monthly_obs=monthly_obs_ts)


### Now need reconstruction step
apr_lagged_recon <- flow_reconstr(recon_model=apr_lagged_fit)
apr_lagged_climind_recon <- flow_reconstr(recon_model=apr_lagged_climind_fit)
apr_all_noshort_recon <- flow_reconstr(recon_model=apr_all_noshort_fit)
apr_all_recon <- flow_reconstr(recon_model=apr_all_fit)


### Save results
write.csv(apr_lagged_recon$ts, "yup.csv")


### Save Results
save_reconstruction(apr_lagged_recon$ts, site_id, site_name, output_name, data_name="rec", method="Percentile Lag Model using Reconstructed MAF")


plot(apr_lagged_recon$ts$flow[4500:4700], type="l")
lines(apr_lagged_climind_recon$ts$flow[4500:4700], col="green")
lines(apr_all_noshort_recon$ts$flow[4500:4700], col="blue")
lines(apr_all_recon$ts$flow[4500:4700], col="red")






monthly_ts$date <-  as.Date(paste0(monthly_ts$year,"-",monthly_ts$month,"-15"))

apr_lagged_climind_df <- apr_lagged_climind_recon$ts
apr_lagged_climind_df$date <- as.Date(paste0(apr_lagged_climind_df$year,"-",apr_lagged_climind_df$month,"-15"))

obs_ts <- data.frame(year=flow_obs$water_year, month=flow_obs$month, flow=flow_obs$monthly_mean)
obs_ts$date <- as.Date(paste0(obs_ts$year,"-",obs_ts$month,"-15"))

p <- ggplot(monthly_ts, aes(x=date, y=flow))
p <- p + geom_line()
p <- p + geom_line(data = obs_ts, color="red")
p <- p + geom_line(data = apr_lagged_climind_df, color="green")
p <- p + theme_classic(8)
p <- p + scale_x_date(limits = c(as.Date("1920-01-01"), as.Date("2010-01-01")))
p

p + scale_x_date(limits = c(as.Date("1920-01-01"), as.Date("1940-01-01")))

p + scale_x_date(limits = c(as.Date("1960-01-01"), as.Date("1980-01-01")))

p + scale_x_date(limits = c(as.Date("1980-01-01"), as.Date("2000-01-01")))




### A fitting step for APR model
#apr_model_fit

x <- paleo.fit(method="ap", reconst_data=annual_rec_region_pred_ts, annual_norm=annual_rec_norm , monthly_norm=monthly_obs_norm)



#, pred_ts=list(clim_ind_ts, pc_ts), reg_eq = "l(flow,seq(-1,1)) + ENSO + PC1")


		### Calculate annual percentile and insert into object
		x$reconst_data$ts <- ap_fit(recon_data = x)
		x$reconst_data$time_scale <- "monthly"
		### Switch back to APR
		x$method <- "apr"
		
		### run the apr fitting
		yup <- apr_fit(recon_data = x)
		
		
		
		### Calculate annual percentile and insert into object
		x$reconst_data$ts <- ap_fit(recon_data = x)
		x$reconst_data$time_scale <- "monthly"
		### run the apr fitting
		yup <- apr_fit(recon_data = x)


flow_obs$month


monthly_fit <- flow_perc_fit(flow_data=data.frame(year=flow_obs$water_year, month=flow_obs$month, flow=flow_obs$monthly_mean), time_scale="monthly", distr="gamma", ref_period=ref_period, save_prefix="just_a_try")





### Create test for years in the reference period
year_test <- flow_recon$age_AD >= ref_period[1] & flow_recon$age_AD <= ref_period[2]

### Process observed annual flows
annual_flows <- flow_recon$flow.obs.m3s[year_test]
annual_fit <- perc_fit(flows=annual_flows, distr=annual_distr, plot_name="observ_annual", write_folder=write_figures_path)
	

### Process reconstructed flows
if (site_id == "10109001") {
### For this site there are two reconstructed time series (flow.rec.local.m3s and flow.rec.region.m3s)

### create column name and row name for analyis and results
col_name <- "flow.rec.local.m3s"
plot_name <- paste0("annual_",gsub("\\.", "_", col_name))
### Create a vector with annual flows from column col_name within reference years
annual_flows <- flow_recon[,c(col_name)][year_test]
### Perform fit on annual flows
annual_fit <- rbind(annual_fit, perc_fit(flows=annual_flows, distr=annual_distr, plot_name=plot_name, write_folder=write_figures_path))

### create column name and row name for analyis and results
col_name <- "flow.rec.region.m3s"
plot_name <- paste0("annual_",gsub("\\.", "_", col_name))
### Create a vector with annual flows from column col_name within reference years
annual_flows <- flow_recon[,c(col_name)][year_test]
### Perform fit on annual flows
annual_fit <- rbind(annual_fit, perc_fit(flows=annual_flows, distr=annual_distr, plot_name=plot_name, write_folder=write_figures_path))


} else {
### For other sites do reconstructed analysis
### create column name and row name for analyis and results
col_name <- "flow.rec.m3s"
plot_name <- paste0("annual_",gsub("\\.", "_", col_name))
### Create a vector with annual flows from column col_name within reference years
annual_flows <- flow_recon[,c(col_name)][year_test]
### Perform fit on annual flows
annual_fit <- rbind(annual_fit, perc_fit(flows=annual_flows, distr=annual_distr, plot_name=plot_name, write_folder=write_figures_path))
}


### Write fit results to CSV file
write_location <- file.path(write_output_path, paste0(site_id,"_param_annual_",annual_distr,".csv"))
write.csv(annual_fit, file = write_location,row.names=FALSE)

}

