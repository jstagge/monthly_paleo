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

write_output_path <- file.path(output_path, "gof")
write_figures_path <- file.path(output_path,"figures/gof")

###########################################################################
###  Load functions
###########################################################################
### Load these functions for all code
require(colorout)
require(assertthat)
require(reshape2)

### Load these functions for this unique project
require(ggplot2)
require(svglite)

### Load project specific functions
#source(file.path(function_path,"gof_calcs.R"))
#source(file.path(function_path,"null_model.R"))
#source(file.path(function_path,"perc_fit.R"))
file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
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
file.sources = list.files(global_path, pattern="*.R", recursive=TRUE)
sapply(file.path(global_path, file.sources),source)

###########################################################################
## Set Initial Values
###########################################################################
### Set site data
site_id_list <- c("10109001", "10011500")
site_name_list <- c("Logan Utah", "Bear River near Utah-Wyo")
recons_file_name_list <- c("logan2013flow.txt", "bear2015flow.txt")

param_cd <- "00060"

###  This reference period is used for comparison purposes
ref_period_wy <- c(1925,2005)

###########################################################################
###  Set up a loop to run through all site_ides and Transform based on the Percentile Model
###########################################################################
for (n in seq(1,length(site_id_list))) {

site_id <- site_id_list[n]
site_name <- site_name_list[n]
recons_file_name <- recons_file_name_list[n]

###########################################################################
###  Read in Observed Flow Data
###########################################################################

### Read in observed flow and fix data type
obs_file_name <- paste0(site_id,"_",param_cd,"_mon_wy.csv")
flow_obs <- read.csv(file.path(output_path,paste0("observed_utah_flow/",obs_file_name)))
flow_obs$date <- as.Date(flow_obs$date)  
#head(flow_obs) # Review data frame

###########################################################################
### Prepare to loop through all possible model combinations
###########################################################################
### List all models
model_list <- c("null_model", "percentile_model", "percentile_lag_model", "percentile_pred_model_clim_only", "percentile_pred_model_clim_only_lag", "percentile_pred_model_clim_pca_impute_concur", "percentile_pred_model_clim_pca_impute_lag")
### List all input (observed or reconstructed)
if (site_id == "10109001") {
	input_list <- c("observ_annual", "rec_local", "rec_region")
} else {
	input_list <- c("observ_annual", "rec")
}
### Create a matrix with all possible model combinations
runs_list <- expand.grid(model_list, input_list)
### Reorder so that models are grouped together
runs_list <- runs_list[order(runs_list$Var1),]


###########################################################################
### Loop through each model combination and calculate GOF properties
###########################################################################
for (i in seq(1,dim(runs_list)[1])) {

### Extract the model and input names for this instance
Var1 <- as.character(runs_list$Var1[i])
Var2 <- as.character(runs_list$Var2[i])
var_folder <- sub("\\model.*", "model", Var1)

### Read in the reconstructed time series
reconst_location <- file.path(file.path(output_path,"paleo_monthly_gen"), paste0(var_folder, "/",site_id,"_",Var1, "_month_ts_",Var2,".csv"))
reconst_ts <- read.csv(reconst_location)

### Merge the reconstructed time series with the observed flows
reconst_ts <- merge(reconst_ts, flow_obs[,c("year", "month", "monthly_mean", "annual_mean")], by=c("year", "month"))
### Convert date field from character to date
reconst_ts$date <- as.Date(reconst_ts$date)
### Resort to proper date order
reconst_ts <- reconst_ts[order(reconst_ts$date),] 

### Create a list of all valid ref period years and cut data to this list
ref_period_list <- seq(ref_period_wy[1], ref_period_wy[2])
reconst_ts <- reconst_ts[reconst_ts$water_year %in% ref_period_list, ]
### Check if the number of water years is the same as the reference period
#assert_that(length(unique(reconst_ts$water_year)) == length(ref_period_list))

###########################################################################
### Calculate goodness of fit statistics for annual fit
###########################################################################
### Calculate gof statistics
gof_annual_temp<- gof_ts(pred=reconst_ts$flow_rec_m3s, obs=reconst_ts$monthly_mean)
### Add method and input data
gof_annual_temp[["method"]] <- as.character(reconst_ts$method[1])
gof_annual_temp[["model"]] <- Var1
gof_annual_temp[["data"]] <- Var2

### Save gof to a combined dataframe
if (i == 1) {
	gof_annual <- gof_annual_temp
} else {
	gof_annual <- rbind(gof_annual, gof_annual_temp)
}

### Plot timeseries against observed
write_file <- paste0(site_id, "_",Var1,"_",Var2,"_ts")
plot_result <- ts_plot(data=reconst_ts, write_folder= write_figures_path, write_file=write_file)

### Create residual plots
plot_result <- gof_plot_wrapper(x.obs=reconst_ts$monthly_mean, x.pred=reconst_ts$flow_rec_m3s, x.factor=reconst_ts$month, write_folder= write_figures_path, write_file=write_file)

### Combine monthly gof results
gof_month_temp <- data.frame(t(plot_result$gof))
gof_month_temp <- data.frame(Month=rownames(gof_month_temp), gof_month_temp, method=reconst_ts$method[1], model=Var1, data=Var2)
if (i == 1) {
	gof_month_stats <- gof_month_temp
} else {
	gof_month_stats <- rbind(gof_month_stats, gof_month_temp)
}


}

gof_annual <- data.frame(gof_annual)
### Split, convert variable formats, recombine
gof_annual_stats <- data.frame(lapply(gof_annual[,seq(1,6)], as.numeric, stringsAsFactors=FALSE))
gof_annual_names <- data.frame(lapply(gof_annual[,seq(7,9)], as.character, stringsAsFactors=FALSE))
gof_annual <- cbind(gof_annual_names, gof_annual_stats)

### Save to csv
write_location <- file.path(write_output_path, paste0(site_id,"_annual_gof.csv"))
write.csv(gof_annual, file = write_location, row.names=FALSE)

### Save to csv
write_location <- file.path(write_output_path, paste0(site_id,"_month_gof.csv"))
write.csv(gof_month_stats, file = write_location, row.names=FALSE)

}




###########################################################################
## Read in monthly GOF and plot
###########################################################################
for (n in seq(1,length(site_id_list))) {

site_id <- site_id_list[n]
site_name <- site_name_list[n]
recons_file_name <- recons_file_name_list[n]

### Read in monthly goodness of fit statistics
read_location <- file.path(write_output_path, paste0(site_id,"_month_gof.csv"))
month_gof <- read.csv(read_location)

### Re-factor months to water year
month_gof$Month <- factor(month_gof$Month, levels=c(seq(10,12), seq(1,9)))

### subset to remove lags
month_gof <- month_gof[month_gof$model %in% c("null_model","percentile_model","percentile_lag_model", "percentile_pred_model_clim_only", "percentile_pred_model_clim_pca_impute_concur"),]

### Re-factor models to rename levels
month_gof$model <- factor(month_gof$model, levels=c("null_model","percentile_model","percentile_lag_model", "percentile_pred_model_clim_only", "percentile_pred_model_clim_only_lag", "percentile_pred_model_clim_pca_impute_concur", "percentile_pred_model_clim_pca_impute_lag"), labels=c("MF Model", "AP Model", "APR Model with Lags", "APR Climate Only Model", "APR Climate Only Model\nwith Lags", "APR Full Model", "APR Full Model\nwith Lags"))


### Re-factor data inputs to rename levels
if (site_id == "10109001") {
	month_gof$data_name <- factor(month_gof$data, labels=c("True Mean \nAnnual Flow","Local \nReconstructed \nMAF","Regional \nReconstructed \nMAF"))
} else {
	month_gof$data_name <- factor(month_gof$data, labels=c("True Mean \nAnnual Flow","Reconstructed\n MAF"))
}

### Create location to save plots
write_folder <- file.path(write_figures_path,"gof_by_month/")

### Set up loop over data
data_list <- as.character(unique(month_gof$data))

for (q in seq(1,length(data_list))) {

### Extract the data for plotting
data_q <- data_list[q]
month_gof_q <- subset(month_gof, data==data_q)

### Mean Error Plot
param_list <- list(x="Month", y = "ME", group="method", color="model")
param_names <- list(x="Month", y = expression(bold(paste("Mean Error  ( ",m^3,"/s )"))), color="Model")
p <- gof_month_plot2(data=month_gof_q, param_list=param_list, param_names=param_names, y_hor_line=c(0))
### Save plot
ggsave(paste0(write_folder,"/png/", site_id, "_ME_by_month_",data_q,".png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_folder,"/pdf/", site_id, "_ME_by_month_",data_q,".pdf"), p, width=5, height=4)
ggsave(paste0(write_folder,"/svg/", site_id, "_ME_by_month_",data_q,".svg"), p, width=5, height=4)

### Mean Absolute Error Plot
param_list$y <- "MAE"
param_names$y <- expression(bold(paste("Mean Absolute Error  ( ",m^3,"/s )")))
p <- gof_month_plot2(data=month_gof_q, param_list=param_list, param_names=param_names, y_hor_line=NULL)
### Save plot
ggsave(paste0(write_folder,"/png/", site_id, "_MAE_by_month_",data_q,".png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_folder,"/pdf/", site_id, "_MAE_by_month_",data_q,".pdf"), p, width=5, height=4)
ggsave(paste0(write_folder,"/svg/", site_id, "_MAE_by_month_",data_q,".svg"), p, width=5, height=4)

### Pearson Correlation Plot
param_list$y <- "R"
param_names$y <- "Pearson Correlation (R)"
p <- gof_month_plot2(data=month_gof_q, param_list=param_list, param_names=param_names, y_hor_line=c(0,1))
### Save plot
ggsave(paste0(write_folder,"/png/", site_id, "_R_by_month_",data_q,".png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_folder,"/pdf/", site_id, "_R_by_month_",data_q,".pdf"), p, width=5, height=4)
ggsave(paste0(write_folder,"/svg/", site_id, "_R_by_month_",data_q,".svg"), p, width=5, height=4)

### Spearman's Rho Plot
param_list$y <- "R.spear"
param_names$y <- expression(bold(paste("Spearman's Rho  ( ",rho," )")))
p <- gof_month_plot2(data=month_gof_q, param_list=param_list, param_names=param_names, y_hor_line=c(1))
### Save plot
ggsave(paste0(write_folder,"/png/", site_id, "_Rspear_by_month_",data_q,".png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_folder,"/pdf/", site_id, "_Rspear_by_month_",data_q,".pdf"), p, width=5, height=4)
ggsave(paste0(write_folder,"/svg/", site_id, "_Rspear_by_month_",data_q,".svg"), p, width=5, height=4)

### RMSE
param_list$y <- "RMSE"
param_names$y <- expression(bold(paste("RMSE  ( ",m^3,"/s )")))
p <- gof_month_plot2(data=month_gof_q, param_list=param_list, param_names=param_names, y_hor_line=NULL)
### Save plot
ggsave(paste0(write_folder,"/png/", site_id, "_RMSE_by_month_",data_q,".png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_folder,"/pdf/", site_id, "_RMSE_by_month_",data_q,".pdf"), p, width=5, height=4)
ggsave(paste0(write_folder,"/svg/", site_id, "_RMSE_by_month_",data_q,".svg"), p, width=5, height=4)

### Nash Sutcliffe Efficiency Plot
param_list$y <- "NSE"
param_names$y <- "Nash-Sutcliffe Efficiency"
p <- gof_month_plot2(data=month_gof_q, param_list=param_list, param_names=param_names, y_hor_line=c(0,1))
p <- p + coord_cartesian(ylim=c(-1,1))
### Save plot
ggsave(paste0(write_folder,"/png/", site_id, "_NSE_by_month_",data_q,".png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_folder,"/pdf/", site_id, "_NSE_by_month_",data_q,".pdf"), p, width=5, height=4)
ggsave(paste0(write_folder,"/svg/", site_id, "_NSE_by_month_",data_q,".svg"), p, width=5, height=4)


}


}





###########################################################################
###  Set up a loop to run through all site_ides and Transform based on the Percentile Model
###########################################################################
for (n in seq(1,length(site_id_list))) {

site_id <- site_id_list[n]
site_name <- site_name_list[n]
recons_file_name <- recons_file_name_list[n]

###########################################################################
### Prepare to loop through all possible model combinations
###########################################################################
### List all models
model_list <- c("null_model", "percentile_model", "percentile_lag_model", "percentile_pred_model")
### List all input (observed or reconstructed)
if (site_id == "10109001") {
	input_list <- c("rec_local", "rec_region")
} else {
	input_list <- c("rec")
}
### Create a matrix with all possible model combinations
runs_list <- expand.grid(model_list, input_list)
### Reorder so that models are grouped together
runs_list <- runs_list[order(runs_list$Var1),]


###########################################################################
### Loop through each model combination and calculate GOF properties
###########################################################################
for (i in seq(1,dim(runs_list)[1])) {

### Extract the model and input names for this instance
Var1 <- as.character(runs_list$Var1[i])
Var2 <- as.character(runs_list$Var2[i])

### Read in the reconstructed time series
reconst_location <- file.path(file.path(output_path,"paleo_monthly_gen"), paste0(Var1, "/",site_id,"_",Var1, "_month_ts_",Var2,".csv"))
reconst_ts <- read.csv(reconst_location)

### Convert date field from character to date
reconst_ts$date <- as.Date(reconst_ts$date)
### Resort to proper date order
reconst_ts <- reconst_ts[order(reconst_ts$date),] 

### Remove NAs
reconst_ts <- reconst_ts[complete.cases(reconst_ts),]

### Set file name
write_file <- paste0(site_id, "_",Var1,"_",Var2,"_ts")

### Create plot
p <- ggplot(reconst_ts, aes(x=date, y=flow_rec_m3s))
p <- p + geom_line(size=0.2)
p <- p + scale_x_date(name="Date", breaks=seq(as.Date("800-01-01"), as.Date("2030-01-01"), by="25 years"), date_labels = "%Y")
p <- p + scale_y_continuous(name=expression(bold(paste("Monthly Mean Discharge  ( ",m^3,"/s )"))))
p <- p + theme_classic_correct()
p

### Save full plot
ggsave(paste0(file.path(write_folder,"ts_long/png/"), write_file, "_long.png"), p, width=8, height=4, dpi=600)
ggsave(paste0(file.path(write_folder,"ts_long/pdf/"), write_file, "_long.pdf"), p, width=8, height=4)
ggsave(paste0(file.path(write_folder,"ts_long/svg/"), write_file, "_long.svg"), p, width=8, height=4)

}}




###########################################################################
## Create AGU figure
###########################################################################
n <- 1

site_id <- site_id_list[n]
site_name <- site_name_list[n]
recons_file_name <- recons_file_name_list[n]

date_df <- expand.grid(year=seq(1400,2016), month=seq(1,12))
date_df$water_year <- usgs_wateryear(year=date_df$year, month=date_df$month)

### Read in reconst flows (use fread because of large header)
recons_file_name <- "logan2013flow.txt"
flow_recon <- read_table_wheaders(file.path(data_path,paste0("paleo_derose_annual_recon/",recons_file_name)), sep="\t", na.string="-9999")

flow_merge <- merge(date_df, flow_recon, by.x="water_year", by.y="age_AD", all.x=TRUE)
flow_merge$date <- as.Date(paste0(flow_merge$year, "-", flow_merge$month, "-01"))


### Read in observed flow and fix data type
obs_file_name <- paste0(site_id,"_",param_cd,"_mon_wy.csv")
flow_obs <- read.csv(file.path(output_path,paste0("observed_utah_flow/",obs_file_name)))
flow_obs$date <- as.Date(flow_obs$date)  
#head(flow_obs) # Review data frame


### Read in the reconstructed time series
reconst_location <- file.path(file.path(output_path,"paleo_monthly_gen"), "percentile_pred_model/10109001_percentile_pred_model_clim_pca_impute_concur_month_ts_rec_region.csv")
reconst_ts <- read.csv(reconst_location)

### Convert date field from character to date
reconst_ts$date <- as.Date(reconst_ts$date)
### Resort to proper date order
reconst_ts <- reconst_ts[order(reconst_ts$date),] 

### Remove NAs
reconst_ts <- reconst_ts[complete.cases(reconst_ts),]

### Set file name
write_file <- "10109001_percentile_pred_model_clim_pca_impute_concur_ts"

### Create plot
p <- ggplot(reconst_ts, aes(x=date, y=flow_rec_m3s))
p <- p + geom_line(data=flow_obs, aes(y=monthly_mean), size=0.25, colour="red")
p <- p + geom_line(size=0.25, colour="black")
p <- p + geom_line(data=flow_merge, aes(y=flow.rec.region.m3s), size=0.25, colour="blue", linetype="longdash", alpha=0.4)
p <- p + scale_x_date(name="Date", breaks=seq(as.Date("800-01-01"), as.Date("2030-01-01"), by="5 years"), date_labels = "%Y")
p <- p + scale_y_continuous(name=expression(bold(paste("Monthly Mean Discharge  ( ",m^3,"/s )"))))
p <- p + theme_classic_correct()
p <- p + coord_cartesian(xlim=c(as.Date("1670-01-01"), as.Date("1990-01-01")))
p


### Save full plot
#ggsave(paste0(file.path(write_figures_path,"ts_long/png/"), write_file, "_agu.png"), p, width=68, height=6, dpi=600, limitsize=FALSE)
ggsave(paste0(file.path(write_figures_path,"ts_long/pdf/"), write_file, "_agu.pdf"), p, width=70, height=5, limitsize=FALSE)
ggsave(paste0(file.path(write_figures_path,"ts_long/svg/"), write_file, "_agu.svg"), p, width=70, height=5, limitsize=FALSE)








###########################################################################
## Create 1730s figure
###########################################################################
n <- 1

site_id <- site_id_list[n]
site_name <- site_name_list[n]
recons_file_name <- recons_file_name_list[n]

date_df <- expand.grid(year=seq(1400,2016), month=seq(1,12))
date_df$water_year <- usgs_wateryear(year=date_df$year, month=date_df$month)

### Read in reconst flows (use fread because of large header)
recons_file_name <- "logan2013flow.txt"
flow_recon <- read_table_wheaders(file.path(data_path,paste0("paleo_derose_annual_recon/",recons_file_name)), sep="\t", na.string="-9999")

flow_merge <- merge(date_df, flow_recon, by.x="water_year", by.y="age_AD", all.x=TRUE)
flow_merge$date <- as.Date(paste0(flow_merge$year, "-", flow_merge$month, "-01"))


### Read in observed flow and fix data type
obs_file_name <- paste0(site_id,"_",param_cd,"_mon_wy.csv")
flow_obs <- read.csv(file.path(output_path,paste0("observed_utah_flow/",obs_file_name)))
flow_obs$date <- as.Date(flow_obs$date)  
#head(flow_obs) # Review data frame


### Read in the reconstructed time series
reconst_location <- file.path(file.path(output_path,"paleo_monthly_gen"), "percentile_pred_model/10109001_percentile_pred_model_clim_pca_impute_concur_month_ts_rec_region.csv")
reconst_ts <- read.csv(reconst_location)

### Convert date field from character to date
reconst_ts$date <- as.Date(reconst_ts$date)
### Resort to proper date order
reconst_ts <- reconst_ts[order(reconst_ts$date),] 

### Remove NAs
reconst_ts <- reconst_ts[complete.cases(reconst_ts),]

### Set file name
write_file <- "10109001_percentile_pred_model_clim_pca_impute_concur_ts"

### Create plot
p <- ggplot(reconst_ts, aes(x=date, y=flow_rec_m3s))
p <- p + geom_line(data=flow_obs, aes(y=monthly_mean), size=0.25, colour="black")
p <- p + geom_line(size=0.25, colour="red")
p <- p + geom_line(data=flow_merge, aes(y=flow.rec.region.m3s), size=0.35, colour="blue", linetype="longdash")
p <- p + scale_x_date(name="Date", breaks=seq(as.Date("800-01-01"), as.Date("2030-01-01"), by="5 years"), date_labels = "%Y")
p <- p + scale_y_continuous(name=expression(bold(paste("Monthly Mean Discharge  ( ",m^3,"/s )"))))
p <- p + theme_classic_correct()
p <- p + coord_cartesian(xlim=c(as.Date("1715-01-01"), as.Date("1745-01-01")))
p


### Save full plot
#ggsave(paste0(file.path(write_figures_path,"ts_long/png/"), write_file, "_agu.png"), p, width=68, height=6, dpi=600, limitsize=FALSE)
ggsave(paste0(file.path(write_figures_path,"ts_long/pdf/"), write_file, "_1730s.pdf"), p, width=6, height=4, limitsize=FALSE)
ggsave(paste0(file.path(write_figures_path,"ts_long/svg/"), write_file, "_1730s.svg"), p, width=6, height=4, limitsize=FALSE)








###########################################################################
## Create 1970s figure
###########################################################################
n <- 1

site_id <- site_id_list[n]
site_name <- site_name_list[n]
recons_file_name <- recons_file_name_list[n]

date_df <- expand.grid(year=seq(1400,2016), month=seq(1,12))
date_df$water_year <- usgs_wateryear(year=date_df$year, month=date_df$month)

### Read in reconst flows (use fread because of large header)
recons_file_name <- "logan2013flow.txt"
flow_recon <- read_table_wheaders(file.path(data_path,paste0("paleo_derose_annual_recon/",recons_file_name)), sep="\t", na.string="-9999")

flow_merge <- merge(date_df, flow_recon, by.x="water_year", by.y="age_AD", all.x=TRUE)
flow_merge$date <- as.Date(paste0(flow_merge$year, "-", flow_merge$month, "-01"))


### Read in observed flow and fix data type
obs_file_name <- paste0(site_id,"_",param_cd,"_mon_wy.csv")
flow_obs <- read.csv(file.path(output_path,paste0("observed_utah_flow/",obs_file_name)))
flow_obs$date <- as.Date(flow_obs$date)  
#head(flow_obs) # Review data frame


### Read in the reconstructed time series
#reconst_file_list <- c("null_model","percentile_model","percentile_lag_model","percentile_pred_model_clim_pca_impute_concur")
#reconst_folder_list <- c("null_model","percentile_model","percentile_lag_model","percentile_pred_model")

reconst_file_list <- c("percentile_model","percentile_pred_model_clim_pca_impute_concur")
reconst_folder_list <- c("percentile_model","percentile_pred_model")


### Loop to read in all files
for (j in seq(1,length(reconst_file_list))) {

reconst_name <- reconst_file_list[j]
reconst_folder <- reconst_folder_list[j]

reconst_location <- file.path(file.path(output_path,"paleo_monthly_gen"), paste0(reconst_folder,"/10109001_",reconst_name,"_month_ts_rec_region.csv"))
reconst_ts_temp <- read.csv(reconst_location)

### Convert date field from character to date
reconst_ts_temp$date <- as.Date(reconst_ts_temp$date)
### Resort to proper date order
reconst_ts_temp <- reconst_ts_temp[order(reconst_ts_temp$date),] 

### Remove NAs
reconst_ts_temp <- reconst_ts_temp[complete.cases(reconst_ts_temp),]

### Select only some columns
reconst_ts_temp <- reconst_ts_temp[ , c("site_id","site_name", "date", "flow_rec_m3s", "method")]
reconst_ts_temp$method_short <- reconst_name

### Add results to bottom of data frame
if(j==1) {reconst_ts <- reconst_ts_temp} else {reconst_ts <- rbind(reconst_ts, reconst_ts_temp)}

}

### Set file name
write_file <- "10109001_percentile_pred_model_clim_pca_impute_concur_ts"

### Create plot
p <- ggplot(reconst_ts, aes(x=date, y=flow_rec_m3s))
p <- p + geom_line(size=0.25, aes(colour=method_short, group=method_short))
p <- p + geom_line(data=flow_merge, aes(y=flow.rec.region.m3s), size=0.25, colour="blue", linetype="longdash", alpha=0.8)
p <- p + geom_line(data=flow_obs, aes(y=monthly_mean), size=0.25, colour="black",  alpha=0.65)

p <- p + scale_x_date(name="Date", breaks=seq(as.Date("800-01-01"), as.Date("2030-01-01"), by="2 years"), date_labels = "%Y")
p <- p + scale_y_continuous(name=expression(bold(paste("Monthly Mean Discharge  ( ",m^3,"/s )"))))
p <- p + scale_colour_manual(name= NULL, values = c("#4daf4a", "red"))
p <- p + theme_classic_correct()
p <- p + theme(legend.position="none")
p <- p + coord_cartesian(xlim=c(as.Date("1960-01-01"), as.Date("1975-01-01")))
p


### Save full plot
#ggsave(paste0(file.path(write_figures_path,"ts_long/png/"), write_file, "_agu.png"), p, width=68, height=6, dpi=600, limitsize=FALSE)
ggsave(paste0(file.path(write_figures_path,"ts_long/pdf/"), write_file, "_1970s.pdf"), p, width=6, height=4, limitsize=FALSE)
ggsave(paste0(file.path(write_figures_path,"ts_long/svg/"), write_file, "_1970s.svg"), p, width=6, height=4, limitsize=FALSE)





###########################################################################
## Read in monthly GOF and plot
###########################################################################
for (n in seq(1,length(site_id_list))) {

site_id <- site_id_list[n]
site_name <- site_name_list[n]
recons_file_name <- recons_file_name_list[n]

### Read in monthly goodness of fit statistics
read_location <- file.path(write_output_path, paste0(site_id,"_annual_gof.csv"))
annual_gof <- read.csv(read_location)

### Extract without lags
#annual_gof_nolag <- annual_gof[!grepl("lag", annual_gof$model),]
model_test <- unique(annual_gof$model)[c(1,2,3,4,6)]
annual_gof_nolag <- annual_gof[annual_gof$model %in% model_test,]


### Re-factor models to rename levels
annual_gof_nolag$model <- factor(annual_gof_nolag$model, levels=c("null_model","percentile_model","percentile_lag_model", "percentile_pred_model_clim_only", "percentile_pred_model_clim_only_lag", "percentile_pred_model_clim_pca_impute_concur", "percentile_pred_model_clim_pca_impute_lag"), labels=c("MF Model", "AP Model", "APR Model \n with Lags", "APR Climate \n Only Model", "APR Climate Only Model\nwith Lags", "APR Full \n Model", "APR Full Model\nwith Lags"))

### Re-factor data inputs to rename levels
if (site_id == "10109001") {
	annual_gof_nolag$data <- factor(annual_gof_nolag$data, labels=c("True Mean \nAnnual Flow","Local Reconstructed  \nMAF","Regional Reconstructed  \nMAF"))
} else {
	annual_gof_nolag$data <- factor(annual_gof_nolag$data, labels=c("True Mean \nAnnual Flow","Reconstructed\n MAF"))
}


### Mean Error Plot
param_list <- list(x="model", y = "ME", group="data", color="data")
param_names <- list(x="\nModel", y = expression(bold(paste("Mean Error  ( ",m^3,"/s )"))), color="Data")
p <- gof_annual_plot(data=annual_gof_nolag, param_list=param_list, param_names=param_names, y_hor_line=c(0))
### Save plot
ggsave(paste0(write_folder,"/png/", site_id, "_ME_annual.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_folder,"/pdf/", site_id, "_ME_annual.pdf"), p, width=5, height=4)
ggsave(paste0(write_folder,"/svg/", site_id, "_ME_annual.svg"), p, width=5, height=4)

### Mean Absolute Error Plot
param_list$y <- "MAE"
param_names$y <- expression(bold(paste("Mean Absolute Error  ( ",m^3,"/s )")))
p <- gof_annual_plot(data=annual_gof_nolag, param_list=param_list, param_names=param_names, y_hor_line=NULL)
### Save plot
ggsave(paste0(write_folder,"/png/", site_id, "_MAE_annual.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_folder,"/pdf/", site_id, "_MAE_annual.pdf"), p, width=5, height=4)
ggsave(paste0(write_folder,"/svg/", site_id, "_MAE_annual.svg"), p, width=5, height=4)

### Pearson Correlation Plot
param_list$y <- "R"
param_names$y <- "Pearson Correlation (R)"
p <- gof_annual_plot(data=annual_gof_nolag, param_list=param_list, param_names=param_names, y_hor_line=c(0,1))
### Save plot
ggsave(paste0(write_folder,"/png/", site_id, "_R_annual.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_folder,"/pdf/", site_id, "_R_annual.pdf"), p, width=5, height=4)
ggsave(paste0(write_folder,"/svg/", site_id, "_R_annual.svg"), p, width=5, height=4)

### Spearman's Rho Plot
param_list$y <- "R.spear"
param_names$y <- expression(bold(paste("Spearman's Rho  ( ",rho," )")))
p <- gof_annual_plot(data=annual_gof_nolag, param_list=param_list, param_names=param_names, y_hor_line=c(1))
### Save plot
ggsave(paste0(write_folder,"/png/", site_id, "_Rspear_annual.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_folder,"/pdf/", site_id, "_Rspear_annual.pdf"), p, width=5, height=4)
ggsave(paste0(write_folder,"/svg/", site_id, "_Rspear_annual.svg"), p, width=5, height=4)

### RMSE
param_list$y <- "RMSE"
param_names$y <- expression(bold(paste("RMSE  ( ",m^3,"/s )")))
p <- gof_annual_plot(data=annual_gof_nolag, param_list=param_list, param_names=param_names, y_hor_line=NULL)
### Save plot
ggsave(paste0(write_folder,"/png/", site_id, "_RMSE_annual.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_folder,"/pdf/", site_id, "_RMSE_annual.pdf"), p, width=5, height=4)
ggsave(paste0(write_folder,"/svg/", site_id, "_RMSE_annual.svg"), p, width=5, height=4)

### Nash Sutcliffe Efficiency Plot
param_list$y <- "NSE"
param_names$y <- "Nash-Sutcliffe Efficiency"
p <- gof_annual_plot(data=annual_gof_nolag, param_list=param_list, param_names=param_names, y_hor_line=c(0,1))
#p <- p + coord_cartesian(ylim=c(-1,1))
### Save plot
ggsave(paste0(write_folder,"/png/", site_id, "_NSE_annual.png"), p, width=5, height=4, dpi=600)
ggsave(paste0(write_folder,"/pdf/", site_id, "_NSE_annual.pdf"), p, width=5, height=4)
ggsave(paste0(write_folder,"/svg/", site_id, "_NSE_annual.svg"), p, width=5, height=4)

}



