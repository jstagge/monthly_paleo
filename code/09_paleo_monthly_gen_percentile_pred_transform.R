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
output_name <- "percentile_pred_model"

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
require(ggplot2)

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

### Number of PCs to consider as predictors
pc_pred <- 8
### Lags to consider
lags_pred <- seq(-3,2)

###########################################################################
## Read in reconstructed flows, tree rings and climate indices
###########################################################################
### Read in PC scores
pc_score_impute <- read.csv(file.path(file.path(output_path,"pca_chronol"), "PC_Score_impute.csv"))

### Read in climate indices
clim_ind <- read.csv(file.path(file.path(data_path,"paleo_clim_ind"), "clim_ind.csv"))
### Remove ENSO_var, this is simply a running variance calculation - not useful as a predictor
clim_ind <- subset(clim_ind, select=-c(ENSO_var))

################################################
### Create matrix of potential predictor values
#################################################
## Cut PCs to only number in initial settings
pc_cut <- seq(1,pc_pred+1)
pc_score_impute <- pc_score_impute[,pc_cut]

### Create concurrent predictors (same calendar year), with climate indices and first 8 PCs
pred_clim_pca_impute_concur <- merge(clim_ind, pc_score_impute, by.x="Year", by.y="X", all=TRUE)

### Lag the predictors
pred_clim_pca_impute_lag <- shift_indices_df(pred_clim_pca_impute_concur, lags=lags_pred)

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
percentile_path <- file.path(file.path(output_path, "paleo_monthly_gen"), "percentile_model")

monthly_param <- list(param=read.csv(file.path(percentile_path, paste0(site_id,"_param_month_",monthly_distr,".csv"))), distr=monthly_distr)
monthly_param$param <- monthly_param$param[,c(2,3)]

annual_param <- list(param=read.csv(file.path(percentile_path, paste0(site_id,"_param_annual_",annual_distr,".csv"))), distr=annual_distr)
annual_param$param <- annual_param$param[,seq(1,3)]


################################################
### Set up model combination matrix
#################################################
pred_list <- c("clim_only","clim_only_lag","clim_pca_impute_concur", "clim_pca_impute_lag")

if (site_id == "10109001") {
	data_list <- c("observ_annual", "rec_local_m3s", "rec_region_m3s")
} else {
	data_list <- c("observ_annual", "rec_m3s")
}

run_combinations <- expand.grid(predictors=pred_list, data=data_list)	

### Set up reconstruction input
run_combinations$rec_data <- NA
run_combinations$rec_data[run_combinations$data=="observ_annual"] <- "flow_obs"
run_combinations$rec_data[run_combinations$data!="observ_annual"] <- "flow_recon"

### Set up reconstruction input
run_combinations$pred_frame <- NA
run_combinations$pred_frame[!grepl("lag", run_combinations$predictors)] <- "pred_clim_pca_impute_concur"
run_combinations$pred_frame[grepl("lag", run_combinations$predictors)] <- "pred_clim_pca_impute_lag"


model_name_list <- rep("Percentile Predictor", dim(run_combinations)[1])
model_name_list[grepl("only", run_combinations$predictors)] <- paste0(model_name_list[grepl("only", run_combinations$predictors)], " Climate Only Model")

model_name_list[grepl("pca", run_combinations$predictors)] <- paste0(model_name_list[grepl("pca", run_combinations$predictors)], " Full Model")

model_name_list[grepl("lag", run_combinations$predictors)] <- paste0(model_name_list[grepl("lag", run_combinations$predictors)], " with Lags")

model_name_list[run_combinations$data == "observ_annual"] <- paste0(model_name_list[run_combinations$data == "observ_annual"], " using True Annual Mean")
model_name_list[run_combinations$data == "rec_m3s"] <- paste0(model_name_list[run_combinations$data == "rec_m3s"], " using Reconstructed MAF")
model_name_list[run_combinations$data == "rec_local_m3s"] <- paste0(model_name_list[run_combinations$data == "rec_local_m3s"], " using Local Reconstructed MAF")
model_name_list[run_combinations$data == "rec_region_m3s"] <- paste0(model_name_list[run_combinations$data == "rec_region_m3s"], " using Regional Reconstructed MAF")


### Loop through all combinations, fitting, and saving
for (c in seq(1,dim(run_combinations)[1])){

pred_c <- run_combinations$predictors[c]
data_c <- run_combinations$data[c]
recon_name_c <- run_combinations$rec_data[c]
pred_frame_c <- run_combinations$pred_frame[c]

data_name <- as.character(data_c)

### Create dataframes for annual flows
recon_c <- get(recon_name_c)

if (recon_name_c == "flow_obs") {
annual_data <- data.frame(water_year=recon_c$water_year, annual_flow=recon_c$annual_mean)
} else {
recon_column_name <- gsub("_",".",paste0("flow_",data_c))
recon_column_number <- match(recon_column_name, colnames(recon_c))
annual_data <- data.frame(water_year=recon_c$age_AD, annual_flow=recon_c[, recon_column_number])
}

### Only use one observation per year
annual_data <- unique(annual_data)

### Read in standard normal model
model_path <- file.path(file.path(output_path, "paleo_monthly_gen"), "percentile_pred_model")
norm_model <- read.csv(file.path(model_path, paste0(site_id,"_percentile_pred_model_coef_",data_name,"_",pred_c,".csv")),  row.names = 1)

### Run Percentile Model
annual_name <- data_name
if (annual_name != "observ_annual") {annual_name <- paste0("annual_flow_",annual_name)}
month_ts <- perc_model_pred(annual_rec=annual_data, predictors = get(pred_frame_c), monthly_param=monthly_param, annual_param=annual_param, norm_model=norm_model, first_month_wy=first_month_wy, data_name=annual_name)

### Save Results
save_reconstruction(month_ts, site_id, site_name, paste0(output_name, "_",pred_c), data_name=sub('_m3s', '', data_c), method=model_name_list[c])
	
}

}



