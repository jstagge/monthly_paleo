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

output_name <- "null_model"
"paleo_monthly_gen"
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
### Calculate Monthly Proportions for Null Model
#################################################
null_dt <- data.table(flow_obs)

### Only consider those water years with 12 months of data, so first sum months with flows greater than 0.  Negative flows will confuse the proportion method.
months_by_wy <- null_dt[,list(count_months=sum(monthly_mean>0, na.rm=TRUE)), by=water_year]
	
### Determine full water years and subset null_dt to only full years 
full_wys <- months_by_wy$water_year[months_by_wy$count_months==12]
null_dt <- null_dt[null_dt$water_year %in% full_wys,]

### Calculate monthly proportion 
null_dt$prop_month <- null_dt$monthly_sum/null_dt$annual_sum

### Summarize null_dt by calculating the mean and median proportion across all years
null_prop <- null_dt[,list(prop_mean=mean(prop_month, na.rm=TRUE),prop_median=median(prop_month, na.rm=TRUE)), by=month] 

### Rescale so that months always add up to 1
null_prop$prop_mean <- null_prop$prop_mean/sum(null_prop$prop_mean)
null_prop$prop_median <- null_prop$prop_median/sum(null_prop$prop_median)
null_prop <- data.frame(null_prop)

### Write to csv file
null_prop_write_location <- file.path(write_output_path, paste0(site_id,"_",output_name,"_prop_monthly.csv"))
write.csv(null_prop, file = null_prop_write_location,row.names=FALSE)



#################################################
### Prepare to apply the Null Model
#################################################
### Create monthly proportion table
### For all further analyses, we will use the monthly mean rather than
### the median.  Produces no noticeable difference.
monthly_prop=data.frame(month=null_prop$month, prop=null_prop$prop_mean)



################################################
### Plot proportions
#################################################
### Create plot dataframes
null_dt_plot <- null_dt
null_dt_plot$month <- factor(null_dt_plot$month, levels=c(seq(10,12),seq(1,9)))
null_prop_plot <- null_prop
null_prop_plot$group <- "group"
null_prop_plot$month <- factor(null_prop_plot$month, levels=c(seq(10,12),seq(1,9)))

### Create plot
p <- ggplot(null_dt_plot, aes(x=month))
p <- p + geom_line(aes(y=prop_month, group=water_year), colour="grey", size=0.15)
p <- p + geom_line(data=null_prop_plot, aes(y=prop_mean, group=group), colour="black", size=0.7)
p <- p + scale_x_discrete(expand= c(0,0), name="Month")
#p <- p + scale_y_continuous(name="Monthly Flow Proportion (%)\n", breaks=seq(0,100,5))
p <- p + scale_y_continuous(name="Monthly Flow Proportion\n", labels = scales::percent, breaks=seq(0,1,0.05))
p <- p + coord_cartesian(ylim=c(0,0.52), expand=FALSE)
#p <- p + coord_cartesian(ylim=c(0,52), expand=FALSE)
p <- p + theme_classic_correct()
p

### Save annual drivers
ggsave(paste0(file.path(write_figures_path,"png/"), site_id, "_null_proportion.png"), p, width=6, height=4, dpi=600)
ggsave(paste0(file.path(write_figures_path,"svg/"), site_id, "_null_proportion.svg"), p, width=6, height=4)
ggsave(paste0(file.path(write_figures_path,"pdf/"), site_id, "_null_proportion.pdf"), p, width=6, height=4)




#################################################
### Apply the Null Model to True Observed Annual Flows
#################################################
### Create dataframes for annual flows
annual_rec <- data.frame(water_year=flow_obs$water_year, annual_flow=flow_obs$annual_mean)
### Only use one observation per year
annual_rec <- unique(annual_rec)

### Run Null Model
month_ts <- null_model(annual_rec, monthly_prop, first_month_wy=first_month_wy)

### Save Results
save_reconstruction(month_ts, site_id, site_name, output_name, data_name="observ_annual", method="Null Model using True Annual Mean Flow")

#################################################
### Specific Catch for the Logan reconstruction, which has 2 reconstruction models
#################################################

if (site_id == "10109001") {
	#################################################
	### Apply the Null Model to Reconstructed "Local Model"
	#################################################
	### Create dataframes for annual flows
	annual_rec <- data.frame(water_year=flow_recon$age_AD, annual_flow=flow_recon	$flow.rec.local.m3s)

	### Run Null Model
	month_ts <- null_model(annual_rec, monthly_prop, first_month_wy=first_month_wy)

	### Save Results
	save_reconstruction(month_ts, site_id, site_name, output_name, data_name="rec_local", method="Null Model using Local Reconstructed MAF")
	

	################################################
	### Apply the Null Model to Reconstructed "Regional Model"
	#################################################
	### Create dataframes for annual flows
	annual_rec <- data.frame(water_year=flow_recon$age_AD, annual_flow=flow_recon$flow.rec.region.m3s)

	### Run Null Model
	month_ts <- null_model(annual_rec, monthly_prop, first_month_wy=first_month_wy)

	### Save Results
	save_reconstruction(month_ts, site_id, site_name, output_name, data_name="rec_region", method="Null Model using Regional Reconstructed MAF")

} else {
	#################################################
	### Apply the Null Model to Generic Reconstructed Model
	#################################################
	### Create dataframes for annual flows
	annual_rec <- data.frame(water_year=flow_recon$age_AD, annual_flow=flow_recon$flow.rec.m3s)

	### Run Null Model
	month_ts <- null_model(annual_rec, monthly_prop, first_month_wy=first_month_wy)

	### Save Results
	save_reconstruction(month_ts, site_id, site_name, output_name, data_name="rec", method="Null Model using Reconstructed MAF")

}



}
