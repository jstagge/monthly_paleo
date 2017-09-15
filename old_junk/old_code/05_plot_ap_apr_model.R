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
require(ggplot2)

### Load project specific functions
file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
sapply(file.path(function_path, file.sources),source)

#################################################
### Read AP APR scenario matrix
#################################################
run_scenarios <- read.csv(file.path(write_output_base_path, "run_scenarios_ap_apr.csv"))


#################################################
### Plot annual norm coefficients against lag by looping through scenarios
#################################################

for (n in seq(1,dim(run_scenarios)[1])) {

### Read in scenario information
site_n <- as.character(run_scenarios$site[n])
model_name_n <- as.character(run_scenarios$model_name[n])

### Set predictor levels
predictor_levels <- c('annual_norm.-1', 'annual_norm.0',  'annual_norm.1')
predictor_labels <- c("Previous (-1) Water Year\nStd Normal", "Concurrent Water Year\nStd Normal",   "Next (+1) Water Year\nStd Normal")

### Set output folder for model fits and make sure folder exists
read_path <- file.path(file.path(write_output_base_path, "model_fit"), site_n)
### Read Coefficients
model_coef <- read.csv(file.path(read_path, paste0("coef/", model_name_n, "_coef.csv")), row.names = 1)

### Create figure
p <- norm_coef_plot(model_coef, predictor_levels=predictor_levels, predictor_labels=predictor_labels)
p <- p + scale_colour_brewer(name="", palette="Set2")
p <- p + geom_point(aes(shape=predictor))
p <- p + scale_shape_manual(name="", values=c(2, 15, 3))
p

### Set up output folders
write_output_path <- file.path(write_path, site_n)
dir.create(file.path(write_output_path,"png"), recursive=TRUE)
dir.create(file.path(write_output_path,"pdf"), recursive=TRUE)
dir.create(file.path(write_output_path,"svg"), recursive=TRUE)

### Save figure
ggsave(paste0(file.path(write_output_path,"png/"), model_name_n, "_norm_coef.png"), p, width=6, height=4, dpi=600)
ggsave(paste0(file.path(write_output_path,"svg/"), model_name_n, "_norm_coef.svg"), p, width=6, height=4)
ggsave(paste0(file.path(write_output_path,"pdf/"), model_name_n, "_norm_coef.pdf"), p, width=6, height=4)

### Save publication Fig 3a
if (model_name_n == "10109001_apr_rec_region_flow_lag"){
	ggsave(file.path(pub_path,"png/fig_3a.png"), p, width=6, height=4, dpi=600)
	ggsave(file.path(pub_path,"svg/fig_3a.svg"), p, width=6, height=4)
	ggsave(file.path(pub_path,"pdf/fig_3a.pdf"), p, width=6, height=4)
}

### Create publication Fig 3b
if (model_name_n == "10011500_apr_rec_flow_lag"){
	ggsave(file.path(pub_path,"png/fig_3b.png"), p, width=6, height=4, dpi=600)
	ggsave(file.path(pub_path,"svg/fig_3b.svg"), p, width=6, height=4)
	ggsave(file.path(pub_path,"pdf/fig_3b.pdf"), p, width=6, height=4)
}

}



#################################################
### Plot clim index coefficients against lag by looping through scenarios
#################################################

for (n in seq(1,dim(run_scenarios)[1])) {

### Read in scenario information
site_n <- as.character(run_scenarios$site[n])
model_name_n <- as.character(run_scenarios$model_name[n])

### Set predictor levels
predictor_levels <- c("ENSO", "ENSO_short")
predictor_labels <- c("ENSO (NADA)", "ENSO (Pacific Proxy)")

### Set output folder for model fits and make sure folder exists
read_path <- file.path(file.path(write_output_base_path, "model_fit"), site_n)
### Read Coefficients
model_coef <- read.csv(file.path(read_path, paste0("coef/", model_name_n, "_coef.csv")), row.names = 1)

### Create figure
p <- norm_coef_plot(model_coef, predictor_levels=predictor_levels, predictor_labels=predictor_labels)
p <- p + scale_colour_manual(name="", values=cb_pal(n=2, pal="wong", sort=FALSE))
p <- p + geom_point(aes(shape=predictor))
p <- p + scale_shape_manual(name="", values=c(2, 15, 3))
p

### Set up output folders
write_output_path <- file.path(write_path, site_n)
dir.create(file.path(write_output_path,"png"), recursive=TRUE)
dir.create(file.path(write_output_path,"pdf"), recursive=TRUE)
dir.create(file.path(write_output_path,"svg"), recursive=TRUE)

### Save figure
ggsave(paste0(file.path(write_output_path,"png/"), model_name_n, "_clim_coef.png"), p, width=6, height=4, dpi=600)
ggsave(paste0(file.path(write_output_path,"svg/"), model_name_n, "_clim_coef.svg"), p, width=6, height=4)
ggsave(paste0(file.path(write_output_path,"pdf/"), model_name_n, "_clim_coef.pdf"), p, width=6, height=4)

### Save publication Fig 5a
if (model_name_n == "10109001_apr_rec_region_clim_pca_impute"){
	ggsave(file.path(pub_path,"png/fig_5a.png"), p, width=6, height=4, dpi=600)
	ggsave(file.path(pub_path,"svg/fig_5a.svg"), p, width=6, height=4)
	ggsave(file.path(pub_path,"pdf/fig_5a.pdf"), p, width=6, height=4)
}

### Create publication Fig 5c
if (model_name_n == "10011500_apr_rec_clim_pca_impute"){
	ggsave(file.path(pub_path,"png/fig_5c.png"), p, width=6, height=4, dpi=600)
	ggsave(file.path(pub_path,"svg/fig_5c.svg"), p, width=6, height=4)
	ggsave(file.path(pub_path,"pdf/fig_5c.pdf"), p, width=6, height=4)
}

}



#################################################
### Plot PCs against lag by looping through scenarios
#################################################

for (n in seq(1,dim(run_scenarios)[1])) {

### Read in scenario information
site_n <- as.character(run_scenarios$site[n])
model_name_n <- as.character(run_scenarios$model_name[n])

### Set predictor levels
predictor_levels <- paste0("PC",seq(1,8))
predictor_labels <- predictor_levels

### Set output folder for model fits and make sure folder exists
read_path <- file.path(file.path(write_output_base_path, "model_fit"), site_n)
### Read Coefficients
model_coef <- read.csv(file.path(read_path, paste0("coef/", model_name_n, "_coef.csv")), row.names = 1)

### Create figure
p <- norm_coef_plot(model_coef, predictor_levels=predictor_levels, predictor_labels=predictor_labels)
p <- p + scale_colour_manual(name="", values=c("#CE7058" , "#D77FB4", "#7AC36A", "grey65", "#9E67AB", "#FAA75B", "#5A9BD4", "#F15A60"))
#p <- p + scale_colour_manual(name="", values=cb_pal(n=8, pal="d3"))
p

### Set up output folders
write_output_path <- file.path(write_path, site_n)
dir.create(file.path(write_output_path,"png"), recursive=TRUE)
dir.create(file.path(write_output_path,"pdf"), recursive=TRUE)
dir.create(file.path(write_output_path,"svg"), recursive=TRUE)

### Save figure
ggsave(paste0(file.path(write_output_path,"png/"), model_name_n, "_pc_coef.png"), p, width=6, height=4, dpi=600)
ggsave(paste0(file.path(write_output_path,"svg/"), model_name_n, "_pc_coef.svg"), p, width=6, height=4)
ggsave(paste0(file.path(write_output_path,"pdf/"), model_name_n, "_pc_coef.pdf"), p, width=6, height=4)

### Save publication Fig 5b
if (model_name_n == "10109001_apr_rec_region_clim_pca_impute"){
	ggsave(file.path(pub_path,"png/fig_5b.png"), p, width=6, height=4, dpi=600)
	ggsave(file.path(pub_path,"svg/fig_5b.svg"), p, width=6, height=4)
	ggsave(file.path(pub_path,"pdf/fig_5b.pdf"), p, width=6, height=4)
}

### Create publication Fig 5d
if (model_name_n == "10011500_apr_rec_clim_pca_impute"){
	ggsave(file.path(pub_path,"png/fig_5d.png"), p, width=6, height=4, dpi=600)
	ggsave(file.path(pub_path,"svg/fig_5d.svg"), p, width=6, height=4)
	ggsave(file.path(pub_path,"pdf/fig_5d.pdf"), p, width=6, height=4)
}

}




