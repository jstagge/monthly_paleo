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
data_path <- "../data"
output_path <- "../output"
global_path <- "./global_func"
function_path <- "./functions"

### Create output folders
write_output_base_path <- file.path(output_path, "pca_chronol")
write_figures_base_path <- file.path(output_path,"figures")

### Create output folders
dir.create(write_output_base_path)
dir.create(write_figures_base_path)

#################################################
### Create path for figures
#################################################
### Create folder for all figures
write_path <- file.path(write_figures_base_path,"pca_chronol")
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

require(fitdistrplus)
require(ggplot2)
require("maps")
require(ggmap)
require(gridExtra)
require(ggrepel)
require(dataRetrieval)
require(missMDA)

### Load project specific functions
file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
sapply(file.path(function_path, file.sources),source)

###########################################################################
## Set Initial Values
###########################################################################

### Number of PCs to retain
pc.n <- 8

### Set reconstruction site info
site_id_list <- c("10011500", "10109001")
site_name_list <- c( "Bear River near Utah-Wyo", "Logan Utah")



###########################################################################
## Set Reconstruction sites
###########################################################################
site_info <- readNWISsite(site_id_list)
site_info$short_name <- site_name_list


###########################################################################
###  Read in Tree Data
###########################################################################
### Read in tree chronologies
wadr_file_name <- "WADR-data-Stagge_update.csv"
wadr_data <- read.csv(file.path(data_path,paste0("paleo_wadr_tree_chron/",wadr_file_name)))

### Extract the site names and whether the reconstruction is std or res
wadr_name_type <- substr(names(wadr_data),4,6)
wadr_name_name <- substr(names(wadr_data),1,3)

### Separate the std or res chronologies
wadr_data_res <- wadr_data[,which(wadr_name_type == "res")]
names(wadr_data_res) <- wadr_name_name[which(wadr_name_type == "res")]
wadr_data_res <- data.frame(year=wadr_data$year, wadr_data_res)

wadr_data_std <- wadr_data[,which(wadr_name_type == "std")]
names(wadr_data_std) <- wadr_name_name[which(wadr_name_type == "std")]
wadr_data_std <- data.frame(year=wadr_data$year, wadr_data_std)


### Read in tree chronology site data
wadr_site_file_name <- "WADR-site-data-Stagge_update.csv"
wadr_site <- read.csv(file.path(data_path,paste0("paleo_wadr_tree_chron/",wadr_site_file_name)))

### Process the length of time series by counting all years with valid data
tree_length <- apply(wadr_data_std,2,function(x) sum(is.finite(x)))
### Merge the length data back with the chronologies
site_order <- match(unlist(wadr_site$ID), names(wadr_data_std))
wadr_site$chron_length <- tree_length[site_order]


##################################################
##  Download data for map tiles
##################################################
### Download background map tiles	
lon_lim <- c(min(wadr_site$Lon)-0.25,max(wadr_site$Lon)+0.25)
lat_lim <- c(min(wadr_site$Lat)-0.25,max(wadr_site$Lat)+0.25)
map_box <- as.matrix(data.frame(left=lon_lim[1]-5, bottom=lat_lim[1]-5, right=lon_lim[2]+5, top=lat_lim[2]+5))
map_big <- get_stamenmap(map_box, maptype="terrain", zoom = 8)

### Download state boundaries
states <- map_data("state")
colnames(states) <- c("lon", "lat", "group", "order", "region", "subregion")

################################################
### Prepare Data for PCA
#################################################
### Create a dataframe with no year (necesary for several calculations)
wadr_std_noyear <- wadr_data_std[,-1]
rownames(wadr_std_noyear) <- wadr_data_std[,1]

### Calculate the number of valid chronologies per year
wadr_chron_per_year <- apply(wadr_std_noyear, 1, function(x) sum(!is.na(x)))

### Create a dataframe to plot the number of tree-ring chronologies
plot_df <- data.frame(year=wadr_data_std$year, chron_per_year=wadr_chron_per_year)

### Plot time series of tree samples
p <- ggplot(subset(plot_df, chron_per_year>0), aes(x=year, ymin=0, ymax=chron_per_year))
p <- p + geom_ribbon(colour="black", fill="grey30", alpha=0.75, stat="stepribbon")
p <- p + geom_hline(yintercept=3.5, colour="red", linetype="longdash")
p <- p + theme_classic_correct_majgrid()
p <- p + coord_cartesian(xlim=c(700,2020), ylim=c(0,51), expand=FALSE)
p <- p + scale_y_continuous("Tree Ring Chronologies") + scale_x_continuous(name="Year")

### Set up output folders
dir.create(file.path(write_path,"png"), recursive=TRUE)
dir.create(file.path(write_path,"pdf"), recursive=TRUE)
dir.create(file.path(write_path,"svg"), recursive=TRUE)

### Save figure
plot_name <- "tree_chron_over_time"
ggsave(paste0(file.path(write_path,"png/"), plot_name, ".png"), p, width=6, height=4, dpi=600)
ggsave(paste0(file.path(write_path,"svg/"), plot_name, ".svg"), p, width=6, height=4)
ggsave(paste0(file.path(write_path,"pdf/"), plot_name, ".pdf"), p, width=6, height=4)


###########################################################################
## Test the number of sites and retain a few versions of complete dataframes
###########################################################################
		
yearbreak_df <- data.frame(n=seq(200,1100,10), FirstYear=NA, Sites=NA)

for (q in seq(1,dim(yearbreak_df)[1])) {
	n <- yearbreak_df$n[q]
	wadr_std_n <- wadr_std_noyear[,apply(wadr_std_noyear, 2, function(x) sum(!is.na(x)))>n]
	wadr_std_n <- wadr_std_n[complete.cases(wadr_std_n),]	
	yearbreak_df$FirstYear[q] <- as.numeric(rownames(wadr_std_n)[1])
	yearbreak_df$Sites[q] <- dim(wadr_std_n)[2]
}

ggplot(yearbreak_df, aes(x=FirstYear, y=Sites)) + geom_line() + theme_bw()
	
	
##################################################
##  Perform PCA on 600 year version
##################################################
### Extract a 600 year version
wadr_std_600 <- wadr_std_noyear[,apply(wadr_std_noyear, 2, function(x) sum(!is.na(x)))>600]
wadr_std_600 <- wadr_std_600[complete.cases(wadr_std_600),]

###  Perform PCA with scaling using SVD
pca_600 <- pca_calc(wadr_std_600)

### Recalculate time series
pca_x_600 <- pca_reconstruct(data=wadr_std_600, pca_fit = pca_600)

### Save PC scores
write.csv(pca_x_600,file.path(write_output_base_path, "pc_score_600.csv"))

### Create a table showing PCA diagnostics
scree_df <- data.frame(cbind(Component=seq(1,dim(pca_600$importance)[2]),t(pca_600$importance)))
scree_df$EigenVal <- scree_df$Standard.deviation^2

### Cut to first 20 and save to file
write.csv(scree_df[1:20,],file.path(write_output_base_path, "pca_var_exp_600.csv"), row.names=FALSE)

### Plot diagnostics for PCA 600
pca_plot_wrapper(data=scree_df, write_folder=write_path, write_file="pca_600")

### Plot Loadings for PCA 600
pca_loading_plot_wrapper(pca_loading=pca_600$loadings, pc.n=8, write_folder=write_path, write_file="pca_600" )


##################################################
##  Perform PCA on 400 year version
##################################################
### Extract a 400 year version
wadr_std_400 <- wadr_std_noyear[,apply(wadr_std_noyear, 2, function(x) sum(!is.na(x)))>380]
wadr_std_400 <- wadr_std_400[complete.cases(wadr_std_400),]

###  Perform PCA with scaling using SVD
pca_400 <- pca_calc(wadr_std_400)

### Recalculate time series
pca_x_400 <- pca_reconstruct(data=wadr_std_400, pca_fit = pca_400)

### Save PC scores
write.csv(pca_x_400,file.path(write_output_base_path, "pc_score_400.csv"))

### Create a table showing PCA diagnostics
scree_df <- data.frame(cbind(Component=seq(1,dim(pca_400$importance)[2]),t(pca_400$importance)))
scree_df$EigenVal <- scree_df$Standard.deviation^2

### Cut to first 20 and save to file
write.csv(scree_df[1:20,],file.path(write_output_base_path, "pca_var_exp_400.csv"), row.names=FALSE)

### Plot diagnostics for PCA 400
pca_plot_wrapper(data=scree_df, write_folder=write_path, write_file="pca_400")

### Plot Loadings for PCA 400
pca_loading_plot_wrapper(pca_loading=pca_400$loadings, pc.n=8, write_folder=write_path, write_file="pca_400" )




##################################################
##  Prepare for PCA with Imputation, plot chronologies
##################################################
### Cut only to the periods with > 3 samples
#wadr_std_impute <- wadr_std_noyear[wadr_chron_per_year>3,]
wadr_std_impute <- wadr_std_noyear[wadr_data_std$year>=1390,]


### Create a data frame showing chronology length and number of chronologies
chron_time_df <- data.frame(Year=c(as.numeric(rownames(pca_x_400)[1]),as.numeric(rownames(pca_x_400)[1]), as.numeric(rownames(pca_x_400)[dim(pca_x_400)[1]]), as.numeric(rownames(pca_x_400)[dim(pca_x_400)[1]])), Samples=c(0,dim(pca_x_400)[2],dim(pca_x_400)[2],0), Model="400 Years")
chron_time_df <- rbind(chron_time_df, data.frame(Year=c(as.numeric(rownames(pca_x_600)[1]),as.numeric(rownames(pca_x_600)[1]), as.numeric(rownames(pca_x_600)[dim(pca_x_600)[1]]), as.numeric(rownames(pca_x_600)[dim(pca_x_600)[1]])), Samples=c(0,dim(pca_x_600)[2],dim(pca_x_600)[2],0), Model="600 Years"))
chron_time_df <- rbind(chron_time_df, data.frame(Year=c(as.numeric(rownames(wadr_std_impute)[1]),as.numeric(rownames(wadr_std_impute)[1]), as.numeric(rownames(wadr_std_impute)[dim(wadr_std_impute)[1]]), as.numeric(rownames(wadr_std_impute)[dim(wadr_std_impute)[1]])), Samples=c(0,dim(wadr_std_impute)[2],dim(wadr_std_impute)[2],0), Model="900 Years (Imputed)"))
### Create a dataframe of available chronologies
Plot.df <- data.frame(Year=wadr_data_std$year, Samples=wadr_chron_per_year)
### Plot time series of tree samples
p <- ggplot(subset(Plot.df, Samples>0), aes(x=Year, ymin=0, ymax=Samples))
p <- p + geom_ribbon(colour="black", fill="grey30", alpha=0.75, stat="stepribbon")
p <- p + geom_polygon(data=chron_time_df, aes(x=Year, y=Samples, color=Model), fill=NA, size=0.75)
p <- p + scale_colour_brewer(name="Data Subset", type="qual", palette="Set1")
p <- p + theme_classic_new()
p <- p + theme(panel.grid.major =   element_line(colour = "grey90", size = 0.2))
p <- p + theme(legend.position="bottom")
p <- p + coord_cartesian(xlim=c(700,2023), ylim=c(0,51), expand=FALSE)
p <- p + scale_y_continuous("Tree Ring Chronologies") + scale_x_continuous(name="Year")
p

### Save figure
plot_name <- "tree_chron_over_time_3_alternatives"
ggsave(paste0(file.path(write_path,"png/"), plot_name, ".png"), p, width=6, height=4, dpi=600)
ggsave(paste0(file.path(write_path,"svg/"), plot_name, ".svg"), p, width=6, height=4)
ggsave(paste0(file.path(write_path,"pdf/"), plot_name, ".pdf"), p, width=6, height=4)

### Save Publication Figure 1
plot_name <- "fig_1"
ggsave(paste0(file.path(pub_path,"png/"), plot_name, ".png"), p, width=6, height=4, dpi=600)
ggsave(paste0(file.path(pub_path,"svg/"), plot_name, ".svg"), p, width=6, height=4)
ggsave(paste0(file.path(pub_path,"pdf/"), plot_name, ".pdf"), p, width=6, height=4)


##################################################
##  Run PCA with Imputation
##################################################

### Estimate number of PCs to retain using Kfold
nb <- estim_ncpPCA(wadr_std_impute,ncp.max=12, nbsim=100, pNA=0.1, method.cv="Kfold")

plot_df <- data.frame(PCs=names(nb$criterion), MSE=nb$criterion)
plot_df$PCs <-as.numeric(as.character(plot_df$PCs))
plot_df <- subset(plot_df, PCs > 0)
### Plot MSE of K-fold cross validated PCs
p <- ggplot(plot_df, aes(x=PCs, y=MSE))
p <- p + geom_line()
p <- p + geom_point()
p <- p + scale_y_continuous("Mean Squared Error")
p <- p + scale_x_continuous(name="Principal Component", breaks=seq(0,20,2))
p <- p + theme_classic_new()
p
### Save results
dir.create(file.path(file.path(write_path,"png"), "pca_impute"), recursive=TRUE)
dir.create(file.path(file.path(write_path,"svg"), "pca_impute"), recursive=TRUE)
dir.create(file.path(file.path(write_path,"pdf"), "pca_impute"), recursive=TRUE)

plot_name <- "pca_mse_k_fold"
ggsave(file.path(file.path(file.path(write_path,"png"), "pca_impute"), paste0(plot_name, ".png")), p, width=6, height=4, dpi=600)
ggsave(file.path(file.path(file.path(write_path,"svg"), "pca_impute"), paste0(plot_name, ".svg")), p, width=6, height=4)
ggsave(file.path(file.path(file.path(write_path,"pdf"), "pca_impute"), paste0(plot_name, ".pdf")), p, width=6, height=4)


##################################################
##  Perform PCA on impute year version
##################################################
### Impute missing values using 6 PCs
wadr_std_impute <- imputePCA(wadr_std_impute,ncp=9)
wadr_std_impute <- wadr_std_impute$completeObs

###  Save imputed values
write.csv(wadr_std_impute,file.path(write_output_base_path, "wadr_std_impute.csv"))

###  Perform PCA with scaling using SVD
pca_impute <- pca_calc(wadr_std_impute)

### Recalculate time series
pca_x_impute <- pca_reconstruct(data=wadr_std_impute, pca_fit = pca_impute)

### Save PC scores
write.csv(pca_x_impute,file.path(write_output_base_path, "pc_score_impute.csv"))

### Create a table showing PCA diagnostics
scree_df <- data.frame(cbind(Component=seq(1,dim(pca_impute$importance)[2]),t(pca_impute$importance)))
scree_df$EigenVal <- scree_df$Standard.deviation^2

### Cut to first 20 and save to file
write.csv(scree_df[1:20,],file.path(write_output_base_path, "pca_var_exp_impute.csv"), row.names=FALSE)

### Plot diagnostics for PCA Imputed
pca_plot_wrapper(data=scree_df, write_folder=write_path, write_file="pca_impute")

### Plot Loadings for PCA Imputed
pca_loading_plot_wrapper(pca_loading=pca_impute$loadings, pc.n=12, write_folder=write_path, write_file="pca_impute" )




